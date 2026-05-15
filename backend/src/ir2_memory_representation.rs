use std::{
    fmt::Debug,
    fmt::Display,
    ops::{Add, AddAssign, Sub, SubAssign},
};

// this represents an actual location in the stack
// size is implicitly 1 byte
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Hash, Eq)]
pub struct PhysicalLocation(usize); // offset from stack frame adress

macro_rules! impl_arithmetic_ops {
    ($type:ty, unsigned) => {
        impl Add<$type> for PhysicalLocation {
            type Output = Self;

            fn add(self, rhs: $type) -> Self::Output {
                PhysicalLocation(self.0 + rhs as usize)
            }
        }

        impl Sub<$type> for PhysicalLocation {
            type Output = Self;

            fn sub(self, rhs: $type) -> Self::Output {
                PhysicalLocation(self.0 - rhs as usize)
            }
        }

        impl AddAssign<$type> for PhysicalLocation {
            fn add_assign(&mut self, rhs: $type) {
                self.0 += rhs as usize;
            }
        }

        impl SubAssign<$type> for PhysicalLocation {
            fn sub_assign(&mut self, rhs: $type) {
                self.0 -= rhs as usize;
            }
        }
    };
    ($type:ty, signed) => {
        impl Add<$type> for PhysicalLocation {
            type Output = Self;

            fn add(self, rhs: $type) -> Self::Output {
                PhysicalLocation(self.0.checked_add_signed(rhs as isize).unwrap())
            }
        }

        impl Sub<$type> for PhysicalLocation {
            type Output = Self;

            fn sub(self, rhs: $type) -> Self::Output {
                PhysicalLocation(self.0.checked_sub_signed(rhs as isize).unwrap())
            }
        }

        impl AddAssign<$type> for PhysicalLocation {
            fn add_assign(&mut self, rhs: $type) {
                *self = *self + rhs;
            }
        }

        impl SubAssign<$type> for PhysicalLocation {
            fn sub_assign(&mut self, rhs: $type) {
                *self = *self - rhs;
            }
        }
    };
}

// Unsigned integer types
impl_arithmetic_ops!(u8, unsigned);
impl_arithmetic_ops!(u16, unsigned);
impl_arithmetic_ops!(u32, unsigned);
impl_arithmetic_ops!(u64, unsigned);
impl_arithmetic_ops!(u128, unsigned);
impl_arithmetic_ops!(usize, unsigned);

// Signed integer types
impl_arithmetic_ops!(i8, signed);
impl_arithmetic_ops!(i16, signed);
impl_arithmetic_ops!(i32, signed);
impl_arithmetic_ops!(i64, signed);
impl_arithmetic_ops!(i128, signed);
impl_arithmetic_ops!(isize, signed);

impl Display for PhysicalLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PhysicalLocation {
    pub fn new(offset: usize) -> Self {
        PhysicalLocation(offset)
    }

    pub fn get_difference(from: PhysicalLocation, to: PhysicalLocation) -> isize {
        from.0 as isize - to.0 as isize
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct PhysicalSlot {
    location: PhysicalLocation,
    size: usize,
}

// this is for adding an offset to slot, just adds to the start and keeps the size the same
impl Add<PhysicalLocation> for PhysicalSlot {
    type Output = Self;

    fn add(self, rhs: PhysicalLocation) -> Self::Output {
        PhysicalSlot {
            location: self.location + rhs.0,
            size: self.size,
        }
    }
}

impl PhysicalSlot {
    pub fn from_start_end_inclusive(start: PhysicalLocation, end: PhysicalLocation) -> Self {
        assert!(
            end.0 >= start.0,
            "end must be greater than or equal to start"
        );
        PhysicalSlot {
            location: start,
            size: end.0 - start.0 + 1,
        }
    }

    pub fn from_start_size(start: PhysicalLocation, size: usize) -> Self {
        PhysicalSlot {
            location: start,
            size,
        }
    }

    pub fn intersects(&self, other: &PhysicalSlot) -> bool {
        let self_end = PhysicalLocation(self.location.0 + self.size - 1);
        let other_end = PhysicalLocation(other.location.0 + other.size - 1);
        !(self_end < other.location || other_end < self.location)
    }

    // this should only be used for dereferencing for array variables
    pub fn get_start(&self) -> PhysicalLocation {
        self.location
    }

    //this is the last location inside of the slot
    pub fn get_end(&self) -> PhysicalLocation {
        assert!(self.size > 0, "cannot get end of empty slot");
        PhysicalLocation(self.location.0 + self.size - 1)
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_all_locations(&self) -> Vec<PhysicalLocation> {
        (0..self.size)
            .map(|i| PhysicalLocation(self.location.0 + i))
            .collect()
    }
}

impl Display for PhysicalSlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} size {}]", self.get_start(), self.size)
    }
}

pub struct PhysicalLocationAllocator<V> {
    open_spaces: Vec<SpaceInterval>,
    registers_in_use: Vec<(PhysicalSlot, V)>,
}

enum SpaceInterval {
    // this is inclusive of both start and end
    Closed { span: PhysicalSlot },
    Open { start: PhysicalLocation },
}

impl<V> PhysicalLocationAllocator<V>
where
    V: Debug + Clone,
{
    pub fn new() -> Self {
        PhysicalLocationAllocator {
            open_spaces: vec![SpaceInterval::Open {
                start: PhysicalLocation(0),
            }],
            registers_in_use: Vec::new(),
        }
    }

    pub fn allocate(&mut self, size: usize, tag: V) -> PhysicalSlot {
        //first find closed intervals that could fit this register
        let slot = self.find_open_space(size);

        self.close_free_space(slot);
        self.registers_in_use.push((slot, tag));
        slot
    }

    fn find_open_space(&self, size: usize) -> PhysicalSlot {
        for open_space in &self.open_spaces {
            match open_space {
                SpaceInterval::Closed { span } => {
                    if span.size >= size {
                        PhysicalSlot::from_start_size(span.location, size);
                    }
                }
                _ => {}
            }
        }
        //find an open interval
        for open_space in &self.open_spaces {
            match open_space {
                SpaceInterval::Open { start } => {
                    return PhysicalSlot::from_start_size(*start, size);
                }
                _ => {}
            }
        }
        panic!("No open space found in infinite tape???");
    }

    fn check_space_available(&self, slot: PhysicalSlot) -> bool {
        if slot.get_size() == 0 {
            return true;
        }

        let end = slot.get_end();
        let start = slot.get_start();

        for open_space in &self.open_spaces {
            match open_space {
                SpaceInterval::Closed {
                    span: interval_slot,
                } => {
                    let interval_start = interval_slot.get_start();
                    let interval_end = interval_slot.get_end();
                    if interval_start <= start && interval_end >= end {
                        return true;
                    }
                }
                SpaceInterval::Open {
                    start: interval_start,
                } => {
                    if *interval_start <= start {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn close_free_space(&mut self, slot: PhysicalSlot) {
        //this is used to mark a space as closed after we have allocated a register there
        //first find the containing interval in self.open_spaces

        if slot.get_size() == 0 {
            return;
        }

        let new_start = slot.get_start();
        let new_end = slot.get_end();

        self.open_spaces = std::mem::take(&mut self.open_spaces)
            .into_iter()
            .flat_map(|open_space| {
                match open_space {
                    SpaceInterval::Closed { span } => {
                        let interval_start = span.get_start();
                        let interval_end = span.get_end();
                        if interval_start <= new_start && interval_end >= new_end {
                            //this is the containing interval
                            //4 cases:
                            if interval_start == new_start && interval_end > new_end {
                                //taking up portion of the start of the interval
                                vec![SpaceInterval::Closed {
                                    span: PhysicalSlot::from_start_end_inclusive(
                                        new_end + 1,
                                        interval_end,
                                    ),
                                }]
                            } else if interval_start < new_start && interval_end == new_end {
                                //taking up portion of the end of the interval
                                vec![SpaceInterval::Closed {
                                    span: PhysicalSlot::from_start_end_inclusive(
                                        interval_start,
                                        new_start - 1,
                                    ),
                                }]
                            } else if interval_start < new_start && interval_end > new_end {
                                //taking up a portion in the middle of the interval
                                vec![
                                    SpaceInterval::Closed {
                                        span: PhysicalSlot::from_start_end_inclusive(
                                            interval_start,
                                            new_start - 1,
                                        ),
                                    },
                                    SpaceInterval::Closed {
                                        span: PhysicalSlot::from_start_end_inclusive(
                                            new_end + 1,
                                            interval_end,
                                        ),
                                    },
                                ]
                            } else {
                                //taking up the entire interval
                                vec![]
                            }
                        } else {
                            vec![open_space]
                        }
                    }
                    SpaceInterval::Open {
                        start: interval_start,
                    } => {
                        if new_start >= interval_start {
                            //2 cases:
                            if new_start == interval_start {
                                //taking up the start of the interval, just move the start forward
                                vec![SpaceInterval::Open { start: new_end + 1 }]
                            } else {
                                //taking up a portion off the start of the interval, move the start forward and add closed interval for the gap

                                vec![
                                    SpaceInterval::Closed {
                                        span: PhysicalSlot::from_start_end_inclusive(
                                            interval_start,
                                            new_start - 1,
                                        ),
                                    },
                                    SpaceInterval::Open { start: new_end + 1 },
                                ]
                            }
                        } else {
                            vec![open_space]
                        }
                    }
                }
            })
            .collect();
    }

    pub fn register_allocation(&mut self, slot: PhysicalSlot, tag: V) {
        assert!(
            self.check_space_available(slot),
            "attempting to register allocation for space that is not available: {:?} ({} bytes)",
            tag,
            slot.get_size()
        );

        self.close_free_space(slot);

        self.registers_in_use.push((slot, tag));
    }

    //this finds the start of the open interval of free space
    pub fn free_space_start(&self) -> PhysicalLocation {
        for open_space in &self.open_spaces {
            match open_space {
                SpaceInterval::Closed { .. } => {}
                SpaceInterval::Open { start } => {
                    return *start;
                }
            }
        }
        panic!("No open space found in infinite tape???");
    }

    pub fn get_all_allocations(&self) -> Vec<(PhysicalSlot, V)> {
        self.registers_in_use.clone()
    }
}

pub struct Tape {
    pub data: Vec<u8>,
}

impl Tape {
    pub fn new() -> Self {
        Tape { data: vec![] }
    }
}

pub struct TapeView<'a> {
    view: &'a mut Tape,
    offset: usize,
}

impl<'a> TapeView<'a> {
    pub fn new(tape: &'a mut Tape) -> Self {
        TapeView {
            view: tape,
            offset: 0,
        }
    }

    pub fn read(&mut self, index: PhysicalLocation) -> u8 {
        self.ensure_capacity(index.0);
        self.view.data[self.offset + index.0]
    }

    pub fn read_slot_single(&mut self, index: PhysicalSlot) -> u8 {
        assert_eq!(
            index.get_size(),
            1,
            "can only read single byte slots with read_slot_single"
        );
        self.read(index.get_start())
    }

    pub fn write(&mut self, index: PhysicalLocation, value: u8) {
        self.ensure_capacity(index.0);
        self.view.data[self.offset + index.0] = value;
    }

    fn ensure_capacity(&mut self, index: usize) {
        let required_length = self.offset + index + 1;
        if self.view.data.len() < required_length {
            self.view.data.resize(required_length, 0);
        }
    }

    pub fn get_offset_view<'b>(&'b mut self, offset: PhysicalLocation) -> TapeView<'b> {
        TapeView {
            view: self.view,
            offset: self.offset + offset.0,
        }
    }
}

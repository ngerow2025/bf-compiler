use std::hash::Hash;
use std::{cell::UnsafeCell, collections::HashMap};

use thiserror::Error;

pub struct BumpAllocator<'alloc, T> {
    memory: UnsafeCell<Box<[u8]>>,
    offset: usize,
    capacity: usize,
    allocated_memory: HashMap<T, &'alloc mut [u8]>,
}

impl<'alloc, T> BumpAllocator<'alloc, T>
where
    T: Hash + Eq + Copy,
{
    pub fn new(size: usize) -> Self {
        Self {
            memory: UnsafeCell::new(vec![0; size].into_boxed_slice()),
            offset: 0,
            capacity: size,
            allocated_memory: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, size: usize, key: T) -> Result<(), BumpAllocatorError> {
        if self.offset + size > self.capacity {
            return Err(BumpAllocatorError::OutOfMemoryCapacity);
        }
        let start = self.offset;
        self.offset += size;
        let memory = unsafe {
            let ptr = (&raw mut **self.memory.get()).cast::<u8>().add(start);
            std::slice::from_raw_parts_mut(ptr, size)
        };
        self.allocated_memory.insert(key, memory);
        Ok(())
    }

    fn get_mem_info(&self, key: T) -> Result<(*const u8, usize), BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        Ok((mem.as_ptr(), mem.len()))
    }

    fn get_mem_info_mut(&mut self, key: T) -> Result<(*mut u8, usize), BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get_mut(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        Ok((mem.as_mut_ptr(), mem.len()))
    }

    pub fn copy(&mut self, from: T, to: T) -> Result<(), BumpAllocatorError> {
        let (from_mem_ptr, from_mem_len) = self.get_mem_info(from)?;
        let (to_mem_ptr, to_mem_len) = self.get_mem_info_mut(to)?;
        if from_mem_len != to_mem_len {
            return Err(BumpAllocatorError::MemorySizeMismatch);
        }
        unsafe {
            std::ptr::copy_nonoverlapping(from_mem_ptr, to_mem_ptr, from_mem_len);
        }
        Ok(())
    }

    pub fn get_memory_contents(&self, key: T) -> Result<Vec<u8>, BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        Ok(mem.to_vec())
    }

    pub fn get_memory_size(&self, key: T) -> Result<usize, BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        Ok(mem.len())
    }

    pub fn set_memory_contents(
        &mut self,
        key: T,
        contents: &[u8],
    ) -> Result<(), BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get_mut(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        if mem.len() != contents.len() {
            return Err(BumpAllocatorError::MemorySizeMismatch);
        }
        mem.copy_from_slice(contents);
        Ok(())
    }

    pub fn read_offset(&self, key: T, offset: usize) -> Result<u8, BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        if offset >= mem.len() {
            return Err(BumpAllocatorError::MemorySizeMismatch);
        }
        Ok(mem[offset])
    }

    pub fn write_offset(
        &mut self,
        key: T,
        offset: usize,
        value: u8,
    ) -> Result<(), BumpAllocatorError> {
        let mem = self
            .allocated_memory
            .get_mut(&key)
            .ok_or(BumpAllocatorError::KeyNotFound)?;
        if offset >= mem.len() {
            return Err(BumpAllocatorError::MemorySizeMismatch);
        }
        mem[offset] = value;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
pub enum BumpAllocatorError {
    #[error("Out of memory capacity in bump allocator")]
    OutOfMemoryCapacity,
    #[error("Key not found in allocated memory")]
    KeyNotFound,
    #[error("Source and target memory sizes do not match for copy operation")]
    MemorySizeMismatch,
}

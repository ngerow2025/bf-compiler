use crossterm::{
    event::{Event, KeyCode, KeyEventKind, read},
    terminal::{disable_raw_mode, enable_raw_mode},
};

pub struct RawModeGuard;

impl RawModeGuard {
    pub fn new() -> Self {
        enable_raw_mode().expect("failed to enable raw mode");
        Self
    }
}

impl Default for RawModeGuard {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
    }
}

pub fn read_single_byte() -> u8 {
    loop {
        let event = read().expect("failed to read terminal input event");
        if let Event::Key(key_event) = event {
            if key_event.kind != KeyEventKind::Press {
                continue;
            }

            match key_event.code {
                KeyCode::Char(c) => return c as u8,
                KeyCode::Enter => return b'\n',
                KeyCode::Tab => return b'\t',
                KeyCode::Backspace => return 8,
                _ => continue,
            }
        }
    }
}

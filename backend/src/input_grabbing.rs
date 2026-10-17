use std::{panic, sync::Arc};

use crossterm::{
    event::{Event, KeyCode, KeyEventKind, read},
    terminal::{disable_raw_mode, enable_raw_mode},
};

type PanicHook = Box<dyn Fn(&panic::PanicHookInfo<'_>) + Send + Sync + 'static>;

pub struct RawModeGuard {
    previous_hook: Option<PanicHook>,
}

impl RawModeGuard {
    #[must_use]
    pub fn new() -> Self {
        enable_raw_mode().expect("failed to enable raw mode");

        let previous_hook: PanicHook = panic::take_hook();
        let prev: Arc<PanicHook> = Arc::new(previous_hook);
        let prev_clone = Arc::clone(&prev);

        panic::set_hook(Box::new(move |panic_info| {
            let _ = disable_raw_mode();
            prev_clone(panic_info);
        }));

        let prev_hook = Arc::into_inner(prev);

        Self {
            previous_hook: prev_hook,
        }
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
        if let Some(prev_hook) = self.previous_hook.take() {
            panic::set_hook(prev_hook);
        }
    }
}

#[must_use]
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
                _ => {}
            }
        }
    }
}

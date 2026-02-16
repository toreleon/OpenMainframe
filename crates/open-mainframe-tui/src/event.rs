//! Pluggable event sources for the TUI session.
//!
//! Abstracts terminal event input so that `Session::wait_for_input` can be
//! driven by real keyboard events (via crossterm) in production, or by
//! pre-queued events in tests.

use std::collections::VecDeque;

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};

use crate::error::SessionError;

/// Abstraction over terminal event input for testability.
///
/// Production code uses [`CrosstermEventSource`] which blocks on real
/// keyboard input.  Tests use [`MockEventSource`] which pops from a
/// pre-filled queue.
pub trait EventSource {
    /// Read the next terminal event.  Blocks in production; returns queued
    /// events (or `Err(Interrupted)` when empty) in test mode.
    fn read_event(&mut self) -> Result<Event, SessionError>;
}

/// Real crossterm event source for production use.
pub struct CrosstermEventSource;

impl EventSource for CrosstermEventSource {
    fn read_event(&mut self) -> Result<Event, SessionError> {
        crossterm::event::read().map_err(SessionError::TerminalInit)
    }
}

/// Mock event source that pops from a pre-filled queue of events.
///
/// When the queue is exhausted, `read_event` returns
/// `Err(SessionError::Interrupted)` to break out of the input loop,
/// preventing tests from hanging.
pub struct MockEventSource {
    events: VecDeque<Event>,
}

impl MockEventSource {
    /// Create an empty mock event source.
    pub fn new() -> Self {
        Self {
            events: VecDeque::new(),
        }
    }

    /// Queue a key press event with no modifiers.
    pub fn push_key(&mut self, code: KeyCode) {
        self.events.push_back(Event::Key(KeyEvent {
            code,
            modifiers: KeyModifiers::NONE,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        }));
    }

    /// Queue a key press event with the given modifiers.
    pub fn push_key_mod(&mut self, code: KeyCode, modifiers: KeyModifiers) {
        self.events.push_back(Event::Key(KeyEvent {
            code,
            modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        }));
    }

    /// Queue key press events for each character in the string.
    pub fn push_text(&mut self, text: &str) {
        for ch in text.chars() {
            self.push_key(KeyCode::Char(ch));
        }
    }

    /// Queue a raw crossterm event.
    pub fn push_event(&mut self, event: Event) {
        self.events.push_back(event);
    }

    /// Return the number of remaining events in the queue.
    pub fn remaining(&self) -> usize {
        self.events.len()
    }
}

impl Default for MockEventSource {
    fn default() -> Self {
        Self::new()
    }
}

impl EventSource for MockEventSource {
    fn read_event(&mut self) -> Result<Event, SessionError> {
        self.events
            .pop_front()
            .ok_or(SessionError::Interrupted)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_event_source_push_key() {
        let mut src = MockEventSource::new();
        src.push_key(KeyCode::Enter);
        assert_eq!(src.remaining(), 1);

        let event = src.read_event().unwrap();
        match event {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Enter),
            _ => panic!("expected Key event"),
        }
        assert_eq!(src.remaining(), 0);
    }

    #[test]
    fn test_mock_event_source_push_text() {
        let mut src = MockEventSource::new();
        src.push_text("AB");
        assert_eq!(src.remaining(), 2);

        match src.read_event().unwrap() {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Char('A')),
            _ => panic!("expected Key event"),
        }
        match src.read_event().unwrap() {
            Event::Key(ke) => assert_eq!(ke.code, KeyCode::Char('B')),
            _ => panic!("expected Key event"),
        }
    }

    #[test]
    fn test_mock_event_source_returns_interrupted_when_empty() {
        let mut src = MockEventSource::new();
        let result = src.read_event();
        assert!(result.is_err());
    }

    #[test]
    fn test_mock_event_source_push_key_mod() {
        let mut src = MockEventSource::new();
        src.push_key_mod(KeyCode::Char('c'), KeyModifiers::CONTROL);
        match src.read_event().unwrap() {
            Event::Key(ke) => {
                assert_eq!(ke.code, KeyCode::Char('c'));
                assert!(ke.modifiers.contains(KeyModifiers::CONTROL));
            }
            _ => panic!("expected Key event"),
        }
    }
}

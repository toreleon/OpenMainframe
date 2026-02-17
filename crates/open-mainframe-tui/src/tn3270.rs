//! TN3270E protocol support for connecting to real z/OS systems.
//!
//! Implements TN3270E negotiation, 3270 data stream parsing (outbound),
//! and inbound data stream generation for terminal interaction.

use std::io::{self, Read, Write};
use std::net::TcpStream;

// --- TN3270E Telnet negotiation constants ---

/// Telnet command bytes.
pub mod telnet {
    pub const IAC: u8 = 0xFF;
    pub const WILL: u8 = 0xFB;
    pub const WONT: u8 = 0xFC;
    pub const DO: u8 = 0xFD;
    pub const DONT: u8 = 0xFE;
    pub const SB: u8 = 0xFA;
    pub const SE: u8 = 0xF0;
    pub const EOR: u8 = 0xEF;

    /// Telnet option: TN3270E
    pub const TN3270E: u8 = 0x28;
    /// Telnet option: Binary
    pub const BINARY: u8 = 0x00;
    /// Telnet option: End of Record
    pub const END_OF_RECORD: u8 = 0x19;
}

// --- 3270 Command Codes ---

/// 3270 write commands.
pub mod command {
    pub const WRITE: u8 = 0xF1;
    pub const ERASE_WRITE: u8 = 0xF5;
    pub const ERASE_WRITE_ALTERNATE: u8 = 0x7E;
    pub const WRITE_STRUCTURED_FIELD: u8 = 0xF3;
    pub const READ_BUFFER: u8 = 0xF2;
    pub const READ_MODIFIED: u8 = 0xF6;
    pub const READ_MODIFIED_ALL: u8 = 0x6E;
}

/// 3270 order codes.
pub mod order {
    pub const SBA: u8 = 0x11;  // Set Buffer Address
    pub const SF: u8 = 0x1D;   // Start Field
    pub const SFE: u8 = 0x29;  // Start Field Extended
    pub const SA: u8 = 0x28;   // Set Attribute
    pub const IC: u8 = 0x13;   // Insert Cursor
    pub const PT: u8 = 0x05;   // Program Tab
    pub const RA: u8 = 0x3C;   // Repeat to Address
    pub const EUA: u8 = 0x12;  // Erase Unprotected to Address
    pub const MF: u8 = 0x2C;   // Modify Field
}

/// WCC (Write Control Character) flags.
pub mod wcc {
    pub const RESET_MDT: u8 = 0x04;
    pub const KEYBOARD_RESTORE: u8 = 0x02;
    pub const ALARM: u8 = 0x04;
}

/// AID (Attention Identifier) codes.
pub mod aid {
    pub const ENTER: u8 = 0x7D;
    pub const PF1: u8 = 0xF1;
    pub const PF2: u8 = 0xF2;
    pub const PF3: u8 = 0xF3;
    pub const PF4: u8 = 0xF4;
    pub const PF5: u8 = 0xF5;
    pub const PF6: u8 = 0xF6;
    pub const PF7: u8 = 0xF7;
    pub const PF8: u8 = 0xF8;
    pub const PF9: u8 = 0xF9;
    pub const PF10: u8 = 0x7A;
    pub const PF11: u8 = 0x7B;
    pub const PF12: u8 = 0x7C;
    pub const PA1: u8 = 0x6C;
    pub const PA2: u8 = 0x6E;
    pub const PA3: u8 = 0x6B;
    pub const CLEAR: u8 = 0x6D;
}

// --- Buffer Address Encoding/Decoding ---

/// Encode a 14-bit buffer address to 2-byte 3270 format.
pub fn encode_buffer_address(addr: u16) -> [u8; 2] {
    let high = (addr >> 6) as u8;
    let low = (addr & 0x3F) as u8;
    [encode_6bit(high), encode_6bit(low)]
}

/// Decode a 2-byte 3270 buffer address to 14-bit value.
pub fn decode_buffer_address(b1: u8, b2: u8) -> u16 {
    let high = decode_6bit(b1) as u16;
    let low = decode_6bit(b2) as u16;
    (high << 6) | low
}

/// Encode a 6-bit value for buffer addressing.
fn encode_6bit(val: u8) -> u8 {
    // 3270 uses a specific 6-bit encoding table
    const TABLE: [u8; 64] = [
        0x40, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
        0xC8, 0xC9, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
        0x50, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
        0xD8, 0xD9, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
        0x60, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
        0xE8, 0xE9, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
        0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
        0xF8, 0xF9, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
    ];
    TABLE[(val & 0x3F) as usize]
}

/// Decode a 6-bit encoded value.
fn decode_6bit(byte: u8) -> u8 {
    // Build reverse lookup
    match byte {
        0x40 => 0, 0xC1 => 1, 0xC2 => 2, 0xC3 => 3, 0xC4 => 4, 0xC5 => 5,
        0xC6 => 6, 0xC7 => 7, 0xC8 => 8, 0xC9 => 9, 0x4A => 10, 0x4B => 11,
        0x4C => 12, 0x4D => 13, 0x4E => 14, 0x4F => 15, 0x50 => 16, 0xD1 => 17,
        0xD2 => 18, 0xD3 => 19, 0xD4 => 20, 0xD5 => 21, 0xD6 => 22, 0xD7 => 23,
        0xD8 => 24, 0xD9 => 25, 0x5A => 26, 0x5B => 27, 0x5C => 28, 0x5D => 29,
        0x5E => 30, 0x5F => 31, 0x60 => 32, 0xE1 => 33, 0xE2 => 34, 0xE3 => 35,
        0xE4 => 36, 0xE5 => 37, 0xE6 => 38, 0xE7 => 39, 0xE8 => 40, 0xE9 => 41,
        0x6A => 42, 0x6B => 43, 0x6C => 44, 0x6D => 45, 0x6E => 46, 0x6F => 47,
        0xF0 => 48, 0xF1 => 49, 0xF2 => 50, 0xF3 => 51, 0xF4 => 52, 0xF5 => 53,
        0xF6 => 54, 0xF7 => 55, 0xF8 => 56, 0xF9 => 57, 0x7A => 58, 0x7B => 59,
        0x7C => 60, 0x7D => 61, 0x7E => 62, 0x7F => 63,
        _ => 0,
    }
}

// --- TN3270 Connection ---

/// TN3270E connection state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tn3270State {
    /// Initial connection, negotiating.
    Negotiating,
    /// Connected in TN3270E mode.
    Connected,
    /// Connected in basic TN3270 mode (fallback).
    ConnectedBasic,
    /// Disconnected.
    Disconnected,
}

/// TN3270 connection configuration.
#[derive(Debug, Clone)]
pub struct Tn3270Config {
    /// Host address.
    pub host: String,
    /// Port number.
    pub port: u16,
    /// LU name to request (optional).
    pub lu_name: Option<String>,
    /// Device type to negotiate.
    pub device_type: String,
}

impl Default for Tn3270Config {
    fn default() -> Self {
        Self {
            host: "localhost".to_string(),
            port: 23,
            lu_name: None,
            device_type: "IBM-3278-2-E".to_string(),
        }
    }
}

/// A parsed 3270 data stream record from the host.
#[derive(Debug, Clone)]
pub struct DataStreamRecord {
    /// The command (Write, Erase/Write, EWA, WSF).
    pub command: u8,
    /// WCC byte (for Write commands).
    pub wcc: u8,
    /// Parsed orders and data.
    pub orders: Vec<DataStreamOrder>,
}

/// A single order or data segment in a 3270 data stream.
#[derive(Debug, Clone)]
pub enum DataStreamOrder {
    /// Set Buffer Address to (row, col).
    SetBufferAddress { row: usize, col: usize },
    /// Start Field with attribute byte.
    StartField { attribute: u8 },
    /// Start Field Extended with type/value pairs.
    StartFieldExtended { pairs: Vec<(u8, u8)> },
    /// Insert Cursor at current position.
    InsertCursor,
    /// Program Tab.
    ProgramTab,
    /// Repeat to Address (fill character to target position).
    RepeatToAddress { row: usize, col: usize, character: u8 },
    /// Erase Unprotected to Address.
    EraseUnprotectedToAddress { row: usize, col: usize },
    /// Raw data bytes to display.
    Data(Vec<u8>),
}

impl DataStreamRecord {
    /// Parse a 3270 outbound data stream.
    pub fn parse(data: &[u8], cols: usize) -> Option<Self> {
        if data.is_empty() {
            return None;
        }

        let cmd = data[0];
        let wcc = if data.len() > 1 { data[1] } else { 0 };
        let stream = if data.len() > 2 { &data[2..] } else { &[] };

        let mut orders = Vec::new();
        let mut i = 0;

        while i < stream.len() {
            match stream[i] {
                order::SBA => {
                    if i + 2 < stream.len() {
                        let addr = decode_buffer_address(stream[i + 1], stream[i + 2]);
                        let row = addr as usize / cols + 1;
                        let col = addr as usize % cols + 1;
                        orders.push(DataStreamOrder::SetBufferAddress { row, col });
                        i += 3;
                    } else {
                        break;
                    }
                }
                order::SF => {
                    if i + 1 < stream.len() {
                        orders.push(DataStreamOrder::StartField { attribute: stream[i + 1] });
                        i += 2;
                    } else {
                        break;
                    }
                }
                order::SFE => {
                    if i + 1 < stream.len() {
                        let count = stream[i + 1] as usize;
                        let mut pairs = Vec::new();
                        let mut j = i + 2;
                        for _ in 0..count {
                            if j + 1 < stream.len() {
                                pairs.push((stream[j], stream[j + 1]));
                                j += 2;
                            }
                        }
                        orders.push(DataStreamOrder::StartFieldExtended { pairs });
                        i = j;
                    } else {
                        break;
                    }
                }
                order::IC => {
                    orders.push(DataStreamOrder::InsertCursor);
                    i += 1;
                }
                order::PT => {
                    orders.push(DataStreamOrder::ProgramTab);
                    i += 1;
                }
                order::RA => {
                    if i + 3 < stream.len() {
                        let addr = decode_buffer_address(stream[i + 1], stream[i + 2]);
                        let row = addr as usize / cols + 1;
                        let col = addr as usize % cols + 1;
                        let character = stream[i + 3];
                        orders.push(DataStreamOrder::RepeatToAddress { row, col, character });
                        i += 4;
                    } else {
                        break;
                    }
                }
                order::EUA => {
                    if i + 2 < stream.len() {
                        let addr = decode_buffer_address(stream[i + 1], stream[i + 2]);
                        let row = addr as usize / cols + 1;
                        let col = addr as usize % cols + 1;
                        orders.push(DataStreamOrder::EraseUnprotectedToAddress { row, col });
                        i += 3;
                    } else {
                        break;
                    }
                }
                _ => {
                    // Data bytes — collect until next order
                    let start = i;
                    while i < stream.len() && !is_order(stream[i]) {
                        i += 1;
                    }
                    orders.push(DataStreamOrder::Data(stream[start..i].to_vec()));
                }
            }
        }

        Some(DataStreamRecord {
            command: cmd,
            wcc,
            orders,
        })
    }
}

/// Check if a byte is a 3270 order code.
fn is_order(byte: u8) -> bool {
    matches!(
        byte,
        order::SBA | order::SF | order::SFE | order::SA | order::IC |
        order::PT | order::RA | order::EUA | order::MF
    )
}

/// Generate an inbound 3270 data stream for a Read Modified response.
///
/// This creates the response sent to the host when Enter or a PF key is pressed:
/// AID + cursor address + [SBA + field data]...
pub fn generate_inbound_stream(
    aid_code: u8,
    cursor_row: usize,
    cursor_col: usize,
    cols: usize,
    modified_fields: &[(usize, usize, &[u8])], // (row, col, data)
) -> Vec<u8> {
    let mut stream = Vec::new();

    // AID byte
    stream.push(aid_code);

    // Cursor address (2 bytes)
    let cursor_addr = ((cursor_row - 1) * cols + (cursor_col - 1)) as u16;
    let encoded = encode_buffer_address(cursor_addr);
    stream.push(encoded[0]);
    stream.push(encoded[1]);

    // Modified fields: SBA + data for each
    for &(row, col, data) in modified_fields {
        let addr = ((row - 1) * cols + (col - 1)) as u16;
        stream.push(order::SBA);
        let encoded = encode_buffer_address(addr);
        stream.push(encoded[0]);
        stream.push(encoded[1]);
        stream.extend_from_slice(data);
    }

    stream
}

/// Perform TN3270E telnet negotiation on a raw TCP stream.
///
/// Returns the negotiated state. This handles DO/WILL/WONT/DONT exchanges
/// for BINARY, EOR, and TN3270E options.
pub fn negotiate(stream: &mut TcpStream) -> io::Result<Tn3270State> {
    let mut buf = [0u8; 256];
    let mut state = Tn3270State::Negotiating;

    // Read initial negotiation from server
    let n = stream.read(&mut buf)?;
    if n == 0 {
        return Ok(Tn3270State::Disconnected);
    }

    let mut i = 0;
    let mut responses = Vec::new();

    while i < n {
        if buf[i] == telnet::IAC && i + 2 < n {
            let verb = buf[i + 1];
            let option = buf[i + 2];

            match verb {
                telnet::DO => {
                    // Server asks us to enable an option
                    match option {
                        telnet::BINARY | telnet::END_OF_RECORD | telnet::TN3270E => {
                            responses.extend_from_slice(&[telnet::IAC, telnet::WILL, option]);
                        }
                        _ => {
                            responses.extend_from_slice(&[telnet::IAC, telnet::WONT, option]);
                        }
                    }
                }
                telnet::WILL => {
                    // Server offers an option
                    match option {
                        telnet::BINARY | telnet::END_OF_RECORD | telnet::TN3270E => {
                            responses.extend_from_slice(&[telnet::IAC, telnet::DO, option]);
                            if option == telnet::TN3270E {
                                state = Tn3270State::Connected;
                            }
                        }
                        _ => {
                            responses.extend_from_slice(&[telnet::IAC, telnet::DONT, option]);
                        }
                    }
                }
                _ => {}
            }
            i += 3;
        } else {
            i += 1;
        }
    }

    // Send our responses
    if !responses.is_empty() {
        stream.write_all(&responses)?;
        stream.flush()?;
    }

    if state == Tn3270State::Negotiating {
        // Fallback to basic TN3270
        state = Tn3270State::ConnectedBasic;
    }

    Ok(state)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_address_roundtrip() {
        for addr in [0, 1, 79, 80, 1919, 500] {
            let encoded = encode_buffer_address(addr);
            let decoded = decode_buffer_address(encoded[0], encoded[1]);
            assert_eq!(decoded, addr, "roundtrip failed for address {}", addr);
        }
    }

    #[test]
    fn test_buffer_address_specific() {
        // Address 0 should encode to known values
        let encoded = encode_buffer_address(0);
        assert_eq!(encoded, [0x40, 0x40]); // Both map to 0
    }

    #[test]
    fn test_decode_6bit_known_values() {
        assert_eq!(decode_6bit(0x40), 0);
        assert_eq!(decode_6bit(0xC1), 1);
        assert_eq!(decode_6bit(0xF0), 48);
        assert_eq!(decode_6bit(0x7F), 63);
    }

    #[test]
    fn test_generate_inbound_stream_enter() {
        let stream = generate_inbound_stream(
            aid::ENTER,
            1, 1, 80,
            &[],
        );

        assert_eq!(stream[0], aid::ENTER);
        // Cursor at (1,1) = address 0
        let cursor_addr = decode_buffer_address(stream[1], stream[2]);
        assert_eq!(cursor_addr, 0);
        assert_eq!(stream.len(), 3); // AID + 2 cursor bytes, no fields
    }

    #[test]
    fn test_generate_inbound_stream_with_field() {
        let field_data = b"HELLO";
        let stream = generate_inbound_stream(
            aid::ENTER,
            1, 1, 80,
            &[(1, 12, field_data)],
        );

        assert_eq!(stream[0], aid::ENTER);
        // After AID + cursor (3 bytes), SBA + address (3 bytes) + data (5 bytes)
        assert_eq!(stream.len(), 3 + 3 + 5);
        assert_eq!(stream[3], order::SBA);
        assert_eq!(&stream[6..11], b"HELLO");
    }

    #[test]
    fn test_parse_data_stream_write() {
        let mut data = vec![command::WRITE, 0x00]; // Write + WCC
        // SBA to row 1, col 1 (address 0)
        let addr = encode_buffer_address(0);
        data.push(order::SBA);
        data.push(addr[0]);
        data.push(addr[1]);
        // SF (start field)
        data.push(order::SF);
        data.push(0x20); // protected
        // Data
        data.extend_from_slice(b"Hello");

        let record = DataStreamRecord::parse(&data, 80).unwrap();
        assert_eq!(record.command, command::WRITE);
        assert_eq!(record.orders.len(), 3); // SBA, SF, Data

        match &record.orders[0] {
            DataStreamOrder::SetBufferAddress { row, col } => {
                assert_eq!(*row, 1);
                assert_eq!(*col, 1);
            }
            _ => panic!("Expected SBA"),
        }

        match &record.orders[1] {
            DataStreamOrder::StartField { attribute } => {
                assert_eq!(*attribute, 0x20);
            }
            _ => panic!("Expected SF"),
        }

        match &record.orders[2] {
            DataStreamOrder::Data(bytes) => {
                assert_eq!(bytes, b"Hello");
            }
            _ => panic!("Expected Data"),
        }
    }

    #[test]
    fn test_parse_insert_cursor() {
        let data = vec![command::WRITE, 0x00, order::IC];
        let record = DataStreamRecord::parse(&data, 80).unwrap();
        assert_eq!(record.orders.len(), 1);
        assert!(matches!(record.orders[0], DataStreamOrder::InsertCursor));
    }

    #[test]
    fn test_parse_repeat_to_address() {
        let addr = encode_buffer_address(160); // row 3, col 1
        let data = vec![
            command::WRITE, 0x00,
            order::RA, addr[0], addr[1], 0x00, // Fill with nulls to addr 160
        ];
        let record = DataStreamRecord::parse(&data, 80).unwrap();
        assert_eq!(record.orders.len(), 1);
        match &record.orders[0] {
            DataStreamOrder::RepeatToAddress { row, col, character } => {
                assert_eq!(*row, 3);
                assert_eq!(*col, 1);
                assert_eq!(*character, 0x00);
            }
            _ => panic!("Expected RA"),
        }
    }

    #[test]
    fn test_is_order() {
        assert!(is_order(order::SBA));
        assert!(is_order(order::SF));
        assert!(is_order(order::SFE));
        assert!(is_order(order::IC));
        assert!(!is_order(0xC1)); // 'A' in EBCDIC - data, not order
        assert!(!is_order(0x00));
    }

    #[test]
    fn test_tn3270_config_default() {
        let config = Tn3270Config::default();
        assert_eq!(config.host, "localhost");
        assert_eq!(config.port, 23);
        assert!(config.lu_name.is_none());
        assert_eq!(config.device_type, "IBM-3278-2-E");
    }

    #[test]
    fn test_parse_sfe() {
        let data = vec![
            command::WRITE, 0x00,
            order::SFE, 0x02, // 2 pairs
            0xC0, 0x20,       // Basic 3270 attribute: protected
            0x41, 0xF4,       // Color: green
        ];
        let record = DataStreamRecord::parse(&data, 80).unwrap();
        assert_eq!(record.orders.len(), 1);
        match &record.orders[0] {
            DataStreamOrder::StartFieldExtended { pairs } => {
                assert_eq!(pairs.len(), 2);
                assert_eq!(pairs[0], (0xC0, 0x20));
                assert_eq!(pairs[1], (0x41, 0xF4));
            }
            _ => panic!("Expected SFE"),
        }
    }
}

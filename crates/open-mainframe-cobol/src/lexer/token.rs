//! Token definitions for COBOL lexical analysis.
//!
//! Tokens are the fundamental units produced by the lexer. Each token
//! has a kind, span (location in source), and optional associated data.

use crate::lexer::span::Span;

/// A token produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token.
    pub kind: TokenKind,
    /// Location in source code.
    pub span: Span,
}

impl Token {
    /// Create a new token.
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Check if this is an end-of-file token.
    pub fn is_eof(&self) -> bool {
        self.kind == TokenKind::Eof
    }

    /// Check if this token is a specific keyword.
    pub fn is_keyword(&self, kw: Keyword) -> bool {
        matches!(&self.kind, TokenKind::Keyword(k) if *k == kw)
    }
}

/// The kind of token.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum TokenKind {
    // Literals
    /// Integer literal (e.g., 123)
    IntegerLiteral(i64),
    /// Decimal literal (e.g., 123.45)
    DecimalLiteral(String),
    /// String literal (e.g., "HELLO" or 'HELLO')
    StringLiteral(String),
    /// Hex literal (e.g., X"FF")
    HexLiteral(String),
    /// National string literal (e.g., N"...")
    NationalLiteral(String),

    // Identifiers and keywords
    /// User-defined identifier
    Identifier(String),
    /// Reserved keyword
    Keyword(Keyword),

    // Picture clause (special handling)
    /// Picture string (e.g., X(10), 9(5)V99)
    PictureString(String),

    // Operators
    /// + (plus or positive sign)
    Plus,
    /// - (minus or negative sign)
    Minus,
    /// * (multiply)
    Star,
    /// / (divide)
    Slash,
    /// ** (exponentiation)
    DoubleStar,
    /// = (equals)
    Equals,
    /// > (greater than)
    GreaterThan,
    /// < (less than)
    LessThan,
    /// >= (greater than or equal)
    GreaterEquals,
    /// <= (less than or equal)
    LessEquals,
    /// <> or NOT = (not equal)
    NotEquals,

    // Punctuation
    /// . (period/full stop - statement terminator)
    Period,
    /// , (comma)
    Comma,
    /// ; (semicolon)
    Semicolon,
    /// : (colon)
    Colon,
    /// ( (left parenthesis)
    LeftParen,
    /// ) (right parenthesis)
    RightParen,
    /// :: (double colon, for qualification)
    DoubleColon,

    // Reference modification
    /// ( position : length ) for reference modification is parsed as tokens

    // Special
    /// End of file
    Eof,
    /// Error token (lexer error recovery)
    Error(String),
    /// Newline (for format-sensitive parsing)
    Newline,
}

impl TokenKind {
    /// Get a human-readable description of this token kind.
    pub fn description(&self) -> &'static str {
        match self {
            TokenKind::IntegerLiteral(_) => "integer literal",
            TokenKind::DecimalLiteral(_) => "decimal literal",
            TokenKind::StringLiteral(_) => "string literal",
            TokenKind::HexLiteral(_) => "hex literal",
            TokenKind::NationalLiteral(_) => "national literal",
            TokenKind::Identifier(_) => "identifier",
            TokenKind::Keyword(_) => "keyword",
            TokenKind::PictureString(_) => "picture string",
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Star => "'*'",
            TokenKind::Slash => "'/'",
            TokenKind::DoubleStar => "'**'",
            TokenKind::Equals => "'='",
            TokenKind::GreaterThan => "'>'",
            TokenKind::LessThan => "'<'",
            TokenKind::GreaterEquals => "'>='",
            TokenKind::LessEquals => "'<='",
            TokenKind::NotEquals => "'<>'",
            TokenKind::Period => "'.'",
            TokenKind::Comma => "','",
            TokenKind::Semicolon => "';'",
            TokenKind::Colon => "':'",
            TokenKind::LeftParen => "'('",
            TokenKind::RightParen => "')'",
            TokenKind::DoubleColon => "'::'",
            TokenKind::Eof => "end of file",
            TokenKind::Error(_) => "error",
            TokenKind::Newline => "newline",
        }
    }
}

/// COBOL reserved keywords.
///
/// This enum contains all COBOL-85 reserved words plus commonly used
/// COBOL-2002 and IBM extensions.
///
/// Generated from the master keyword table in `macros.rs`.
/// To add a new keyword, add one line there — the enum variant, HashMap
/// entry, and `as_str()` arm are all generated automatically.
macro_rules! gen_keyword_enum_and_as_str {
    (
        @primary { $($pvar:ident => $pstr:literal),* $(,)? }
        @alias   { $($avar:ident => $astr:literal),* $(,)? }
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[non_exhaustive]
        #[allow(missing_docs)]
        pub enum Keyword {
            $($pvar,)*
            $($avar,)*
        }

        impl Keyword {
            /// Get the string representation of this keyword.
            pub fn as_str(&self) -> &'static str {
                match self {
                    $(Keyword::$pvar => $pstr,)*
                    $(Keyword::$avar => $astr,)*
                }
            }
        }
    };
}
for_all_keywords!(gen_keyword_enum_and_as_str);

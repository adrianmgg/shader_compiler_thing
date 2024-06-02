use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub(crate) enum Token {
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("->")]
    RightArrow,
    #[token("=")]
    SingleEquals,
    #[token("==")]
    DoubleEquals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("@")]
    AtSign,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenCurly,
    #[token("}")]
    CloseCurly,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,

    // TODO add other bases syntax
    // TODO add underscore in number support
    // TODO add float support
    #[regex(r"[+\-]?[0-9]+(?:[ui](?:8|16|32|64))")]
    Number,

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier,
}

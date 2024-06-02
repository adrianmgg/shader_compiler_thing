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

#[cfg(test)]
mod tests {
    macro_rules! lex_test {
        ($testname:ident, $input:literal, [ $($token:ident)* ]) => {
            #[test]
            fn $testname() {
                use crate::lex::Token;
                use logos::Logos;
                let mut lex = Token::lexer($input);
                {
                    use Token::*;
                    $(
                        assert_eq!(lex.next(), Some(Ok($token)));
                    )*
                }
                assert_eq!(lex.next(), None);
            }
        };
    }

    mod single_token {
        lex_test!(comma, ",", [Comma]);
        lex_test!(colon, ":", [Colon]);
        lex_test!(semicolon, ";", [Semicolon]);
        lex_test!(rightarrow, "->", [RightArrow]);
        lex_test!(singleequals, "=", [SingleEquals]);
        lex_test!(doubleequals, "==", [DoubleEquals]);
        lex_test!(plus, "+", [Plus]);
        lex_test!(minus, "-", [Minus]);
        lex_test!(asterisk, "*", [Asterisk]);
        lex_test!(slash, "/", [Slash]);
        lex_test!(atsign, "@", [AtSign]);
        lex_test!(openbracket, "[", [OpenBracket]);
        lex_test!(closebracket, "]", [CloseBracket]);
        lex_test!(opencurly, "{", [OpenCurly]);
        lex_test!(closecurly, "}", [CloseCurly]);
        lex_test!(openparen, "(", [OpenParen]);
        lex_test!(closeparen, ")", [CloseParen]);
    }
}

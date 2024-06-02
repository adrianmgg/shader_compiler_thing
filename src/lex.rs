use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
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
    #[regex(
        r"(?x)
##########  vvv IMPORTANT vvv  ###########
# if you make any changes to this regex, #
# be sure to copy them to the other spot #
# in this file where this regex appears. #
##########  ^^^^ READ ME ^^^^  ###########

# base prefix
# (TODO)
# the number itself
(?<sign>[+\-]?)
(?<number>[0-9]+)
# type suffix
(?<type_>[ui](?:8|16|32|64))
",
        number_token_callback
    )]
    Number(NumberData),

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Identifier,
}

ctreg::regex! {
    pub NumberTokenPattern = r"(?x)
##########  vvv IMPORTANT vvv  ###########
# if you make any changes to this regex, #
# be sure to copy them to the other spot #
# in this file where this regex appears. #
##########  ^^^^ READ ME ^^^^  ###########

^  # start of input (DO NOT include this in the lexer's copy of the regex)

# base prefix
# (TODO)
# the number itself
(?<sign>[+\-]?)
(?<number>[0-9]+)
# type suffix
(?<type_>[ui](?:8|16|32|64))

$  # end of input (DO NOT include this in the lexer's copy of the regex)
"
}

fn number_token_callback(lex: &mut logos::Lexer<Token>) -> Result<NumberData, ()> {
    // TODO don't instance this every time lol
    let number_token_pattern = NumberTokenPattern::new();
    // TODO don't unwrap here
    let capture = number_token_pattern.captures(lex.slice()).unwrap();

    // TODO implement base prefix

    match capture.type_.content {
        "u8" => todo!(),
        "u16" => todo!(),
        "u32" => todo!(),
        "u64" => todo!(),
        "i8" => todo!(),
        "i16" => todo!(),
        "i32" => todo!(),
        "i64" => todo!(),
        _ => todo!(),
    };

    todo!()
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct NumberData {}

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

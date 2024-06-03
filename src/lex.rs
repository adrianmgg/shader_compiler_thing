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
(?:0*(?<base_prefix>(?:2|3|4|5|6|7|8|9|10|11|12|13|14|15|16)x))?
# the number itself
(?<sign>[+\-]?)
(?<number>[0-9a-fA-F]+)
# type suffix
(?<type_suffix>[ui](?:8|16|32|64))
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
(?:0*(?<base_prefix>(?:2|3|4|5|6|7|8|9|10|11|12|13|14|15|16)x))?
# the number itself
(?<sign>[+\-]?)
(?<number>[0-9a-fA-F]+)
# type suffix
(?<type_suffix>[ui](?:8|16|32|64))

$  # end of input (DO NOT include this in the lexer's copy of the regex)
"
}

fn number_token_callback(lex: &mut logos::Lexer<Token>) -> Result<NumberData, ()> {
    // TODO don't instance this every time lol
    let number_token_pattern = NumberTokenPattern::new();
    // TODO don't unwrap here
    let capture = number_token_pattern.captures(lex.slice()).unwrap();

    enum NumBase {
        B2,
        B3,
        B4,
        B5,
        B6,
        B7,
        B8,
        B9,
        B10,
        B11,
        B12,
        B13,
        B14,
        B15,
        B16,
    }
    impl NumBase {
        fn radix(&self) -> u32 {
            match self {
                Self::B2 => 2,
                Self::B3 => 3,
                Self::B4 => 4,
                Self::B5 => 5,
                Self::B6 => 6,
                Self::B7 => 7,
                Self::B8 => 8,
                Self::B9 => 9,
                Self::B10 => 10,
                Self::B11 => 11,
                Self::B12 => 12,
                Self::B13 => 13,
                Self::B14 => 14,
                Self::B15 => 15,
                Self::B16 => 16,
            }
        }
    }
    enum NumType {
        I8,
        I16,
        I32,
        I64,
        U8,
        U16,
        U32,
        U64,
    }
    impl NumType {
        fn parse(&self, src: &str, radix: u32) -> Result<NumberData, std::num::ParseIntError> {
            match self {
                Self::I8 => i8::from_str_radix(src, radix).map(NumberData::SignedInt8),
                Self::I16 => i16::from_str_radix(src, radix).map(NumberData::SignedInt16),
                Self::I32 => i32::from_str_radix(src, radix).map(NumberData::SignedInt32),
                Self::I64 => i64::from_str_radix(src, radix).map(NumberData::SignedInt64),
                Self::U8 => u8::from_str_radix(src, radix).map(NumberData::UnsignedInt8),
                Self::U16 => u16::from_str_radix(src, radix).map(NumberData::UnsignedInt16),
                Self::U32 => u32::from_str_radix(src, radix).map(NumberData::UnsignedInt32),
                Self::U64 => u64::from_str_radix(src, radix).map(NumberData::UnsignedInt64),
            }
        }
    }

    let base = match capture.base_prefix {
        None => NumBase::B10,
        Some(base_prefix) => match base_prefix.content {
            "2x" => NumBase::B2,
            "3x" => NumBase::B3,
            "4x" => NumBase::B4,
            "5x" => NumBase::B5,
            "6x" => NumBase::B6,
            "7x" => NumBase::B7,
            "8x" => NumBase::B8,
            "9x" => NumBase::B9,
            "10x" => NumBase::B10,
            "11x" => NumBase::B11,
            "12x" => NumBase::B12,
            "13x" => NumBase::B13,
            "14x" => NumBase::B14,
            "15x" => NumBase::B15,
            "16x" => NumBase::B16,
            _ => panic!(),
        },
    };

    let r#type = match capture.type_suffix.content {
        "u8" => NumType::U8,
        "u16" => NumType::U16,
        "u32" => NumType::U32,
        "u64" => NumType::U64,
        "i8" => NumType::I8,
        "i16" => NumType::I16,
        "i32" => NumType::I32,
        "i64" => NumType::I64,
        _ => panic!(),
    };

    // TODO actually have proper errors here
    r#type
        .parse(capture.number.content, base.radix())
        .map_err(|_| ())
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum NumberData {
    UnsignedInt8(u8),
    UnsignedInt16(u16),
    UnsignedInt32(u32),
    UnsignedInt64(u64),
    SignedInt8(i8),
    SignedInt16(i16),
    SignedInt32(i32),
    SignedInt64(i64),
}

#[cfg(test)]
mod tests {
    macro_rules! lex_test {
        ($testname:ident, $input:literal, [ $($token:expr)* ]) => {
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

    mod numbers {
        use crate::lex::NumberData;
        lex_test!(implicit_base, "12u32", [Number(NumberData::UnsignedInt32(12u32))]);
        lex_test!(explicit_base, "16xffu8", [Number(NumberData::UnsignedInt8(0xffu8))]);
    }
}

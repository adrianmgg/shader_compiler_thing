use logos::Logos;

use crate::ast::YarnStr;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub(crate) enum Token<'source> {
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
    #[token(".")]
    Period,

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

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*", identifier_callback)]
    Identifier(YarnStr<'source>),
}

fn identifier_callback<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> YarnStr<'a> {
    YarnStr::new(lex.slice())
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

/// given a macro_rules body, call said macro with info pertaining to each base/radix supported by the lexer
///
/// calls provided macro with comma separated paren-wrapped pairs containing, in order:
/// - number of the base (`literal`)
/// - associated internal variant name (`ident`)
macro_rules! for_each_supported_num_base {
    ($mrimpl:tt) => {
        #[doc(hidden)]
        macro_rules! __foreach_tmp $mrimpl
        __foreach_tmp!(
            (2, B2),
            (3, B3),
            (4, B4),
            (5, B5),
            (6, B6),
            (7, B7),
            (8, B8),
            (9, B9),
            (10, B10),
            (11, B11),
            (12, B12),
            (13, B13),
            (14, B14),
            (15, B15),
            (16, B16)
        );
    };
}

/// given a macro_rules body, call said macro with info pertaining to each numeric type supported by the lexer
///
/// calls provided macro with comma separated paren-wrapped pairs containing, in order:
/// - associated rust type (`ty`)
/// - corresponding [`NumberData`] variant name (`ident`)
/// - suffix used to mark literals as that type, as a string (`literal`)
macro_rules! for_each_supported_number_type {
    ($mrimpl:tt) => {
        #[doc(hidden)]
        macro_rules! __foreach_tmp $mrimpl
        __foreach_tmp!(
            (u8, UnsignedInt8, "u8"),
            (u16, UnsignedInt16, "u16"),
            (u32, UnsignedInt32, "u32"),
            (u64, UnsignedInt64, "u64"),
            (i8, SignedInt8, "i8"),
            (i16, SignedInt16, "i16"),
            (i32, SignedInt32, "i32"),
            (i64, SignedInt64, "i64")
        );
    };
}

fn number_token_callback<'source>(
    lex: &mut logos::Lexer<'source, Token<'source>>,
) -> Result<NumberData, ()> {
    // TODO don't instance this every time lol
    let number_token_pattern = NumberTokenPattern::new();
    // TODO don't unwrap here
    let capture = number_token_pattern.captures(lex.slice()).unwrap();

    for_each_supported_num_base!({
        ($(($base:literal, $variant:ident)),*) => {
            enum NumBase {
                $($variant,)*
            }
            impl NumBase {
                fn radix(&self) -> u32 {
                    match self {
                        $(Self::$variant => $base,)*
                    }
                }
                fn from_prefix_str(base_prefix: &str) -> Self {
                    match base_prefix {
                        $(
                            concat!(stringify!($base), "x") => NumBase::$variant,
                        )*
                        _ => panic!(),
                    }
                }
            }
        };
    });

    for_each_supported_number_type!({
        ($(($typ:ty, $variant:ident, $suffix:literal)),*) => {
            enum NumType {
                $($variant,)*
            }
            impl NumType {
                fn parse(&self, src: &str, radix: u32) -> Result<NumberData, std::num::ParseIntError> {
                    match self {
                        $(Self::$variant => <$typ>::from_str_radix(src, radix).map(NumberData::$variant),)*
                    }
                }
                fn from_suffix_str(type_suffix: &str) -> Self {
                    match type_suffix {
                        $($suffix => NumType::$variant,)*
                        _ => panic!(),
                    }
                }
            }
        };
    });

    let base = match capture.base_prefix {
        None => NumBase::B10,
        Some(base_prefix) => NumBase::from_prefix_str(base_prefix.content),
    };

    let r#type = NumType::from_suffix_str(capture.type_suffix.content);

    // TODO actually have proper errors here
    r#type
        .parse(capture.number.content, base.radix())
        .map_err(|_| ())
}

for_each_supported_number_type!({
    ($(($valtype:ty, $variantname:ident, $_suffixstr:literal)),*) => {
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub(crate) enum NumberData {
            $($variantname($valtype),)*
        }
        $(
            impl From<$valtype> for NumberData {
                fn from(value: $valtype) -> Self {
                    NumberData::$variantname(value)
                }
            }
        )*
    };
});

/// given a macro_rules body, call said macro with info pertaining to each 'simple' (i.e. value-free) token.
///
/// calls provided macro with comma separated paren-wrapped pairs containing, in order:
/// - a lowercase snake case identifier corresponding to the token (`ident`)
/// - ... TODO ...
/// - a string literal of the token itself
macro_rules! for_each_simple_token {
    ($mrimpl:tt) => {
        #[doc(hidden)]
        macro_rules! __foreach_tmp $mrimpl
        __foreach_tmp!(
            (comma, Comma, ","),
            (colon, Colon, ":"),
            (semicolon, Semicolon, ";"),
            (rightarrow, RightArrow, "->"),
            (singleequals, SingleEquals, "="),
            (doubleequals, DoubleEquals, "=="),
            (plus, Plus, "+"),
            (minus, Minus, "-"),
            (asterisk, Asterisk, "*"),
            (slash, Slash, "/"),
            (atsign, AtSign, "@"),
            (openbracket, OpenBracket, "["),
            (closebracket, CloseBracket, "]"),
            (opencurly, OpenCurly, "{"),
            (closecurly, CloseCurly, "}"),
            (openparen, OpenParen, "("),
            (closeparen, CloseParen, ")"),
            (period, Period, ".")
        );
    };
}
pub(crate) use for_each_simple_token;

for_each_simple_token!({
    ($(($st_fnname:ident, $st_ident:ident, $st_str:literal)),*) => {
        pub(crate) enum TokenType {
            Identifier,
            Number,
            $($st_ident,)*
        }
        impl<'source> winnow::stream::ContainsToken<Token<'source>> for TokenType {
            #[inline]
            fn contains_token(&self, token: Token<'source>) -> bool {
                match self {
                    $(Self::$st_ident => (token == Token::$st_ident),)*
                    Self::Identifier => matches!(token, Token::Identifier(_)),
                    Self::Number => matches!(token, Token::Number(_)),
                }
            }
        }
        impl<'source, 'a> winnow::stream::ContainsToken<&'a Token<'source>> for TokenType {
            #[inline]
            fn contains_token(&self, token: &'a Token<'source>) -> bool {
                match self {
                    $(Self::$st_ident => matches!(token, Token::$st_ident),)*
                    Self::Identifier => matches!(token, Token::Identifier(_)),
                    Self::Number => matches!(token, Token::Number(_)),
                }
            }
        }
    };
});

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
                    $(
                        assert_eq!(lex.next(), Some(Ok($token)));
                    )*
                }
                assert_eq!(lex.next(), None);
            }
        };
    }

    mod simple_tokens {
        crate::lex::for_each_simple_token!({
            ($(($name:ident, $tok:ident, $str:literal)),*) => {
                $(lex_test!($name, $str, [crate::lex::Token::$tok]);)*
            };
        });
    }

    mod numbers {
        use crate::lex::{NumberData, Token::*};
        lex_test!(
            implicit_base,
            "12u32",
            [Number(NumberData::UnsignedInt32(12u32))]
        );
        lex_test!(
            explicit_base,
            "16xffu8",
            [Number(NumberData::UnsignedInt8(0xffu8))]
        );
    }

    mod identifiers {
        use crate::lex::Token::*;
        lex_test!(simple, "foo", [Identifier("foo".into())]);
    }
}

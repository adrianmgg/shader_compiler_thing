// TODO move this alias elsewhere
pub(crate) type YarnStr<'s> = byteyarn::YarnBox<'s, str>;

pub(crate) struct Identifier<'s> {
    pub(crate) name: YarnStr<'s>,
}

impl<'s> Identifier<'s> {
    pub(crate) fn new(name: YarnStr<'s>) -> Self {
        Self { name }
    }
}

pub(crate) enum Number {
    Signed8 { value: i8 },
    Signed16 { value: i16 },
    Signed32 { value: i32 },
    Signed64 { value: i64 },
    Unsigned8 { value: u8 },
    Unsigned16 { value: u16 },
    Unsigned32 { value: u32 },
    Unsigned64 { value: u64 },
}

use std::convert::Infallible;

use nom::{
    combinator::map_res,
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};

pub fn le_u8_as_bool(input: &[u8]) -> IResult<&[u8], bool> {
    map_res(le_u8, |x| Result::<_, Infallible>::Ok(x == 0))(input)
}

pub fn le_u16_as_usize(input: &[u8]) -> IResult<&[u8], usize> {
    map_res(le_u16, |x| Result::<_, Infallible>::Ok(x as usize))(input)
}

pub fn le_u32_as_usize(input: &[u8]) -> IResult<&[u8], usize> {
    map_res(le_u32, |x| Result::<_, Infallible>::Ok(x as usize))(input)
}

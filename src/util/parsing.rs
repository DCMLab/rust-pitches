use nom::character::complete::{char, digit1};
use nom::combinator::map_res;
use nom::IResult;

pub fn parse_negative(i: &str) -> (&str, bool) {
    match char::<&str, ()>('-')(i) {
        Ok((i, _)) => (i, true),
        Err(_) => (i, false),
    }
}

pub fn parse_nat(i: &str) -> IResult<&str, i32, ()> {
    map_res(digit1, |x: &str| x.parse())(i)
}

pub fn parse_int(i: &str) -> IResult<&str, i32, ()> {
    let (i, neg) = parse_negative(i);
    let (i, n) = parse_nat(i)?;
    Ok((i, if neg { -n } else { n }))
}

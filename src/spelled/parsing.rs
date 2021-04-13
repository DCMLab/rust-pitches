use nom::branch::alt;
use nom::character::complete::{char, digit1, one_of};
use nom::combinator::{eof, map, map_res, opt};
use nom::multi::many1_count;
use nom::IResult;
use std::convert::TryFrom;
use crate::spelled::*;

enum Qual {
    Augmented(i32),
    Major,
    Perfect,
    Minor,
    Diminished(i32),
}

fn nom_fail<'a, R>() -> IResult<&'a str, R, ()> {
    Err(nom::Err::Error(()))
}

fn parse_aug(i: &str) -> IResult<&str, Qual, ()> {
    let (i, len) = map_res(many1_count(char('a')), i32::try_from)(i)?;
    Ok((i, Qual::Augmented(len)))
}

fn parse_dim(i: &str) -> IResult<&str, Qual, ()> {
    let (i, len) = map_res(many1_count(char('d')), i32::try_from)(i)?;
    Ok((i, Qual::Diminished(len)))
}

fn parse_qual(i: &str) -> IResult<&str, Qual, ()> {
    let (i, qual) = one_of("MPm")(i)?;
    match qual {
        'M' => Ok((i, Qual::Major)),
        'm' => Ok((i, Qual::Minor)),
        'P' => Ok((i, Qual::Perfect)),
        _ => nom_fail(),
    }
}

fn parse_size(i: &str) -> IResult<&str, i32, ()> {
    map_res(one_of("1234567"), |n| {
        n.to_digit(10).map(|n| (n as i32) - 1).ok_or("")
    })(i)
}

fn parse_dia(i: &str) -> IResult<&str, i32, ()> {
    let (o_qual, qual) = alt((parse_qual, parse_aug, parse_dim))(i)?;
    let (o_dia, dia) = parse_size(o_qual)?;
    let alteration = if is_perfect(dia) {
        match qual {
            Qual::Augmented(n) => n,
            Qual::Perfect => 0,
            Qual::Diminished(n) => -n,
            _ => return nom_fail(),
        }
    } else {
        match qual {
            Qual::Augmented(n) => n,
            Qual::Major => 0,
            Qual::Minor => -1,
            Qual::Diminished(n) => (-n) - 1,
            _ => return nom_fail(),
        }
    };
    Ok((o_dia, ((dia * 2 + 1) % 7) - 1 + (7 * alteration)))
}

fn parse_negative(i: &str) -> (&str, bool) {
    match char::<&str, ()>('-')(i) {
        Ok((i, _)) => (i, true),
        Err(_) => (i, false),
    }
}

fn parse_nat(i: &str) -> IResult<&str, i32, ()> {
    map_res(digit1, |x| i32::from_str(x))(i)
}

fn parse_int(i: &str) -> IResult<&str, i32, ()> {
    let (i, neg) = parse_negative(i);
    let (i, n) = parse_nat(i)?;
    Ok((i, if neg { -n } else { n }))
}

fn parse_octs(i: &str) -> IResult<&str, i32, ()> {
    let (i, _) = char(':')(i)?;
    parse_nat(i)
}

fn parse_alteration(i: &str) -> IResult<&str, i32, ()> {
    fn count_acc<'a>(c1: char, c2: char) -> impl nom::Parser<&'a str, i32, ()> {
        map_res(
            alt((many1_count(char(c1)), many1_count(char(c2)))),
            i32::try_from,
        )
    }
    let flats = map(count_acc('b', '♭'), |n| -n);
    let sharps = count_acc('#', '♯');
    map(opt(alt((flats, sharps))), |n| n.unwrap_or(0))(i)
}

fn parse_name(i: &str) -> IResult<&str, i32, ()> {
    let (i, letter) = one_of("ABCDEFG")(i)?;
    let (i, alteration) = parse_alteration(i)?;
    let dia = (((letter as i16) - ('A' as i16) - 2).rem_euclid(7)) as i32;
    Ok((i, ((dia * 2 + 1).rem_euclid(7)) - 1 + 7 * alteration))
}

pub fn parse_spelled(s: &str) -> Result<SpelledInterval, nom::Err<()>> {
    let (s, negate) = parse_negative(s);
    let (s, fifths) = parse_dia(s)?;
    let (s, octs) = parse_octs(s)?;
    eof::<&str, ()>(s)?;
    
    let interval = SpelledInterval::new(fifths, octs - (fifths * 4).div_euclid(7));
    Ok(if negate { -interval } else { interval })
}

pub fn parse_sic(s: &str) -> Result<SpelledIC, nom::Err<()>> {
    let (s, negate) = parse_negative(s);
    let (s, fifths) = parse_dia(s)?;
    eof::<&str, ()>(s)?;
    
    let interval = SpelledIC::new(fifths);
    Ok(if negate { -interval } else { interval })
}

pub fn parse_spelledp(s: &str) -> Result<Pitch<SpelledInterval>, nom::Err<()>> {
    let (s, fifths) = parse_name(s)?;
    let (s, oct) = parse_int(s)?;
    eof::<&str, ()>(s)?;
    
    Ok(spelledp(fifths, oct - ((fifths * 4).div_euclid(7))))
}

pub fn parse_spc(s: &str) -> Result<Pitch<SpelledIC>, nom::Err<()>> {
    let (s, fifths) = parse_name(s)?;
    eof::<&str, ()>(s)?;

    Ok(spc(fifths))
}

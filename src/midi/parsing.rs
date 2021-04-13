use super::*;
use crate::util::parsing::{parse_int, parse_nat};
use nom::bytes::complete::tag;
use nom::combinator::eof;

pub fn parse_i(s: &str) -> Result<MidiInterval, nom::Err<()>> {
    let (s, _) = tag("i")(s)?;
    let (s, i) = parse_int(s)?;
    eof::<&str, ()>(s)?;
    Ok(midi(i))
}

pub fn parse_ic(s: &str) -> Result<MidiIC, nom::Err<()>> {
    let (s, _) = tag("ic")(s)?;
    let (s, i) = parse_nat(s)?;
    eof::<&str, ()>(s)?;
    Ok(midic(i))
}

pub fn parse_p(s: &str) -> Result<MidiPitch, nom::Err<()>> {
    let (s, _) = tag("p")(s)?;
    let (s, i) = parse_int(s)?;
    eof::<&str, ()>(s)?;
    Ok(midip(i))
}

pub fn parse_pc(s: &str) -> Result<MidiPC, nom::Err<()>> {
    let (s, _) = tag("pc")(s)?;
    let (s, i) = parse_nat(s)?;
    eof::<&str, ()>(s)?;
    Ok(midipc(i))
}

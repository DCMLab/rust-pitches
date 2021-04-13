use pitches::midi::*;
use pitches::spelled::*;
use pitches::*;

fn transpose_and_add_octave<I>(pitches: &Vec<Pitch<I>>, by: I) -> Vec<Pitch<I>>
where
    I: Interval,
{
    pitches.iter().map(|p| *p + by + I::OCTAVE).collect()
}

fn main() {
    // apply to MIDI pitches/intervals
    println!("MIDI:");
    let midi_pitches = vec![midip(60), midip(64), midip(67), midip(72)];
    for p in transpose_and_add_octave(&midi_pitches, midi(3)).iter() {
        println!("- {}", p);
    }

    // apply to spelled pitches/intervals
    println!("spelled:");
    let spelled_pitches: Vec<SpelledPitch> = vec!["C4", "E♭4", "G♯4", "C5"]
        .iter()
        .map(|s| s.parse().unwrap())
        .collect();
    let spelled_interval = "M3:0".parse().unwrap();
    for p in transpose_and_add_octave(&spelled_pitches, spelled_interval).iter() {
        println!("- {}", p);
    }
}

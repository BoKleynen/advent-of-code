use std::cmp::{max, min};
use std::fs;
use std::ops::RangeInclusive;

use clap::Parser;

#[derive(Parser, Debug)]
struct Day04 {
	input: String,

	#[arg(long, default_value_t = false)]
	overlapping: bool,
}

fn main() {
	let cmd = Day04::parse();

	let s = fs::read_to_string(cmd.input).unwrap();
	let result = if cmd.overlapping {
		count_overlapping_pairs(&s)
	} else {
		count_containing_pairs(&s)
	};

	println!("{result}");
}

fn count_containing_pairs(s: &str) -> u32 {
	count_pairs_where(s, |(fst, snd)| {
		(fst.start() >= snd.start() && fst.end() <= snd.end())
			|| (fst.start() <= snd.start() && fst.end() >= snd.end())
	})
}

fn count_overlapping_pairs(s: &str) -> u32 {
	count_pairs_where(s, |(fst, snd)| {
		max(fst.start(), snd.start()) <= min(fst.end(), snd.end())
	})
}

fn count_pairs_where<P>(s: &str, predicate: P) -> u32
where
	P: FnMut(&(RangeInclusive<u32>, RangeInclusive<u32>)) -> bool,
{
	s.lines()
		.map(|line| {
			let mut ranges = line.split(',').map(|range| {
				let mut parts = range.split('-');
				let start = parts.next().unwrap().parse::<u32>().unwrap();
				let end = parts.next().unwrap().parse::<u32>().unwrap();
				start..=end
			});
			let fst = ranges.next().unwrap();
			let snd = ranges.next().unwrap();
			(fst, snd)
		})
		.filter(predicate)
		.count() as u32
}

#[cfg(test)]
mod test {
	use super::{count_containing_pairs, count_overlapping_pairs};

	const DATA: &str = r#"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"#;

	#[test]
	fn test_part1() {
		assert_eq!(count_containing_pairs(DATA), 2);
	}

	#[test]
	fn test_part2() {
		assert_eq!(count_overlapping_pairs(DATA), 4);
	}
}

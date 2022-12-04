use std::collections::HashSet;
use std::fs;

use clap::Parser;
use itertools::Itertools;

#[derive(Debug, Parser)]
struct Day03 {
	input: String,

	#[arg(long, default_value_t = false)]
	badges: bool,
}

fn main() {
	let cmd = Day03::parse();

	let s = fs::read_to_string(cmd.input).unwrap();
	let result = if cmd.badges { part2(&s) } else { part1(&s) };

	println!("{result}");
}

fn part1(s: &str) -> u32 {
	s.lines()
		.map(|line| {
			let (fst, snd) = line.split_at(line.len() / 2);
			let fst = HashSet::<char>::from_iter(fst.chars());
			let snd = HashSet::<char>::from_iter(snd.chars());
			let diff = fst.intersection(&snd);
			diff.into_iter().map(|&b| priority(b)).sum::<u32>()
		})
		.sum()
}

fn part2(s: &str) -> u32 {
	s.lines()
		.chunks(3)
		.into_iter()
		.map(|lines| {
			lines
				.map(|line| HashSet::<char>::from_iter(line.chars()))
				.reduce(|acc, set| acc.intersection(&set).copied().collect())
				.unwrap()
				.into_iter()
				.map(priority)
				.sum::<u32>()
		})
		.sum()
}

fn priority(b: char) -> u32 {
	match b {
		'A'..='Z' => b as u32 - 'A' as u32 + 27,
		'a'..='z' => b as u32 - 'a' as u32 + 1,
		_ => unreachable!(),
	}
}

#[cfg(test)]
mod test {
	use super::{part1, part2};

	const DATA: &str = r#"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"#;

	#[test]
	fn test_part1() {
		assert_eq!(part1(DATA), 157);
	}

	#[test]
	fn test_part2() {
		assert_eq!(part2(DATA), 70);
	}
}

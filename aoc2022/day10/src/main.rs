use std::fs;
use std::iter::FusedIterator;

use clap::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64, newline};
use nom::combinator::{iterator, map, opt, value};
use nom::sequence::{preceded, terminated};
use nom::IResult;

#[derive(Debug, Parser)]
struct Day10 {
	input: String,
}

fn main() {
	let cmd = Day10::parse();
	let input = fs::read_to_string(cmd.input).unwrap();

	let res1 = part1(&input);
	println!("{res1}");

	let res2 = part2(&input);
	println!("{res2}")
}

fn part1(input: &str) -> i64 {
	let mut it = iterator(input, terminated(parse_instruction, opt(newline)));
	let mut scan = Clock::new(&mut it).scan(1, |x, op| {
		let res = *x;
		match op {
			Some(Instruction::Addx(k)) => *x += k,
			_ => {}
		}
		Some(res)
	});

	20 * scan.nth(19).unwrap()
		+ (1..=5)
			.map(|i| (20 + i * 40) * scan.nth(39).unwrap())
			.sum::<i64>()
}

fn part2(input: &str) -> String {
	unimplemented!()
}

struct Clock<I: Iterator<Item = Instruction>> {
	iter: I,
	executing_instruction: Option<Instruction>,
}

impl<I: Iterator<Item = Instruction>> Clock<I> {
	fn new(iter: I) -> Self {
		Self {
			iter,
			executing_instruction: None,
		}
	}
}

impl<I: Iterator<Item = Instruction>> Iterator for Clock<I> {
	type Item = Option<I::Item>;

	fn next(&mut self) -> Option<Option<I::Item>> {
		if let Some(op) = self.executing_instruction.take() {
			return Some(Some(op));
		}

		match self.iter.next() {
			Some(op) => match op {
				Instruction::Noop => Some(Some(op)),
				Instruction::Addx(_) => {
					self.executing_instruction = Some(op);
					Some(None)
				}
			},
			None => None,
		}
	}
}

impl<I: Iterator<Item = Instruction>> FusedIterator for Clock<I> {}

#[derive(Debug, Clone, Copy)]
enum Instruction {
	Noop,
	Addx(i64),
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
	alt((
		value(Instruction::Noop, tag("noop")),
		map(preceded(tag("addx "), i64), Instruction::Addx),
	))(input)
}

#[cfg(test)]
mod test {
	use crate::part1;

	const DATA: &str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";

	#[test]
	fn test_part1() {
		assert_eq!(part1(DATA), 13140);
	}

	#[test]
	fn test_part2() {
		unimplemented!();
	}
}

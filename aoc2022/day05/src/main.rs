use std::collections::VecDeque;
use std::fs;

use clap::Parser;
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, char, line_ending, satisfy, space0, space1, u32};
use nom::combinator::{eof, map};
use nom::multi::{count, many0, many1, many1_count};
use nom::sequence::delimited;
use nom::IResult;

#[derive(Parser, Debug)]
struct Day05 {
	input: String,
}

fn main() {
	let cmd = Day05::parse();

	let input = fs::read_to_string(cmd.input).unwrap();

	let res_9000 = part1(&input);
	println!("CrateMover9000: {res_9000}");

	let res_9001 = part2(&input);
	println!("CrateMover9001: {res_9001}");
}

fn part1(input: &str) -> String {
	let (_, mut puzzle) = parse_input(&input).unwrap();
	puzzle.run_9000();
	puzzle.top_crates()
}

fn part2(input: &str) -> String {
	let (_, mut puzzle) = parse_input(&input).unwrap();
	puzzle.run_9001();
	puzzle.top_crates()
}

struct CrateMover {
	stacks: Vec<VecDeque<char>>,
	moves: Vec<Move>,
}

impl CrateMover {
	fn run_9000(&mut self) {
		for m in &self.moves {
			for _ in 0..m.amount {
				let c = self.stacks[(m.src - 1) as usize].pop_front().unwrap();
				self.stacks[(m.dst - 1) as usize].push_front(c);
			}
		}
	}

	fn run_9001(&mut self) {
		for m in &self.moves {
			debug_assert_ne!(m.src, m.dst);

			let ptr = self.stacks.as_mut_ptr();
			let src = unsafe { &mut *ptr.add((m.src - 1) as usize) };
			let dst = unsafe { &mut *ptr.add((m.dst - 1) as usize) };

			src.drain(..m.amount as usize).rev().for_each(|c| {
				dst.push_front(c);
			})
		}
	}

	fn top_crates(&self) -> String {
		self.stacks
			.iter()
			.filter_map(|stack| stack.front().cloned())
			.join("")
	}
}

fn parse_input(input: &str) -> IResult<&str, CrateMover> {
	let (input, containers) = parse_containers(input)?;
	let (input, nb_stacks) = many1_count(delimited(space0, u32, space0))(input)?;
	let (input, _) = count(line_ending, 2)(input)?;
	let (input, moves) = many1(parse_move)(input)?;
	debug_assert_eq!(containers.len() % nb_stacks, 0);

	let mut stacks = vec![VecDeque::new(); nb_stacks];
	containers
		.iter()
		.chunks(nb_stacks)
		.into_iter()
		.for_each(|it| {
			it.copied().enumerate().for_each(|(i, container)| {
				if let Some(c) = container {
					stacks[i].push_back(c);
				}
			})
		});

	Ok((input, CrateMover { stacks, moves }))
}

fn foo(input: &str) -> IResult<&str, Option<char>> {
	let (input, c) = alt((map(parse_container, Some), map(parse_hole, |_| None)))(input)?;
	let (input, _) = satisfy(|c| c == ' ' || c == '\n')(input)?;
	Ok((input, c))
}

fn parse_containers(input: &str) -> IResult<&str, Vec<Option<char>>> {
	many0(foo)(input)
}

fn parse_container(input: &str) -> IResult<&str, char> {
	delimited(char('['), anychar, char(']'))(input)
}

fn parse_hole(input: &str) -> IResult<&str, &str> {
	tag("   ")(input)
}

#[derive(Debug)]
struct Move {
	amount: u32,
	src: u32,
	dst: u32,
}

fn parse_move(input: &str) -> IResult<&str, Move> {
	let (input, _) = tag("move")(input)?;
	let (input, amount) = delimited(space1, u32, space1)(input)?;
	let (input, _) = tag("from")(input)?;
	let (input, src) = delimited(space1, u32, space1)(input)?;
	let (input, _) = tag("to")(input)?;
	let (input, dst) = delimited(space1, u32, alt((line_ending, eof)))(input)?;

	Ok((input, Move { amount, src, dst }))
}

#[cfg(test)]
mod test {
	use super::{part1, part2};

	const DATA: &str = r#"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"#;

	#[test]
	fn test_part1() {
		assert_eq!(part1(DATA), "CMZ");
	}

	#[test]
	fn test_part2() {
		assert_eq!(part2(DATA), "MCD");
	}
}

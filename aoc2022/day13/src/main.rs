use std::cmp::Ordering;
use std::fs;

use clap::Parser;
use itertools::Itertools;
use nom::branch::alt;
use nom::character::complete::{char, u32};
use nom::combinator::map;
use nom::multi::{many1, separated_list0, separated_list1};
use nom::sequence::delimited;
use nom::IResult;

#[derive(Parser)]
struct Day13 {
	input: String,
}

fn main() {
	let cmd = Day13::parse();

	let input = fs::read_to_string(cmd.input).unwrap();
	let mut packets = parse_input(&input);

	let res1 = part1(&packets);
	println!("{res1}");

	let res2 = part2(&mut packets);
	println!("{res2}");
}

fn part1(packets: &[Packet]) -> u32 {
	(1..)
		.zip(packets.iter().tuples())
		.filter_map(
			|(i, (left, right))| {
				if left < right {
					Some(i)
				} else {
					None
				}
			},
		)
		.sum()
}

fn part2(packets: &mut [Packet]) -> u32 {
	packets.sort_unstable();
	let i = packets
		.binary_search(&vec![Data::List(vec![Data::Int(2)])])
		.unwrap_err() as u32;
	let j = packets
		.binary_search(&vec![Data::List(vec![Data::Int(6)])])
		.unwrap_err() as u32;

	(i + 1) * (j + 2)
}

type Packet = Vec<Data>;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Data {
	List(Vec<Data>),
	Int(u32),
}

impl PartialOrd for Data {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		use Data::*;

		match (self, other) {
			(Int(x), Int(y)) => x.partial_cmp(y),
			(List(xs), List(ys)) => xs.partial_cmp(ys),
			(l @ Int(_), r @ List(_)) => List(vec![l.clone()]).partial_cmp(r),
			(l @ List(_), r @ Int(_)) => l.partial_cmp(&List(vec![r.clone()])),
		}
	}
}

impl Ord for Data {
	fn cmp(&self, other: &Self) -> Ordering {
		use Data::*;

		match (self, other) {
			(Int(x), Int(y)) => x.cmp(y),
			(List(xs), List(ys)) => xs.cmp(ys),
			(l @ Int(_), r @ List(_)) => List(vec![l.clone()]).cmp(r),
			(l @ List(_), r @ Int(_)) => l.cmp(&List(vec![r.clone()])),
		}
	}
}

fn parse_input(input: &str) -> Vec<Packet> {
	separated_list1(many1(char('\n')), parse_packet)(input)
		.unwrap()
		.1
}

fn parse_packet(input: &str) -> IResult<&str, Packet> {
	parse_list(input)
}

fn parse_data(input: &str) -> IResult<&str, Data> {
	alt((map(parse_list, Data::List), map(u32, Data::Int)))(input)
}

fn parse_list(input: &str) -> IResult<&str, Vec<Data>> {
	delimited(char('['), separated_list0(char(','), parse_data), char(']'))(input)
}

#[cfg(test)]
mod test {
	use super::{parse_input, part1, part2};

	const DATA: &str = r#"[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"#;

	#[test]
	fn test_part1() {
		let pairs = parse_input(DATA);
		assert_eq!(part1(&pairs), 13);
	}

	#[test]
	fn test_part2() {
		let mut packets = parse_input(DATA);
		assert_eq!(part2(&mut packets), 140);
	}
}

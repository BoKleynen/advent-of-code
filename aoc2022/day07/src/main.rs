use std::collections::HashMap;
use std::fs;

use clap::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::{char, multispace1, newline, space1, u64};
use nom::combinator::{iterator, map, opt, value};
use nom::sequence::terminated;
use nom::IResult;

#[derive(Parser, Debug)]
struct Day07 {
	input: String,
}

fn main() {
	let cmd = Day07::parse();
	let input = fs::read_to_string(cmd.input).unwrap();
	let directory_sizes = directory_sizes(&input);

	let res1 = part1(&directory_sizes);
	println!("{res1}");

	let res2 = part2(&directory_sizes);
	println!("{res2}");
}

fn part1(dirs: &HashMap<String, u64>) -> u64 {
	dirs.values().filter(|&&size| size <= 100_000).sum()
}

fn part2(dirs: &HashMap<String, u64>) -> u64 {
	const CAPACITY: u64 = 70000000;
	const UPDATE_SIZE: u64 = 30000000;

	let used_space: u64 = dirs[""];
	let unused = CAPACITY - used_space;
	let required = UPDATE_SIZE - unused;

	*dirs
		.values()
		.filter(|&&size| size >= required)
		.min()
		.unwrap()
}

fn directory_sizes(input: &str) -> HashMap<String, u64> {
	let mut path = Vec::new();
	let mut dirs = HashMap::<String, u64>::new();

	let mut it = iterator(input, parse_line);
	it.for_each(|line| match line {
		Line::Command(cmd) => match cmd {
			Cmd::Cd("/") => {
				path.clear();
				path.push("");
			}
			Cmd::Cd("..") => {
				path.pop();
			}
			Cmd::Cd(dir) => path.push(dir),
			Cmd::Ls => (),
		},
		Line::Entry(entry) => match entry {
			Entry::File(size) => {
				(0..path.len())
					.map(|i| path[0..=i].join("/"))
					.for_each(|dir| *dirs.entry(dir).or_default() += size);
			}
			Entry::Dir(_dir) => {}
		},
	});
	debug_assert!(it.finish().unwrap().0.is_empty());
	dirs
}

#[derive(Debug)]
enum Line<'a> {
	Command(Cmd<'a>),
	Entry(Entry<'a>),
}

fn parse_line(input: &str) -> IResult<&str, Line> {
	let (input, res) = alt((map(parse_cmd, Line::Command), map(parse_entry, Line::Entry)))(input)?;
	let (input, _) = opt(newline)(input)?;
	Ok((input, res))
}

#[derive(Debug)]
enum Entry<'a> {
	File(u64),
	Dir(&'a str),
}

fn parse_entry(input: &str) -> IResult<&str, Entry> {
	alt((map(parse_file, Entry::File), map(parse_dir, Entry::Dir)))(input)
}

fn parse_file(input: &str) -> IResult<&str, u64> {
	let (input, size) = u64(input)?;
	let (input, _) = take_till1(|c| c == '\n')(input)?;
	Ok((input, size))
}

fn parse_dir(input: &str) -> IResult<&str, &str> {
	let (input, _) = terminated(tag("dir"), space1)(input)?;
	let (input, dir) = take_till1(|c| c == '\n')(input)?;
	Ok((input, dir))
}

#[derive(Debug)]
enum Cmd<'a> {
	Ls,
	Cd(&'a str),
}

fn parse_cmd(input: &str) -> IResult<&str, Cmd> {
	let (input, _) = char('$')(input)?;
	let (input, _) = space1(input)?;
	alt((map(parse_cd, Cmd::Cd), map(parse_ls, |_| Cmd::Ls)))(input)
}

fn parse_cd(input: &str) -> IResult<&str, &str> {
	let (input, _) = tag("cd")(input)?;
	let (input, _) = multispace1(input)?;
	let (input, dir) = take_till1(|c| c == '\n')(input)?;
	Ok((input, dir))
}

fn parse_ls(input: &str) -> IResult<&str, ()> {
	value((), tag("ls"))(input)
}

#[cfg(test)]
mod test {
	use crate::{directory_sizes, part1, part2};

	const DATA: &str = r#"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"#;

	#[test]
	fn test_part1() {
		assert_eq!(part1(&directory_sizes(DATA)), 95437);
	}

	#[test]
	fn test_part2() {
		assert_eq!(part2(&directory_sizes(DATA)), 24933642);
	}
}

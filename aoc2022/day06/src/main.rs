use std::fs;

use clap::Parser;

#[derive(Parser, Debug)]
struct Day06 {
	input: String,
}

fn main() {
	let cmd = Day06::parse();
	let input = fs::read_to_string(cmd.input).unwrap();
	let packet_start = part1(&input);
	let message_start = part2(&input);
	println!("start-of-packet: {packet_start}");
	println!("start-of-message: {message_start}");
}

fn part1(input: &str) -> usize {
	first_n_distinct(input, 4).unwrap()
}

fn part2(input: &str) -> usize {
	first_n_distinct(input, 14).unwrap()
}

fn first_n_distinct(input: &str, n: usize) -> Option<usize> {
	input
		.as_bytes()
		.windows(n)
		.enumerate()
		.find_map(|(i, chars)| {
			let has_duplicates = chars
				.iter()
				.enumerate()
				.any(|(j, c)| chars[..j].contains(c));
			if has_duplicates {
				None
			} else {
				Some(i + n)
			}
		})
}

#[cfg(test)]
mod test {
	use super::{part1, part2};

	#[test]
	fn test_part1() {
		assert_eq!(part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
		assert_eq!(part1("nppdvjthqldpwncqszvftbrmjlhg"), 6);
		assert_eq!(part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10);
		assert_eq!(part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11);
	}

	#[test]
	fn test_part2() {
		assert_eq!(part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
		assert_eq!(part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23);
		assert_eq!(part2("nppdvjthqldpwncqszvftbrmjlhg"), 23);
		assert_eq!(part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29);
		assert_eq!(part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26);
	}
}

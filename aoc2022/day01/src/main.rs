use clap::Parser;
use std::fs;
use std::vec;

#[derive(Parser, Debug)]
struct Day01 {
	input: String,

	#[arg(long, default_value_t = 1)]
	top: usize,
}

fn main() {
	let cmd = Day01::parse();

	let s = fs::read_to_string(cmd.input).unwrap();
	let res = sum_top_calories(&s, cmd.top);
	println!("{res}");
}

fn sum_top_calories(s: &str, k: usize) -> u32 {
	let mut top_calories = vec![0; k];
	let mut temp = 0;
	for line in s.lines() {
		if line.is_empty() {
			if let Some(index) = top_calories.iter().position(|&calories| temp > calories) {
				top_calories.pop();
				top_calories.insert(index, temp)
			}
			temp = 0;
		} else {
			temp += line.parse::<u32>().unwrap()
		}
	}

	top_calories.iter().sum()
}

#[cfg(test)]
mod test {
	use super::sum_top_calories;

	const DATA: &str = r#"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"#;

	#[test]
	fn test_part1() {
		assert_eq!(sum_top_calories(DATA, 1), 24000);
	}

	#[test]
	fn test_part2() {
		assert_eq!(sum_top_calories(DATA, 3), 45000);
	}
}

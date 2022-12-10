use std::fs;

use clap::Parser;

#[derive(Parser)]
struct Day08 {
	input: String,
}

fn main() {
	let cmd = Day08::parse();
	let input = fs::read_to_string(cmd.input).unwrap();
	let forest = parse_forest(&input);

	let res1 = part1(&forest);
	println!("{res1}");

	let res2 = part1(&forest);
	println!("{res2}");
}

// TODO: can this function be written more concisely using iterators?
fn part1(forest: &Forest) -> usize {
    let mut visible = vec![false; forest.trees.len()];
    let mut heighest_y = vec![-1; forest.width];

    for i in 0..forest.height {
        let mut heighest_x = -1;

        for j in 0..forest.width {
			let idx = i * forest.width + j;
			let height = forest.trees[idx];

			if height > heighest_x {
				visible[idx] = true;
				heighest_x = height;
			}

            if height > heighest_y[j] {
				visible[idx] = true;
                heighest_y[j] = height;
            }
        }
    }

    let mut heighest_y = vec![-1; forest.width];
    for i in (0..forest.height).rev() {
        let mut heighest_x = -1;

        for j in (0..forest.width).rev() {
			let idx = i * forest.width + j;
			let height = forest.trees[idx];

			if height > heighest_x {
				visible[idx] = true;
				heighest_x = height;
			}

            if height > heighest_y[j] {
				visible[idx] = true;
                heighest_y[j] = height;
            }
        }
    }

	visible.iter().filter(|&&visible| visible).count()
}

fn part2(forest: &Forest) -> u64 {
    let mut max_score = 0;

    for i in 0..forest.height {
        for j in 0..forest.width {
			let height = forest.trees[i * forest.width + j];
            let righ = (j+1..forest.width).map(|k| forest.trees[i * forest.width + k]).take_while(|&h| h < height).count();
        }
    }

    max_score
}

struct Forest {
	trees: Vec<i8>,
	width: usize,
	height: usize,
}

fn parse_forest(input: &str) -> Forest {
	let trees = input
		.as_bytes()
		.iter()
		.copied()
		.filter(|&c| c != '\n' as u8)
		.map(|x| x - '0' as u8)
		.map(|c| c as i8)
		.collect::<Vec<_>>();
	let width = input
		.as_bytes()
		.iter()
		.take_while(|&&c| c != '\n' as u8)
		.count();
	let height = trees.len() / width;
	debug_assert_eq!(width * height, trees.len());

	Forest {
		trees,
		width,
		height,
	}
}

#[cfg(test)]
mod test {
	use super::{parse_forest, part1, part2};

	const DATA: &str = r#"30373
25512
65332
33549
35390"#;

	#[test]
	fn test_part1() {
		assert_eq!(part1(&parse_forest(DATA)), 21);
	}

	#[test]
	#[ignore = "unimplemented"]
	fn test_part2() {
		assert_eq!(part2(&parse_forest(DATA)), 21);
	}
}

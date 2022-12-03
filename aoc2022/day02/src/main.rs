use clap::Parser;
use std::fs;

#[derive(Parser, Debug)]
struct Day02 {
	input: String,

	#[arg(long, default_value_t = false)]
	outcome: bool,
}

fn main() {
	let cmd = Day02::parse();

	let s = fs::read_to_string(cmd.input).unwrap();

	let points = if cmd.outcome {
		challange2(&s)
	} else {
		challange1(&s)
	};

	println!("{points}");
}

fn challange1(s: &str) -> u32 {
	s.lines()
		.map(|line| {
			let opponent = Move::parse_oponnent(&line[0..1]);
			let own = Move::parse_own(&line[2..3]);
			let game_result = play_game(own, opponent);

			game_result.points() + own.points()
		})
		.sum()
}

fn challange2(s: &str) -> u32 {
	s.lines()
		.map(|line| {
			let opponent = Move::parse_oponnent(&line[0..1]);
			let game_result = GameResult::decrypt_outcome(&line[2..3]);
			let own = calculate_move(opponent, game_result);

			game_result.points() + own.points()
		})
		.sum()
}

fn play_game(you: Move, opponent: Move) -> GameResult {
	match (you, opponent) {
		(Move::Rock, Move::Scissors) => GameResult::Win,
		(Move::Paper, Move::Rock) => GameResult::Win,
		(Move::Scissors, Move::Paper) => GameResult::Win,
		(x, y) => {
			if x == y {
				GameResult::Draw
			} else {
				GameResult::Lose
			}
		}
	}
}

fn calculate_move(opponent: Move, outcome: GameResult) -> Move {
	match outcome {
		GameResult::Draw => opponent,
		GameResult::Win => opponent.winning_move(),
		GameResult::Lose => opponent.losing_move(),
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Move {
	Rock,
	Paper,
	Scissors,
}

impl Move {
	fn points(self) -> u32 {
		match self {
			Move::Rock => 1,
			Move::Paper => 2,
			Move::Scissors => 3,
		}
	}

	fn parse_own(s: &str) -> Self {
		match s {
			"X" => Move::Rock,
			"Y" => Move::Paper,
			"Z" => Move::Scissors,
			_ => unreachable!(),
		}
	}

	fn parse_oponnent(s: &str) -> Self {
		match s {
			"A" => Move::Rock,
			"B" => Move::Paper,
			"C" => Move::Scissors,
			_ => unreachable!(),
		}
	}

	fn winning_move(&self) -> Self {
		match self {
			Move::Rock => Move::Paper,
			Move::Paper => Move::Scissors,
			Move::Scissors => Move::Rock,
		}
	}

	fn losing_move(&self) -> Self {
		match self {
			Move::Rock => Move::Scissors,
			Move::Paper => Move::Rock,
			Move::Scissors => Move::Paper,
		}
	}
}

#[derive(Clone, Copy)]
enum GameResult {
	Win,
	Lose,
	Draw,
}

impl GameResult {
	fn points(self) -> u32 {
		match self {
			GameResult::Win => 6,
			GameResult::Lose => 0,
			GameResult::Draw => 3,
		}
	}

	fn decrypt_outcome(s: &str) -> GameResult {
		match s {
			"X" => GameResult::Lose,
			"Y" => GameResult::Draw,
			"Z" => GameResult::Win,
			_ => unreachable!(),
		}
	}
}

#[cfg(test)]
mod test {
	use crate::{challange1, challange2};

	#[test]
	fn test_part1() {
		let data = r#"A Y
B X
C Z"#;

		assert_eq!(challange1(data), 15);
	}

	#[test]
	fn test_part2() {
		let data = r#"A Y
B X
C Z"#;

		assert_eq!(challange2(data), 12);
	}
}

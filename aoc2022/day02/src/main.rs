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
		part2(&s)
	} else {
		part1(&s)
	};

	println!("{points}");
}

fn part1(s: &str) -> u32 {
	s.lines()
		.map(|line| {
			let opponent = Move::parse_opponent(&line[0..1]);
			let own = Move::parse_own(&line[2..3]);
			let game_result = play_game(own, opponent);

			game_result.points() + own.points()
		})
		.sum()
}

fn part2(s: &str) -> u32 {
	s.lines()
		.map(|line| {
			let opponent = Move::parse_opponent(&line[0..1]);
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
		(x, y) if x == y => GameResult::Draw,
		_ => GameResult::Lose,
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

	fn parse_opponent(s: &str) -> Self {
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
	use crate::{part1, part2};

	const DATA: &str = r#"A Y
B X
C Z"#;

	#[test]
	fn test_part1() {
		assert_eq!(part1(DATA), 15);
	}

	#[test]
	fn test_part2() {
		assert_eq!(part2(DATA), 12);
	}
}

fn parse(lines: impl IntoIterator<Item = String>) -> (Vec<Vec<char>>, usize) {
    let parsed: Vec<Vec<char>> = lines
        .into_iter()
        .map(|s| s.chars().collect())
        .collect();
    let width: usize = parsed[0].len();
    (parsed, width)
}

fn part1((v, width): (Vec<Vec<char>>, usize)) -> usize {
    let mut splits: usize = 0;
    let start_pos: usize = v[0].iter().position(|c| *c == 'S').unwrap();
    let mut positions: Vec<usize> = vec![start_pos];
    for line in &v[1..] {
        let mut new_positions: Vec<usize> = Vec::new();
        for pos in positions.iter() {
            match line[*pos] {
                '.' => new_positions.push(*pos),
                '^' => {
                    new_positions.push(*pos - 1);
                    splits += 1;
                    if *pos + 2 < width && line[*pos + 2] != '^' {
                        new_positions.push(*pos + 1);
                    }
                }
                _ => unreachable!("Parse error."),
            }
        }
        positions = new_positions;
        positions.dedup();
    }
    splits
}

fn part2((v, width): (Vec<Vec<char>>, usize)) -> usize {
    let mut splits: usize = 0;
    let start_pos: usize = v[0].iter().position(|c| *c == 'S').unwrap();
    let mut positions: Vec<(usize, u64)> = vec![start_pos];
    for line in &v[1..] {
        let mut new_positions: Vec<(usize, u64)> = Vec::new();
        for (pos, previous) in positions.iter() {
            match line[*pos] {
                '.' => new_positions.push((*pos, *previous)),
                '^' => {
                    new_positions.push(*pos - 1);
                    splits += 1;
                    if *pos + 2 < width {
                        splits += 1;
                        if line[*pos + 2] != '^' {
                            new_positions.push(*pos + 1);
                        }
                    }
                }
                _ => unreachable!("Parse error."),
            }
        }
        positions = new_positions;
        positions.dedup();
    }
    splits
}

fn main() -> std::io::Result<()> {
    let lines = std::io::stdin()
        .lines()
        .collect::<std::io::Result<Vec<String>>>()?;
    println!("{}", part1(parse(lines.clone())));
    println!("{}", part2(parse(lines.clone())));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_part1() {
        let input = String::from(".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............");
        let lines = input.lines().map(ToOwned::to_owned);
        let parsed = parse(lines);
        let solved = part1(parsed);
        assert_eq!(21, solved);
    }

    #[test]
    fn example_part2() {
        let input = String::from(".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............");
        let lines = input.lines().map(ToOwned::to_owned);
        let parsed = parse(lines);
        let solved = part2(parsed);
        assert_eq!(40, solved);
    }
}
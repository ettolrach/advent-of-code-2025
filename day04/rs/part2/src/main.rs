#[derive(Debug, Clone)]
struct Grid {
    letters: Vec<char>,
    width: usize,
    height: usize,
}
impl Grid {
    fn from_vec(lines: Vec<Vec<char>>) -> Grid {
        assert!(!lines.is_empty());
        let width: usize = lines[0].len();
        let mut letters: Vec<char> = Vec::new();
        for l in lines {
            assert!(l.len() == width);
            letters.extend(l.iter());
        }
        Grid {
            height: letters.len() / width,
            letters,
            width,
        }
    }

    fn get_coord(&self, index: usize) -> [usize; 2] {
        [index % self.width, index / self.width]
    }
    fn get_index(&self, coordinate: [usize; 2]) -> usize {
        coordinate[0] + self.width * coordinate[1]
    }

    fn in_bounds_coord(&self, [a, b]: [isize; 2]) -> bool {
        a >= 0 && b >= 0 && a < self.width as isize && b < self.height as isize
    }

    fn neighbours(&self, index: usize) -> Vec<usize> {
        let [x, y] = self.get_coord(index).map(|n| n as isize);
        let neighbours: [[isize; 2]; 8] = [
            [x + 1, y],
            [x + 1, y + 1],
            [x, y + 1],
            [x - 1, y + 1],
            [x - 1, y],
            [x - 1, y - 1],
            [x, y - 1],
            [x + 1, y - 1],
        ];
        neighbours.iter()
            .filter(|&&neighbour| self.in_bounds_coord(neighbour))
            .map(|&coord| self.get_index(coord.map(|i| i as usize)))
            .collect()
    }
}

fn get_indices(grid: &Grid) -> Vec<usize> {
    (0..grid.letters.len())
        .filter(|i| grid.letters[*i] == '@')
        .map(|i| {
            (i, grid.neighbours(i)
                .into_iter()
                .filter(|neighbour| grid.letters[*neighbour] == '@')
                .count())
        })
        .filter(|(_, n)| *n < 4)
        .map(|tuple| tuple.0)
        .collect()
}

fn solve(v: Vec<String>) -> String {
    let mut grid = Grid::from_vec(v.into_iter().map(|line| line.chars().collect()).collect());
    let mut to_remove: Vec<usize>;
    let mut moved: usize = 0;
    while { to_remove = get_indices(&grid); !to_remove.is_empty() } {
        moved += to_remove.len();
        for i in to_remove {
            grid.letters[i] = '.';
        }
    }
    moved.to_string()
}

fn main() -> std::io::Result<()> {
    let lines = std::io::stdin()
        .lines()
        .collect::<std::io::Result<Vec<String>>>()?;
    println!("{}", solve(lines));
    Ok(())
}

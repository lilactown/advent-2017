let abs = IntUtils.abs;

/*
  Desired output:
  1 => (0, 0)
  2 => (1, 0)
  3 => (1, 1)
  4 => (0, 1)
  5 => (-1, 1)
  6 => (-1, 0)
  7 => (-1, -1)
  8 => (0, -1)
  9 => (1, -1)
 */
module Part1 = {
  type input = int;
  type answer = int;
  let cases = [(1, 0), (12, 3), (23, 2), (1024, 31)];
  let spiral = (n) => {
    let rec next = (x, y, count) => {
      /* based off https://stackoverflow.com/a/31864777/4379329
         the direction is wrong but the distance should stay the same */
      let finished = count == n;
      let pivot = abs(x) <= abs(y) && (x != y || x >= 0);
      switch (finished, pivot) {
      | (true, _) => (x, y)
      | (_, true) =>
        let x' = x + (y >= 0 ? 1 : (-1));
        next(x', y, count + 1)
      | (_, false) =>
        let y' = y + (x >= 0 ? (-1) : 1);
        next(x, y', count + 1)
      }
    };
    next(0, 0, 1)
  };
  let distance = ((x, y)) => abs(x) + abs(y);
  let solve = (n) => distance(spiral(n));
};

/**
 * For part two, I created an actual grid. I allow
 * coordinates to be placed inside of it for ease of
 * printing.
 */
type element =
  | Coordinates(int, int)
  | Number(int);

type grid = array(array(element));

let makeGrid: int => grid =
  (n) => {
    let d = n / 2;
    Array.init(n, (y) => Array.init(n, (x) => Coordinates(x - d, - y + d)))
  };

let get = (grid: grid, d, x, y) =>
  switch grid[- (y - d)][x + d] {
  | Coordinates(_, _) => 0
  | exception (Invalid_argument(_)) => 0
  | Number(el) => el
  };

let set = (grid: grid, d, x, y, el) => grid[- (y - d)][x + d] = el;

let spiralNeighbors = (magicNumber, n) => {
  /* Make a grid of some magic size */
  let grid = makeGrid(magicNumber);
  let get = get(grid, magicNumber / 2);
  let set = set(grid, magicNumber / 2);
  set(0, 0, Number(1));
  let rec next = (x, y) => {
    /* based off https://stackoverflow.com/a/31864777/4379329
       the direction is wrong but the distance should stay the same */
    let pivot = abs(x) <= abs(y) && (x != y || x >= 0);
    /* get all neighbors */
    let n1 = get(x + 1, y + 0);
    let n2 = get(x + 1, y + 1);
    let n3 = get(x + 0, y + 1);
    let n4 = get(x - 1, y + 1);
    let n5 = get(x - 1, y + 0);
    let n6 = get(x - 1, y - 1);
    let n7 = get(x + 0, y - 1);
    let n8 = get(x + 1, y - 1);
    let sum = n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8;
    let finished = sum > n || x == magicNumber / 2 && y == magicNumber / 2;
    set(x, y, Number(sum));
    switch (finished, pivot) {
    | (true, _) => sum
    | (_, true) =>
      let x' = x + (y >= 0 ? 1 : (-1));
      next(x', y)
    | (_, false) =>
      let y' = y + (x >= 0 ? (-1) : 1);
      next(x, y')
    }
  };
  next(1, 0)
};

module Part2 = {
  type input = int;
  type answer = int;
  let cases = [(1, 2), (2, 4), (11, 23), (30, 54), (150, 304)];
  let solve = spiralNeighbors(11);
};

let part1 = Part1.solve;

let part2 = Part2.solve;
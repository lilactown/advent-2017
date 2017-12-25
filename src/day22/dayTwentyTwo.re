module InfiniteGrid = {
  include Grid;
  let get = (grid: Grid.t('a), ~x, ~y, ~default: 'a) =>
    switch grid[y][x] {
    | v => (v, x, y, grid)
    | exception _ =>
      let size = Grid.size(grid);
      let xGrow =
        if (x < 0) {
          - x;
        } else {
          x - size;
        };
      let yGrow =
        if (y < 0) {
          - y;
        } else {
          y - size;
        };
      let padAmount = max(xGrow, yGrow);
      let newX = x + padAmount;
      let newY = y + padAmount;
      Js.log3(padAmount, newX, newY);
      (
        default,
        newX,
        newY,
        /* ArrayUtils.padBothF( */
        /*   ~padAmount, */
        /*   ~f=(_) => Array.make(size + padAmount * 2, default), */
        Array.map(ArrayUtils.padBoth(~padAmount, ~withEl=default), grid)
        /* ) */
      );
    };
};

module Virus = {
  type direction =
    | North
    | South
    | East
    | West;
  type turn =
    | Left
    | Right;
  type t = {
    cluster: Grid.t(bool),
    pos: (int, int),
    direction,
    infected: int
  };
  let directionToString = d =>
    switch d {
    | North => "North"
    | South => "South"
    | East => "East"
    | West => "West"
    };
  let burst = (virus: t) : t => {
    let (x, y) = virus.pos;
    let (current, x, y, cluster) =
      InfiniteGrid.get(virus.cluster, ~x, ~y, ~default=false);
    let turn = current ? Right : Left;
    let direction =
      switch (virus.direction, turn) {
      | (North, Left) => West
      | (North, Right) => East
      | (South, Left) => East
      | (South, Right) => West
      | (East, Left) => North
      | (East, Right) => South
      | (West, Left) => South
      | (West, Right) => North
      };
    cluster[y][x] = ! current;
    let infected = current ? virus.infected : virus.infected + 1;
    let pos =
      switch direction {
      | North => (x, y - 1)
      | South => (x, y + 1)
      | East => (x + 1, y)
      | West => (x - 1, y)
      };
    {cluster, pos, direction, infected};
  };
  let make = grid : t => {
    let cluster = grid;
    let pos = ((Grid.size(grid) - 1) / 2, (Grid.size(grid) - 1) / 2);
    Js.log(pos);
    let direction = North;
    let infected = 0;
    {cluster, pos, direction, infected};
  };
  let print = virus =>
    Js.log(
      Grid.toString(virus.cluster, ~f=(x', y', b) =>
        switch (x' == fst(virus.pos) && y' == snd(virus.pos), b) {
        | (true, true) => "[#]"
        | (true, false) => "[.]"
        | (false, true) => " # "
        | (false, false) => " . "
        }
      )
    );
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|..#
#..
...|}, 5587)];
  let solve = input => {
    let seed =
      StringUtils.splitWith("\n", input)
      |> Array.map(StringUtils.split)
      |> Array.map(
           Array.map(k =>
             switch k {
             | "#" => true
             | "." => false
             | _ => raise(Failure("Could not parse"))
             }
           )
         );
    let virus = ref(Virus.make(seed));
    for (_ in 1 to 7) {
      Virus.print(virus^);
      virus := Virus.burst(virus^);
      Js.log(Virus.directionToString(virus^.direction));
    };
    virus^.infected;
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = input => 6;
};

let part1 = Part1.solve;

let part2 = Part2.solve;

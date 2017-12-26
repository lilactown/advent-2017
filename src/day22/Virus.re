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

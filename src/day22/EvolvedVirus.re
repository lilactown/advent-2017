type direction =
  | North
  | South
  | East
  | West;

type directionChange =
  | Left
  | Right
  | Reverse
  | None;

type node =
  | Clean
  | Weakened
  | Infected
  | Flagged;

type t = {
  cluster: Grid.t(node),
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
    InfiniteGrid.get(virus.cluster, ~x, ~y, ~default=Clean);
  let change =
    switch current {
    | Clean => Left
    | Weakened => None
    | Infected => Right
    | Flagged => Reverse
    };
  let direction =
    switch (virus.direction, change) {
    | (d, None) => d
    | (North, Left) => West
    | (North, Right) => East
    | (North, Reverse) => South
    | (South, Left) => East
    | (South, Right) => West
    | (South, Reverse) => North
    | (East, Left) => North
    | (East, Right) => South
    | (East, Reverse) => West
    | (West, Left) => South
    | (West, Right) => North
    | (West, Reverse) => East
    };
  cluster[y][x] = (
    switch current {
    | Clean => Weakened
    | Weakened => Infected
    | Infected => Flagged
    | Flagged => Clean
    }
  );
  let infected =
    switch current {
    | Weakened => virus.infected + 1 /*we're current infecting */
    | _ => virus.infected
    };
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

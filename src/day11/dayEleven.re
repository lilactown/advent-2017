let abs = IntUtils.abs;

let cardinalToCoord = (dir) =>
  switch dir {
  | "n" => (1, (-1), 0)
  | "s" => ((-1), 1, 0)
  | "ne" => (0, (-1), 1)
  | "nw" => (1, 0, (-1))
  | "se" => ((-1), 0, 1)
  | "sw" => (0, 1, (-1))
  | _ => raise(Failure("Invalid direction"))
  };

let distanceFromOrigin = (x, y, z) => (abs(x) + abs(y) + abs(z)) / 2;

let move = ((x, y, z, max), (dx, dy, dz)) => (
  x + dx,
  y + dy,
  z + dz,
  {
    let distance = distanceFromOrigin(x + dx, y + dy, z + dz);
    if (max > distance) {
      max
    } else {
      distance
    }
  }
);

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("ne,ne,ne", 3), ("ne,ne,sw,sw", 0), ("ne,ne,s,s", 2), ("se,sw,se,sw,sw", 3)];
  let solve = (input) => {
    let (x, y, z, _) =
      Js.String.split(",", input)
      |> Array.map(cardinalToCoord)
      |> Array.fold_left(move, (0, 0, 0, 0));
    distanceFromOrigin(x, y, z)
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("ne,ne,ne", 3), ("ne,ne,sw,sw", 2), ("ne,ne,s,s", 2), ("se,sw,se,sw,sw", 3)];
  /**/
  let solve = (input) => {
    let (_, _, _, max) =
      Js.String.split(",", input)
      |> Array.map(cardinalToCoord)
      |> Array.fold_left(move, (0, 0, 0, 0));
    max
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
/* type vCardinal =
     | North
     | South;

   type hCardinal =
     | East
     | West
     | None;

   type cardinal = (vCardinal, hCardinal); */
let abs = Js.Math.abs_int;

let directionToInt = (dir) =>
  switch dir {
  | "n" => (1, 0)
  | "s" => ((-1), 0)
  | "ne" => (1, 1)
  | "nw" => (1, (-1))
  | "se" => ((-1), 1)
  | "sw" => ((-1), (-1))
  | _ => raise(Failure("Invalid direction"))
  };

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("ne,ne,ne", 3), ("ne,ne,sw,sw", 0), ("ne,ne,s,s", 2), ("se,sw,se,sw,sw", 3)];
  let solve = (input) => {
    let (vMoves, hMoves) =
      Js.String.split(",", input)
      |> Array.map(directionToInt)
      |> Array.fold_left(((vTotal, hTotal), (v, h)) => (vTotal + v, hTotal + h), (0, 0));
    Js.log((vMoves, hMoves, abs(vMoves) + abs(abs(vMoves) - abs(hMoves))));
    abs(vMoves) + abs(abs(vMoves) - abs(hMoves))
  };
};
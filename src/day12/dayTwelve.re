/* `Graph` implementation in graph.re */
module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5|}, 6)];
  let solve = (input) => {
    let g =
      Js.String.split("\n", input)
      |> Array.map(Js.String.split(" <-> "))
      |> Array.map(
           (program) =>
             switch program {
             | [|name, connected|] => (name, Js.String.split(", ", connected))
             | _ => raise(Failure("Could not parse programs"))
             }
         )
      |> Graph.fromArray;
    Graph.size(Graph.findAllConnected("0", g))
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5|}, 2)];
  module GraphSet = Set.Make(Graph);
  let solve = (input) => {
    let g =
      Js.String.split("\n", input)
      |> Array.map(Js.String.split(" <-> "))
      |> Array.map(
           (program) =>
             switch program {
             | [|name, connected|] => (name, Js.String.split(", ", connected))
             | _ => raise(Failure("Could not parse programs"))
             }
         )
      |> Graph.fromArray;
    Graph.elements(g)
    |> Array.map((v) => Graph.findAllConnected(v, g))
    |> Array.to_list
    |> GraphSet.of_list
    |> GraphSet.cardinal
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
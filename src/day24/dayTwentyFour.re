module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10|}, 31)];
  let partToTuple = s => {
    let ss = StringUtils.splitWith("/", s);
    switch ss {
    | [|s0, s1|] => (s0, s1)
    | _ => raise(Failure("partToTuple called with invalid arg: " ++ s))
    };
  };
  let swapSides = s => {
    let (s0, s1) = partToTuple(s);
    Js.Array.joinWith("/", [|s1, s0|]);
  };
  let solve = input => {
    let parts =
      StringUtils.splitWith("\n", input)
      |> Array.fold_left(
           (newArr, part) => Array.append(newArr, [|part, swapSides(part)|]),
           [||]
         );
    let bridgeGraph =
      Array.map(
        part => {
          let (in0, out0) =
            ArrayUtils.toTuple(StringUtils.splitWith("/", part));
          let connections =
            ArrayUtils.filter(
              ~f=
                c => {
                  let (in1, out1) =
                    ArrayUtils.toTuple(StringUtils.splitWith("/", c));
                  c != part && c != swapSides(part) && in1 == out0;
                },
              parts
            );
          (part, connections);
        },
        parts
      )
      |> Graph.fromArray;
    Js.log(bridgeGraph);
    Js.log(Graph.sort(bridgeGraph) |> Array.of_list);
    7;
  };
  /* let solve = input => { */
  /*   let parts = */
  /*     StringUtils.splitWith("\n", input) */
  /*     |> Array.fold_left( */
  /*          (newArr, part) => Array.append(newArr, [|part, swapSides(part)|]), */
  /*          [||] */
  /*        ); */
  /*   let bridgeGraph = */
  /*     Array.map( */
  /*       part => { */
  /*         let (_in0, out0) = */
  /*           ArrayUtils.toTuple(StringUtils.splitWith("/", part)); */
  /*         let connections = */
  /*           ArrayUtils.filter( */
  /*             ~f= */
  /*               c => { */
  /*                 let (in1, _out1) = */
  /*                   ArrayUtils.toTuple(StringUtils.splitWith("/", c)); */
  /*                 c != part && c != swapSides(part) && in1 == out0; */
  /*               }, */
  /*             parts */
  /*           ); */
  /*         (part, connections); */
  /*       }, */
  /*       parts */
  /*     ) */
  /*     |> Graph.fromArray; */
  /*   /\* Js.log(bridgeGraph); *\/ */
  /*   let bridges = */
  /*     ArrayUtils.filter(~f=p => p.[0] == '0', parts) */
  /*     |> Array.map( */
  /*          Graph.paths( */
  /*            ~seen=(a, b) => a == b || a == swapSides(b), */
  /*            bridgeGraph */
  /*          ) */
  /*        ) */
  /*     |> ArrayUtils.concat; */
  /*   /\* Js.log(bridges); *\/ */
  /*   let weights = */
  /*     Array.map(Array.map(partToTuple), bridges) */
  /*     |> Array.map( */
  /*          Array.map(((s0, s1)) => (int_of_string(s0), int_of_string(s1))) */
  /*        ) */
  /*     |> Array.map(Array.fold_left((t, (p0, p1)) => t + p0 + p1, 0)); */
  /*   Js.log(weights); */
  /*   IntUtils.max(weights); */
  /* }; */
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = input => 5;
};

let part1 = Part1.solve;

let part2 = Part2.solve;

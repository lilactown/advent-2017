let countSeverity = (firewall) => {
  let severity = ref(0);
  let firewall' = ref(firewall);
  for (depth in 0 to Firewall.size(firewall) - 1) {
    let currentLayer = Firewall.getLayer(firewall'^, depth);
    if (currentLayer.range > 0) {
      if (currentLayer.scanner == 0) {
        severity := severity^ + depth * currentLayer.range
      }
    };
    firewall' := Firewall.tick(firewall'^)
  };
  severity^
};

let isCaught = (~tickOffset=0, firewall) => {
  let rec detect = (tick, depth, firewall) => {
    /* Js.log(Firewall.print(~packet=(depth, 0), firewall)); */
    let currentLayer = Firewall.getLayer(firewall, depth);
    switch (
      currentLayer.range > 0,
      currentLayer.scanner == 0,
      depth == Firewall.size(firewall) - 1
    ) {
    | (false, _, false) => detect(tick + 1, depth + 1, Firewall.tickMany(tick + 1, firewall))
    | (false, _, true) => false
    | (true, false, false) => detect(tick + 1, depth + 1, Firewall.tickMany(tick + 1, firewall))
    | (true, true, _) => true
    | (_, false, true) => false
    }
  };
  let firewall' = Firewall.tickMany(tickOffset, firewall);
  detect(tickOffset, 0, firewall')
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("0: 3\n1: 2\n4: 4\n6: 4", 24)];
  let solve = (input) =>
    input
    |> Js.String.split("\n")
    |> Array.map(Js.String.split(": "))
    |> Array.map(
         (line) =>
           switch line {
           | [|depth, range|] => (int_of_string(depth), int_of_string(range))
           | _ => raise(Failure("Could not parse firewall"))
           }
       )
    |> Firewall.ofArray
    |> countSeverity;
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("0: 3\n1: 2\n4: 4\n6: 4", 10)];
  let solve = (input) => {
    let firewall =
      input
      |> Js.String.split("\n")
      |> Array.map(Js.String.split(": "))
      |> Array.map(
           (line) =>
             switch line {
             | [|depth, range|] => (int_of_string(depth), int_of_string(range))
             | _ => raise(Failure("Could not parse firewall"))
             }
         )
      |> Firewall.ofArray;
    let time = ref(0);
    while (isCaught(~tickOffset=time^, firewall)) {
      time := time^ + 1
    };
    time^
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
let max = (numbers) => {
  let (max, index, _) =
    Array.fold_left(
      ((curMax, lastMaxIndex, index), n: int) =>
        curMax >= n ? (curMax, lastMaxIndex, index + 1) : (n, index, index + 1),
      (numbers[0], 0, 0),
      numbers
    );
  (max, index)
};

module Firewall = {
  module Layer = {
    type direction =
      | Up
      | Down;
    type t = {
      range: int,
      scanner: int,
      initial: int,
      scannerDirection: direction
    };
    let make = (range, initial) => {range, scanner: initial, initial, scannerDirection: Down};
    let tick = ({range, initial, scanner, scannerDirection}: t) =>
      if (range > 0) {
        switch (scannerDirection, scanner + 1 > range - 1, scanner - 1 < 0) {
        | (Down, false, _) => {
            range,
            initial,
            scanner: (scanner + 1) mod range,
            scannerDirection: Down
          }
        | (Down, true, _) => {
            range,
            initial,
            scanner: (scanner - 1) mod range,
            scannerDirection: Up
          }
        | (Up, _, true) => {
            range,
            initial,
            scanner: (scanner + 1) mod range,
            scannerDirection: Down
          }
        | (Up, _, false) => {
            range,
            initial,
            scanner: (scanner - 1) mod range,
            scannerDirection: Up
          }
        }
      } else {
        {range, initial, scanner, scannerDirection}
      };
    let tickMany = (amount, layer) => {
      let layer' = ref(layer);
      for (_ in 0 to amount - 1) {
        layer' := tick(layer'^)
      };
      layer'^
    };
  };
  type t = array(Layer.t);
  let ofArray = (array) : t => {
    let size = fst(array[Array.length(array) - 1]) + 1;
    let firewall = Array.make(size, Layer.make(0, 0));
    Array.iter(((layer, range)) => firewall[layer] = Layer.make(range, 0), array);
    firewall
  };
  let map: (Layer.t => Layer.t, t) => t = Array.map;
  external toArray : t => array(Layer.t) = "%identity";
  let size: t => int = Array.length;
  let getLayer = (firewall: t, layer) => firewall[layer];
  let tick = map(Layer.tick);
  let tickMany = (amount) => map(Layer.tickMany(amount));
  let maxRange = (firewall) => fst(max(Array.map((layer: Layer.t) => layer.range, firewall)));
  let print = (~packet=?, firewall) => {
    let size = size(firewall);
    let maxRange = maxRange(firewall);
    let packet =
      switch packet {
      | Some(p) => p
      | None => ((-1), (-1))
      };
    let out = ref(Array.init(size, (i) => "  " ++ string_of_int(i)) |> Js.Array.joinWith(" "));
    for (range in 0 to maxRange) {
      out :=
        out^
        ++ "\n "
        ++ (
          Array.mapi(
            (i, layer: Layer.t) =>
              switch (
                layer.range == 0,
                layer.range - 1 < range,
                layer.scanner == range,
                packet == (i, range)
              ) {
              | (true, true, true, false) => "..."
              | (true, true, true, true) => "(.)"
              | (_, true, false, _) => "   "
              | (false, false, true, false) => "[S]"
              | (false, false, true, true) => "(S)"
              | (false, false, false, false) => "[ ]"
              | (false, false, false, true) => "( )"
              | (n, m, o, p) =>
                Js.log((n, m, o, p));
                raise(Failure("Couldn't parse firewall"))
              },
            firewall
          )
          |> Js.Array.joinWith(" ")
        )
    };
    out^
  };
};

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
  let rec detect = (depth, firewall) => {
    let currentLayer = Firewall.getLayer(firewall, depth);
    switch (
      currentLayer.range > 0,
      currentLayer.scanner == 0,
      depth == Firewall.size(firewall) - 1
    ) {
    | (false, _, false) => detect(depth + 1, Firewall.tick(firewall))
    | (false, _, true) => false
    | (true, false, false) => detect(depth + 1, Firewall.tick(firewall))
    | (true, true, _) => true
    | (_, false, true) => false
    }
  };
  detect(0, Firewall.tickMany(tickOffset, firewall))
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
      Js.log(time^);
      time := time^ + 1
    };
    time^
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
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
    type t = {
      range: int,
      scanner: int,
      initial: int
    };
    let make = (range, initial) => {range, scanner: initial, initial};
    let tick = ({range, initial, scanner}: t) =>
      if (range > 0) {
        {range, initial, scanner: (scanner + 1) mod range}
      } else {
        {range, initial, scanner}
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
  let maxRange = (firewall) => fst(max(Array.map((layer: Layer.t) => layer.range, firewall)));
  let print = (firewall) => {
    let size = size(firewall);
    let range = maxRange(firewall);
    let out = ref(Array.init(size, (i) => "  " ++ string_of_int(i)) |> Js.Array.joinWith(" "));
    for (depth in 0 to range) {
      out :=
        out^
        ++ "\n "
        ++ (
          Array.map(
            (layer: Layer.t) =>
              if (layer.range <= 0) {
                "..."
              } else if (layer.scanner == 0) {
                "[S]"
              } else {
                "[ ]"
              },
            firewall
          )
          |> Js.Array.joinWith(" ")
        )
    }
    /* ++ "\n "
       ++ (
         Array.map(
           (layer: Layer.t) =>
             if (layer.range <= 0) {
               "..."
             } else if (layer.scanner == 0) {
               "[S]"
             } else {
               "[ ]"
             },
           firewall
         )
         |> Js.Array.joinWith(" ")
       )
       ++ "\n "
       ++ (
         Array.map(
           (layer: Layer.t) =>
             if (layer.range <= 1) {
               "   "
             } else if (layer.scanner == 1) {
               "[S]"
             } else {
               "[ ]"
             },
           firewall
         )
         |> Js.Array.joinWith(" ")
       ) */
  };
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("0: 3\n1: 2\n4: 4\n6: 4", 24)];
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
    let severity = ref(0);
    let firewall' = ref(firewall);
    let currentRange = ref(0);
    Js.log(Firewall.print(firewall));
    for (depth in 0 to Firewall.size(firewall) - 1) {
      Js.log(depth);
      let currentLayer = Firewall.getLayer(firewall'^, depth);
      if (currentLayer.range > 0) {
        if (currentLayer.scanner == currentRange^) {
          Js.log((currentLayer.scanner, currentRange^));
          severity := severity^ + depth * currentLayer.range
        }
      };
      currentRange := currentRange^ + 1;
      firewall' := Firewall.tick(firewall'^)
    };
    severity^
  };
};
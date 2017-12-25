module Image = {
  open! Grid;
  type t = Grid.t(bool);
  let ofString = string : t =>
    StringUtils.splitWith("/", string)
    |> Array.map(StringUtils.split)
    |> Array.map(
         Array.map(s =>
           switch s {
           | "." => false
           | "#" => true
           | _ => raise(Failure("Could not parse image"))
           }
         )
       );
  let toString = image : string =>
    Array.map(Array.map(x => x ? "#" : "."), image)
    |> Array.map(Js.Array.joinWith(""))
    |> Js.Array.joinWith("/");
  let partition = image : Grid.t(t) =>
    switch (size(image)) {
    | n when n mod 2 == 0 => partitionBy2(image)
    | n when n mod 3 == 0 => partitionBy3(image)
    | _ => raise(Failure("Invalid image size"))
    };
  let flatten: Grid.t(t) => t = Grid.flatten;
  let rotate = Grid.rotateClockwise;
  let flip = Grid.flip;
};

type rules = Js.Dict.t(string);

let parseRules = ruleString =>
  StringUtils.splitWith("\n", ruleString)
  |> Array.map(StringUtils.splitWith(" => "))
  |> Array.fold_left(
       (rules, r) =>
         switch r {
         | [|inp, out|] =>
           Js.Dict.set(rules, inp, out);
           rules;
         | _ => raise(Failure("Could not parse rules"))
         },
       Js.Dict.empty()
     );

let someOpt = opts =>
  List.find(
    o =>
      switch o {
      | Some(_) => true
      | None => false
      },
    opts
  );

let applyRule = (rules: rules, pattern) : Image.t => {
  let get = p => Js.Dict.get(rules, Image.toString(p));
  let id = pattern;
  let r = Image.rotate(id);
  let r2 = Image.rotate(r);
  let r3 = Image.rotate(r2);
  let f = Image.flip(id);
  let fr = Image.rotate(f);
  let fr2 = Image.rotate(fr);
  let fr3 = Image.rotate(fr2);
  switch (
    someOpt([
      get(id),
      get(r),
      get(r2),
      get(r3),
      get(f),
      get(fr),
      get(fr2),
      get(fr3)
    ])
  ) {
  | Some(p) => Image.ofString(p)
  | None => raise(Failure("This should not happen"))
  | exception _ => raise(Not_found)
  };
};

module Part1: Solution.Solver = {
  type input = (string, int);
  type answer = int;
  let cases = [
    (({|../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#|}, 2), 12)
  ];
  let solve = ((input, iterations)) => {
    let image = ref(Image.ofString(".#./..#/###"));
    let rules = parseRules(input);
    for (_ in 0 to iterations - 1) {
      let p = Image.partition(image^);
      image :=
        Image.flatten(
          Array.map(Array.map(subImage => applyRule(rules, subImage)), p)
        );
    };
    ArrayUtils.concat(image^)
    |> Array.fold_left(
         (t, b) =>
           if (b) {
             t + 1;
           } else {
             t;
           },
         0
       );
  };
};

module Part2: Solution.Solver = {
  type input = (string, int);
  type answer = int;
  let cases = [];
  let solve = ((input, _iterations)) => {
    let image = ref(Image.ofString(".#./..#/###"));
    let rules = parseRules(input);
    for (i in 0 to 17) {
      Js.log(i);
      let p = Image.partition(image^);
      image :=
        Image.flatten(
          Array.map(Array.map(subImage => applyRule(rules, subImage)), p)
        );
    };
    ArrayUtils.concat(image^)
    |> Array.fold_left(
         (t, b) =>
           if (b) {
             t + 1;
           } else {
             t;
           },
         0
       );
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;

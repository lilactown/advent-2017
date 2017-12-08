let unsafeUnwrap = (option) =>
  switch option {
  | None => raise(Not_found)
  | Some(k) => k
  };

module Part1 = {
  type input = string;
  type answer = string;
  let cases = [
    (
      {|pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)|},
      "tknk"
    )
  ];
  /* Find the name of the root program */
  let solve = (input) => Tower.Definition.findRootDef(Tower.Definition.fromInput(input), 0).name;
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    (
      {|pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)|},
      60
    )
  ];
  let findUnbalanced = (towers) => Js.Array.find((t: Tower.t) => ! t.isBalanced, towers);
  let findBalanced = (towers) => Js.Array.find((t: Tower.t) => t.isBalanced, towers);
  let rec balance = (tower: Tower.t) =>
    switch (tower.children, tower.isBalanced) {
    | (None, _) => raise(Failure("Could not balance 0"))
    | (_, true) => raise(Failure("Could not balance 1"))
    | (Some(children), false) =>
      switch (findUnbalanced(children)) {
      | None =>
        let sample = unsafeUnwrap(findBalanced(children));
        let outlier =
          unsafeUnwrap(
            Js.Array.find((t: Tower.t) => t.totalWeight != sample.totalWeight, children)
          );
        let diff = outlier.totalWeight - sample.totalWeight;
        (outlier.name, outlier.weight - diff)
      | Some(unbalanced) => balance(unbalanced)
      }
    };
  let solve = (input) => {
    let towerDefs = Tower.Definition.fromInput(input);
    let rootDef = Tower.Definition.findRootDef(towerDefs, 0);
    let tower = Tower.make(towerDefs, rootDef);
    let (_, newWeight) = balance(tower);
    newWeight
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
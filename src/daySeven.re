let sumArray = Array.fold_left((total, n) => total + n, 0);

let unsafeUnwrap = (option) =>
  switch option {
  | None => raise(Not_found)
  | Some(k) => k
  };

let nameWeightRe = [%bs.re "/(.+) \\((\\d+)\\)/"];

let splitNames = (array) =>
  switch array {
  | [||] => None
  | [|names|] => Some(Js.String.split(", ", names))
  | _ => raise(Failure("Could not parse children names"))
  };

module Tower = {
  /**
   * This module provides recursive representation (tree) of the
   * programs that are stacked in Day 7.
   *
   * Each tower has a name, a weight, and possibly some children stacked
   * on top of it.
   * While we construct the program, we also add some additional information
   * such as whether it is already balanced correctly, and what it's
   * weight plus its childrens weight equal.
   */
  type t = {
    name: string,
    weight: int,
    children: option(array(t)),
    isBalanced: bool,
    totalWeight: int
  };
  module Definition = {
    /**
     * The Tower.Definition module provides a simple type for defining
     * a (non-recursive) program, and some helpers for parsing the string
     * input from the challenge into these definitions.
     */
    type t = {
      name: string,
      weight: int,
      childrenNames: option(array(string))
    };
    let fromInput: Js.String.t => array(t) =
      (input) =>
        Js.Array.(
          Js.String.(
            split("\n", input)
            |> map(split(" -> "))
            |> map(
                 (line) =>
                   switch (Js.Re.exec(line[0], nameWeightRe)) {
                   | Some(result) =>
                     let captures = Js.Re.captures(result);
                     let name = Js.Nullable.to_opt(captures[1]);
                     let weight = Js.Nullable.to_opt(captures[2]);
                     switch (name, weight) {
                     | (Some(name), Some(weight)) => {
                         name,
                         weight: int_of_string(weight),
                         childrenNames: splitNames(Js.Array.sliceFrom(1, line))
                       }
                     | _ => raise(Failure("Could not parse name and weight"))
                     }
                   | None => raise(Failure("Could not parse input"))
                   }
               )
          )
        );
    let rec findRootDef = (definitions, currentDefIndex) => {
      let currentDef = definitions[currentDefIndex];
      let currentName = currentDef.name;
      let parent =
        Js.Array.find(
          (def) =>
            switch def.childrenNames {
            | None => false
            | Some(children) =>
              /* Find parent - if a parent is found, return true. else, return false */
              Js.Option.isSome(Js.Array.find((name) => name == currentName, children))
            },
          definitions
        );
      switch parent {
      | Some(_) => findRootDef(definitions, currentDefIndex + 1)
      | None => currentDef
      }
    };
    let findChildrenDefs = (definitions, names) =>
      Js.Array.filter((def) => Js.Array.includes(def.name, names), definitions);
  };
  /**
   *  Weight of the tower plus the weight of it's children
   **/
  let rec weigh = (tower) =>
    switch tower.children {
    | None => tower.weight
    | Some(children) =>
      tower.weight
      + Array.(map(weigh, children) |> fold_left((total, weight) => total + weight, 0))
    };
  let isBalanced = (weights: array(int)) =>
    Js.Array.every((weight) => weight == weights[0], weights);
  let rec make = (definitions: array(Definition.t), root: Definition.t) =>
    switch root.childrenNames {
    | None => {
        name: root.name,
        weight: root.weight,
        children: None,
        isBalanced: true,
        totalWeight: root.weight
      }
    | Some(names) =>
      let children = Array.map(make(definitions), Definition.findChildrenDefs(definitions, names));
      let weights = Array.map(weigh, children);
      {
        name: root.name,
        weight: root.weight,
        children: Some(children),
        isBalanced: isBalanced(weights),
        totalWeight: root.weight + sumArray(weights)
      }
    };
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
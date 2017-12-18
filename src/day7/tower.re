let nameWeightRe = [%bs.re "/(.+) \\((\\d+)\\)/"];

let splitNames = (array) =>
  switch array {
  | [||] => None
  | [|names|] => Some(Js.String.split(", ", names))
  | _ => raise(Failure("Could not parse children names"))
  };

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
    tower.weight + Array.(map(weigh, children) |> fold_left((total, weight) => total + weight, 0))
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
      totalWeight: root.weight + ArrayUtils.sum(0, weights)
    }
  };
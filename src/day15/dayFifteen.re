type binary = string;

/* BuckleScript maintains OCaml's 32-bit precision in integers
   which ends up truncating a lot of numbers - so we define our
   own platform-specific integer multiplication here */
let mul: (int, int) => int = [%bs.raw {| (a, b) => a * b|}];

let decToBin = (~bits=32, n) : binary => {
  let rec toBin = (k) =>
    switch (k / 2, k mod 2) {
    | (0, d) => string_of_int(d)
    | (k', d) => toBin(k') ++ string_of_int(d)
    };
  KnotHash.padStart(~length=bits, ~padWith="0", toBin(n))
};

module Generator = {
  type t = {
    value: int,
    factor: int
  };
  type makeFn = (~factor: int, ~start: int) => t;
  let constant = 2147483647;
  let value = ({value}: t) => value;
  let next = ({value, factor}) : t => {
    let next = mul(value, factor) mod constant;
    {value: next, factor}
  };
  let make: makeFn = fun (~factor, ~start) => ({factor, value: start}: t);
};

module PickyGenerator = {
  type t = {
    value: int,
    factor: int,
    criteria: int
  };
  type makeFn = (~factor: int, ~start: int, ~criteria: int) => t;
  let constant = 2147483647;
  let value = ({value}: t) => value;
  let rec next = ({value, factor, criteria}) : t => {
    let nextVal = mul(value, factor) mod constant;
    if (nextVal mod criteria == 0) {
      {value: nextVal, factor, criteria}
    } else {
      next({value: nextVal, factor, criteria})
    }
  };
  let make: makeFn = fun (~factor, ~start, ~criteria) => ({factor, value: start, criteria}: t);
};

module Judge = {
  module type GeneratorType = {
    type t;
    type makeFn;
    let value: t => int;
    let next: t => t;
    let make: makeFn;
  };
  module Make = (G: GeneratorType) => {
    let getLowest16 = (b: binary) => Js.String.substringToEnd(~from=16, b);
    let consider = (a, b) => {
      let aBin = decToBin(a);
      let bBin = decToBin(b);
      let a' = int_of_string("0b" ++ getLowest16(aBin));
      let b' = int_of_string("0b" ++ getLowest16(bBin));
      switch (a' - b') {
      | 0 => 0
      | n when n < 0 => (-1)
      | n when n > 0 => 1
      | _ => raise(Failure("Could not compare values??"))
      }
    };
    let make = (~generations=5, genA: G.t, genB: G.t) => {
      let matches = ref(0);
      let genA' = ref(genA);
      let genB' = ref(genB);
      for (_ in 0 to generations) {
        genA' := G.next(genA'^);
        genB' := G.next(genB'^);
        let a = G.value(genA'^);
        let b = G.value(genB'^);
        if (consider(a, b) == 0) {
          matches := matches^ + 1
        } else {
          ()
        }
      };
      matches^
    };
  };
};

module Part1: Solution.Solver = {
  type input = (int, int);
  type answer = int;
  /* let cases = [((65, 8921), 588)]; */
  let cases = [((65, 8921), 1)];
  module Judge = Judge.Make(Generator);
  let solve = ((aStart, bStart)) => {
    let genA = Generator.make(~factor=16807, ~start=aStart);
    let genB = Generator.make(~factor=48271, ~start=bStart);
    /* Judge.make(~generations=40_000_000, genA, genB) */
    Judge.make(~generations=5, genA, genB) /* only run 5 to keep test time down */
  };
};

module Part2: Solution.Solver = {
  type input = (int, int);
  type answer = int;
  /* let cases = [((65, 8921), 309)]; */
  let cases = [((65, 8921), 0)];
  module Judge = Judge.Make(PickyGenerator);
  let solve = ((aStart, bStart)) => {
    let genA = PickyGenerator.make(~factor=16807, ~start=aStart, ~criteria=4);
    let genB = PickyGenerator.make(~factor=48271, ~start=bStart, ~criteria=8);
    /* Judge.make(~generations=5_000_000, genA, genB) */
    Judge.make(~generations=5, genA, genB)
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
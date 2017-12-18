module Spinlock = {
  type t = {
    step: int,
    position: int,
    value: int,
    buffer: CircularArray.t(int)
  };
  let make = (step) => {step, position: 0, value: 0, buffer: CircularArray.make(1, 0)};
  let step = ({step, position, value, buffer}: t) => {
    let pos = CircularArray.computePos(buffer, position + step) + 1;
    /* Js.log(pos); */
    let buffer = CircularArray.insert(buffer, pos, value + 1);
    {step, buffer, value: value + 1, position: pos}
  };
};

module Part1: Solution.Solver = {
  type input = int;
  type answer = int;
  let cases = [(3, 638), (324, 1306)];
  let rec thrash = (spinlock: Spinlock.t) =>
    if (spinlock.value == 2017) {
      spinlock
    } else {
      thrash(Spinlock.step(spinlock))
    };
  let solve = (input) => {
    open Spinlock;
    let lock = thrash(make(input));
    lock.buffer[lock.position + 1]
  };
};

module Part2: Solution.Solver = {
  type input = int;
  type answer = int;
  let cases = [];
  let solve = (stepAmount) => {
    let lastOne = ref(0);
    let seq = (a_n, n) => (a_n + stepAmount) mod n + 1;
    let steps = 50_000_000;
    let a_n = ref(0);
    for (n in 1 to steps) {
      a_n := seq(a_n^, n);
      if (a_n^ == 1) {
        lastOne := n
      }
    };
    lastOne^
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
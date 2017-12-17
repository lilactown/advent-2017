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
    let buffer = CircularArray.insert(buffer, pos, value + 1);
    {step, buffer, value: value + 1, position: pos}
  };
  let position = (sp) => sp.position;
  let getFromBuffer = (sp, pos) => sp.buffer[pos];
};

let rec thrash = (spinlock: Spinlock.t) =>
  if (spinlock.value == 2017) {
    spinlock
  } else {
    thrash(Spinlock.step(spinlock))
  };

module Part1: Solution.Solver = {
  type input = int;
  type answer = int;
  let cases = [(3, 638)];
  let solve = (input) => {
    open Spinlock;
    let lock = thrash(make(input));
    getFromBuffer(lock, position(lock) + 1)
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = (input) => 6;
};

let part1 = Part1.solve;

let part2 = Part2.solve;
/* solution contained in knotHash.re */
module Part1: Solution.Solver = {
  type input = (string, int);
  type answer = int;
  let cases = [
    (("3,4,1,5", 5), 12),
    (("1,2,3,4", 4), 2),
    (("1,1,1,1", 5), 0),
    (("17,31,73,47,23", 256), 240)
  ];
  let solve = ((input, size)) => {
    let lengths = Js.String.split(",", input) |> Array.map(int_of_string);
    let numbers = Array.init(size, (i) => i);
    let (knottedNumbers, _, _) =
      KnotHash.knot(~lengths=Array.to_list(lengths), ~numbers, ~position=0, ~skip=0);
    knottedNumbers[0] * knottedNumbers[1]
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = string;
  let cases = [
    ("", "a2582a3a0e66e6e86e3812dcb672a272"),
    ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
    ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
    ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
  ];
  let solve = (input) => KnotHash.make(input);
};

let part1 = Part1.solve;

let part2 = Part2.solve;
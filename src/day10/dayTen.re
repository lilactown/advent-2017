let explode = (s) => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l
    } else {
      exp(i - 1, [s.[i], ...l])
    };
  exp(String.length(s) - 1, [])
};

[@bs.send] external padStart : (string, ~length: int, ~padWith: string) => string = "padStart";

let xorMany = Array.fold_left((accum, n) => accum lxor n, 0);

let toHex = (n) => padStart(Printf.sprintf("%x", n), ~length=2, ~padWith="0");

let rec knot = (~lengths, ~numbers, ~position, ~skip) =>
  switch lengths {
  | [length, ...tail] =>
    let start = position;
    /* CircularArray impl in `./circularArray.re` */
    let subArray = CircularArray.rev(CircularArray.sub(numbers, ~start, ~length));
    let newNumbers = CircularArray.replace(numbers, ~start, ~withArray=subArray);
    knot(~lengths=tail, ~numbers=newNumbers, ~position=position + length + skip, ~skip=skip + 1)
  | [] => (numbers, position, skip)
  };

let rec sparse = (~lengths, ~round, ~numbers, ~position, ~skip) =>
  if (round > 0) {
    let (newNumbers, newPosition, newSkip) = knot(~lengths, ~position, ~skip, ~numbers);
    sparse(~lengths, ~round=round - 1, ~numbers=newNumbers, ~position=newPosition, ~skip=newSkip)
  } else {
    numbers
  };

let dense = (numbers) => {
  let blocks = Array.make(16, 0);
  for (blockNum in 0 to 15) {
    let block = Array.sub(numbers, blockNum * 16, 16);
    blocks[blockNum] = xorMany(block)
  };
  blocks
};

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
      knot(~lengths=Array.to_list(lengths), ~numbers, ~position=0, ~skip=0);
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
  let solve = (input) => {
    let ascii = explode(input) |> List.map(Char.code);
    let lengths = List.concat([ascii, [17, 31, 73, 47, 23]]);
    let numbers = Array.init(256, (i) => i);
    let sparseHash = sparse(~lengths, ~numbers, ~round=64, ~position=0, ~skip=0);
    let denseHash = dense(sparseHash);
    Array.map(toHex, denseHash) |> Js.Array.joinWith("")
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
type name = string;

type position = int;

type moves =
  | Spin(int)
  | Exchange(position, position)
  | Partner(name, name);

type program = string;

type programs = array(program);

let spin = (p: programs, n) : programs =>
  Array.mapi((i, _) => p[(Array.length(p) - n + i) mod Array.length(p)], p);

let exchange = (p: programs, posA: position, posB) =>
  Array.mapi(
    (i, program) =>
      if (i == posA) {
        p[posB]
      } else if (i == posB) {
        p[posA]
      } else {
        program
      },
    p
  );

let partner = (p: programs, nameA: name, nameB: name) =>
  Array.mapi(
    (_, program) =>
      if (program == nameA) {
        nameB
      } else if (program == nameB) {
        nameA
      } else {
        program
      },
    p
  );

let executeMove = (p: programs, move) =>
  switch move {
  | Spin(n) => spin(p, n)
  | Exchange(p1, p2) => exchange(p, p1, p2)
  | Partner(n1, n2) => partner(p, n1, n2)
  };

let rec dance = (p: programs, moves: list(moves)) =>
  switch moves {
  | [] => p
  | [currentMove, ...rest] => dance(executeMove(p, currentMove), rest)
  };

let matchMove = [%bs.re "/(s|x|p)(\\d+|.)(\\/)?(\\d+|.)?/"];

let parseDance = (input) =>
  Js.String.split(",", input)
  |> Array.map((s) => Js.Re.exec(s, matchMove))
  |> Array.map(
       (o) =>
         switch o {
         | Some(v) => Js.Re.captures(v)
         | None => raise(Failure("Could not match on string"))
         }
     )
  |> Array.map(
       Array.map(
         (v) =>
           switch (Js.Nullable.to_opt(v)) {
           | Some(v') => v'
           | None => ""
           }
       )
     )
  |> Array.map(
       (instruction) =>
         switch instruction {
         /* The first element is the fully matched string */
         | [|_, "s", n, "", ""|] => Spin(int_of_string(n))
         | [|_, "x", p1, "/", p2|] => Exchange(int_of_string(p1), int_of_string(p2))
         | [|_, "p", n1, "/", n2|] => Partner(n1, n2)
         | err =>
           Js.log(err);
           raise(Failure("Could not parse instructions"))
         }
     )
  |> Array.to_list;

module Part1: Solution.Solver = {
  type input = (int, string);
  type answer = string;
  let cases = [((5, "s1,x3/4,pe/b"), "baedc")];
  let solve = ((amount, input)) => {
    let start = Array.init(amount, (i) => Char.chr(97 + i)) |> Array.map(String.make(1));
    dance(start, parseDance(input)) |> Js.Array.joinWith("")
  };
};

let rec findCycle = (~initialProgram: programs, programs, ~danceMoves, iter) => {
  let programs' = dance(List.hd(programs), danceMoves);
  if (programs' == initialProgram) {
    programs
  } else {
    findCycle(~initialProgram, [programs', ...programs], ~danceMoves, iter + 1)
  }
};

module Part2: Solution.Solver = {
  type input = (int, string);
  type answer = string;
  let cases = [];
  let solve = ((amount, input)) => {
    let start = Array.init(amount, (i) => Char.chr(97 + i)) |> Array.map(String.make(1));
    let danceMoves = parseDance(input);
    let programs = findCycle(~initialProgram=start, [start], ~danceMoves, 1);
    let position = 1_000_000_000 mod List.length(programs);
    Js.log((List.length(programs), position));
    List.nth(programs, position) |> Js.Array.joinWith("")
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
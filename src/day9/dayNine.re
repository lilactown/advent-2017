let explode = (s) => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l
    } else {
      exp(i - 1, [s.[i], ...l])
    };
  exp(String.length(s) - 1, [])
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    ("{}", 1),
    ("{{{}}}", 6),
    ("{{},{}}", 5),
    ("{{{},{},{{}}}}", 16),
    ("{<a>,<a>,<a>,<a>}", 1),
    ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9),
    ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
    ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3),
    ("{{!}!{!}!{!!!{}}", 3),
    ("{{}{<<<<{>{}{}}}", 11)
  ];
  let solve = (input) => {
    let stream = explode(input) |> StreamParser.parse;
    /* StreamParser.print(stream); */
    StreamParser.sumGroups(stream)
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    ("<>", 0),
    ("<random characters>", 17),
    ("<<<<>", 3),
    ("<{!>}>", 2),
    ("<!!>", 0),
    ("<!!!>>", 0),
    ("<{o\"i!a,<{i<a>", 10)
  ];
  let solve = (input) => {
    let stream = explode(input) |> StreamParser.parse;
    StreamParser.sumGarbage(stream)
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
let splitLines = (input) => StringUtils.splitWith("\n", input) |> Array.to_list;

let splitDigits = (line) =>
  /* change this to split "\t" on actual input */
  StringUtils.splitWith(" ", line) |> Array.map(int_of_string) |> Array.to_list;

let debug = (input) => {
  Js.log(input);
  input
};

module Part1 = {
  type input = string;
  type answer = int;
  let cases = [("5 1 9 5\n7 5 3\n2 4 6 8", 18)];
  let minMax = ((x: int, y: int), z) =>
    switch (z > x, z < y) {
    | (true, _) => (z, y)
    | (_, true) => (x, z)
    | _ => (x, y)
    };
  let lineMinMax = (line) =>
    switch line {
    | [] => raise(Failure("Empty line"))
    | [hd, ...tail] => List.fold_left(minMax, (hd, hd), tail)
    };
  let solve = (input) =>
    List.(
      splitLines(input)
      |> map(splitDigits)
      |> map(lineMinMax)
      |> fold_left((total, (x, y)) => total + x - y, 0)
    );
};

module Part2 = {
  type input = string;
  type answer = int;
  let cases = [("5 9 2 8\n9 4 7 3\n3 8 6 5", 9)];
  let rec hasDivisor = (n, digits) =>
    switch digits {
    | [] => None
    | [m, ...rest] =>
      switch (n mod m == 0, m mod n == 0) {
      | (true, _) => Some((n, m))
      | (_, true) => Some((m, n))
      | _ => hasDivisor(n, rest)
      }
    };
  let rec divisors = (digits) =>
    switch digits {
    | [] => raise(Failure("No divisors found"))
    | [n, ...rest] =>
      switch (hasDivisor(n, rest)) {
      | Some(x) => x
      | None => divisors(rest)
      }
    };
  let solve = (input) =>
    List.(
      splitLines(input)
      |> map(splitDigits)
      |> map(divisors)
      |> fold_left((total, (n, m)) => total + n / m, 0)
    );
};

let part1 = Part1.solve;

let part2 = Part2.solve;
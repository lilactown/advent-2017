let splitLines = (input) => Js.String.split("\n", input) |> Array.to_list;

let splitDigits = (line) =>
  Js.String.split("\t", line) |> Array.map(int_of_string) |> Array.to_list;

let debug = (input) => {
  Js.log(input);
  input
};

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

let part1 = (input) =>
  List.(
    splitLines(input)
    |> map(splitDigits)
    |> map(lineMinMax)
    |> fold_left((total, (x, y)) => total + x - y, 0)
  );

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

let part2 = (input) =>
  List.(
    splitLines(input)
    |> map(splitDigits)
    |> map(divisors)
    |> fold_left((total, (n, m)) => total + n / m, 0)
  );
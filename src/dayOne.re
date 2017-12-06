let explode = (input) => Js.String.split("", input) |> Array.map(int_of_string);

let rec solver = (digits, len, step, xPos, total) => {
  let x = digits[xPos];
  let yPos = (xPos + step) mod len;
  let y = digits[yPos];
  /* curry the next call to solver */
  let next = solver(digits, len, step);
  let isEndOfList = xPos == len - 1;
  switch (x == y, isEndOfList) {
  | (false, true) => total
  | (true, true) => total + x
  | (false, false) => next(xPos + 1, total)
  | (true, false) => next(xPos + 1, total + x)
  }
};

module Part1 = {
  type input = string;
  type answer = int;
  let cases = [("1122", 3), ("1111", 4), ("1234", 0), ("91212129", 9)];
  let solve = (input) => {
    let step = 1;
    let digits = explode(input);
    solver(digits, Array.length(digits), step, 0, 0)
  };
};

module Part2 = {
  type input = string;
  type answer = int;
  let cases = [("1212", 6), ("1221", 0), ("123425", 4), ("123123", 12), ("12131415", 4)];
  let solve = (input) => {
    let len = String.length(input);
    let step = len / 2;
    let digits = explode(input);
    solver(digits, len, step, 0, 0)
  };
};
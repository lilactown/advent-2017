module Part1 = {
  type input = string;
  type answer = int;
  let cases = [("0\n3\n0\n1\n-3", 5)];
  /**
   * Solution to part 1
   */
  let rec jump = (stack, pos, step) =>
    switch stack[pos] {
    | exception _ => step
    | offset =>
      /* get next position */
      let pos' = pos + offset;
      /* mutate the array, increasing current offset by 1 */
      stack[pos] = offset + 1;
      /* go to the next position */
      jump(stack, pos', step + 1)
    };
  let solve = (instructions) => {
    let stack = Js.String.split("\n", instructions) |> Array.map(int_of_string);
    jump(stack, 0, 0)
  };
};

module Part2 = {
  type input = string;
  type answer = int;
  let cases = [("0\n3\n0\n1\n-3", 10)];
  /**
   * Solution to part 2
   */
  let rec jump = (stack, pos, step) =>
    switch stack[pos] {
    | exception _ => step
    | offset when offset >= 3 =>
      let pos' = pos + offset;
      stack[pos] = offset - 1;
      jump(stack, pos', step + 1)
    | offset =>
      let pos' = pos + offset;
      stack[pos] = offset + 1;
      jump(stack, pos', step + 1)
    };
  let solve = (instructions) => {
    let stack = Js.String.split("\n", instructions) |> Array.map(int_of_string);
    jump(stack, 0, 0)
  };
};

/* For easy JS consumption */
let part1 = Part1.solve;

let part2 = Part2.solve;
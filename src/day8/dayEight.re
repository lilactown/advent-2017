let max = (numbers) => {
  let (max, index, _) =
    Array.fold_left(
      ((curMax, lastMaxIndex, index), n: int) =>
        curMax >= n ? (curMax, lastMaxIndex, index + 1) : (n, index, index + 1),
      (numbers[0], 0, 0),
      numbers
    );
  (max, index)
};

let unwrapRegisterValue = (register) =>
  switch register {
  | None => 0
  | Some(value) => value
  };

let assertPredicate = (context, register, predicate, amount) => {
  let value = unwrapRegisterValue(Js.Dict.get(context, register));
  switch predicate {
  | ">" => value > amount
  | "<" => value < amount
  | ">=" => value >= amount
  | "<=" => value <= amount
  | "==" => value == amount
  | "!=" => value != amount
  | _ => raise(Failure("Could not parse predicate"))
  }
};

let executeOperation = (context, register, op, amount) => {
  let value = unwrapRegisterValue(Js.Dict.get(context, register));
  switch op {
  | "inc" => value + amount
  | "dec" => value - amount
  | _ => raise(Failure("Could not parse operation"))
  }
};

module Part1: Solution.Solver = {
  /**
   * What is the largest value in any register after completing the instructions in your puzzle input?
   */
  type input = string;
  type answer = int;
  let cases = [
    ({|b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10|}, 1)
  ];
  let solve = (input) => {
    let context = Js.Dict.empty();
    Js.String.split("\n", input)
    |> Array.map(Js.String.split(" "))
    |> Array.iter(
         (line) =>
           switch line {
           | [|opReg, op, amt, "if", predReg, pred, predAmt|] =>
             let amount = int_of_string(amt);
             let predAmount = int_of_string(predAmt);
             if (assertPredicate(context, predReg, pred, predAmount)) {
               let newValue = executeOperation(context, opReg, op, amount);
               Js.Dict.set(context, opReg, newValue)
             } else {
               ()
             }
           | _ => raise(Failure("Could not parse instruction"))
           }
       );
    let (largest, _) = max(Js.Dict.values(context));
    largest
  };
};

module Part2: Solution.Solver = {
  /**
   * To be safe, the CPU also needs to know the highest value
   * held in any register during this process so that it can
   * decide how much memory to allocate to these operations.
   **/
  type input = string;
  type answer = int;
  let cases = [
    ({|b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10|}, 10)
  ];
  let solve = (input) => {
    let context = Js.Dict.empty();
    let max = ref(0);
    Js.String.split("\n", input)
    |> Array.map(Js.String.split(" "))
    |> Array.iteri(
         (i, line) =>
           switch line {
           | [|opReg, op, amt, "if", predReg, pred, predAmt|] =>
             let amount = int_of_string(amt);
             let predAmount = int_of_string(predAmt);
             if (assertPredicate(context, predReg, pred, predAmount)) {
               let newValue = executeOperation(context, opReg, op, amount);
               Js.Dict.set(context, opReg, newValue);
               /* This time, for each iteration, we check to see if the new value
                  is greater than the current max.
                  If it's the first line, we set the current max to be it's
                  value regardless to account for all-negative values. */
               switch (i == 0, newValue > max^) {
               | (true, _) => max := newValue
               | (_, true) => max := newValue
               | _ => ()
               }
             } else {
               ()
             }
           | _ => raise(Failure("Could not parse instruction"))
           }
       );
    max^
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
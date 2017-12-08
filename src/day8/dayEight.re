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

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    ({|b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10|}, 1)
  ];
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
               Js.Dict.set(context, opReg, executeOperation(context, opReg, op, amount))
             } else {
               ()
             }
           | _ => raise(Failure("Could not parse register"))
           }
       );
    let (largest, _) = max(Js.Dict.values(context));
    largest
  };
};

module Part2 = {};

let part1 = Part1.solve;
module type Solver = {
  type input;
  type answer;
  let cases: list((input, answer));
  let solve: input => answer;
};

type result('input, 'answer) =
  | Pass
  | Fail('input, 'answer, 'answer);

module type Solution = {let check: unit => array(result('a, 'b));};

module Make = (S: Solver) => {
  /* type result = result(S.input, S.answer); */
  let check = () => {
    let results =
      List.(
        S.cases
        |> map(
             ((input, expected)) => {
               let result = S.solve(input);
               result == expected ? Pass : Fail(input, expected, result)
             }
           )
      );
    Array.of_list(results)
  };
};

let checkSolution = ((module S): (module Solution)) => S.check();

let resultToBool = (result) =>
  switch result {
  | Pass => Js.true_
  | Fail(_, _, _) => Js.false_
  };

let failureToJs = (result) =>
  switch result {
  | Fail(input, expected, result) => [|input, expected, result|]
  | _ => [||]
  };
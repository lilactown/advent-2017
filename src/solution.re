module type Solver = {
  type input;
  type answer;
  let cases: list((input, answer));
  let solve: input => answer;
};

module Make = (S: Solver) => {
  let check = () =>
    List.(S.cases |> map(((input, output)) => S.solve(input) == output) |> for_all((v) => v))
    |> Js.Boolean.to_js_boolean;
};
module Part1: Solution.Solver = {
  type input = string;
  type answer = string;
  let cases = [([%bs.raw "require('./testInput.js')"], "ABCDEF")];
  let solve = (input) => {
    let d = ref(Route.make(input));
    let journey = [||];
    while (! d^.finished) {
      Js.Array.push(d^, journey) |> ignore;
      d := Route.move(d^)
    };
    /* let frames = Array.map(Route.printDiagram, journey); */
    /* let stop = ref(false);
       let animation = AsciiAnimation.make();
       AsciiAnimation.load(animation, frames) |> AsciiAnimation.start; */
    Js.Array.filter(
      (route: Route.t) =>
        switch route.symbol {
        | Breadcrumb(_) => true
        | _ => false
        },
      journey
    )
    |> Array.map(
         (route: Route.t) =>
           switch route.symbol {
           | Breadcrumb(c) => String.make(1, c)
           | _ => ""
           }
       )
    |> Js.Array.joinWith("")
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [([%bs.raw "require('./testInput.js')"], 38)];
  let solve = (input) => {
    let d = ref(Route.make(input));
    let journey = [||];
    while (! d^.finished) {
      Js.Array.push(d^, journey) |> ignore;
      d := Route.move(d^)
    };
    Array.length(journey) - 1
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
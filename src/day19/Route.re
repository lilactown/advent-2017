type symbol =
  | Empty
  | Vertical
  | Horizontal
  | Joint
  | Breadcrumb(char);

type diagram = array(array(symbol));

type direction =
  | Up
  | Down
  | Left
  | Right
  | NoWhere;

type t = {
  position: (int, int),
  diagram,
  direction,
  symbol,
  finished: bool
};

let parse = (input) : diagram =>
  StringUtils.splitWith("\n", input)
  |> Js.Array.filter((line) => line != "")
  |> Array.map(StringUtils.explode)
  |> Array.map(
       List.map(
         (c) =>
           switch c {
           | ' ' => Empty
           | '|' => Vertical
           | '-' => Horizontal
           | '+' => Joint
           | 'A'..'Z' => Breadcrumb(c)
           | _ => raise(Failure("Could not parse diagram"))
           }
       )
     )
  |> Array.map(Array.of_list);

let printDirection = (route) =>
  switch route.direction {
  | Up => "Up"
  | Down => "Down"
  | Left => "Left"
  | Right => "Right"
  | NoWhere => "Done"
  };

let printDiagram = (route: t) =>
  Array.mapi(
    (y, row) =>
      Array.mapi(
        (x, d) =>
          switch (d, route.position == (x, y)) {
          | (Empty, false) => "   "
          | (Empty, true) => "( )"
          | (Vertical, false) => " | "
          | (Vertical, true) => "(|)"
          | (Horizontal, false) => " - "
          | (Horizontal, true) => "(-)"
          | (Joint, false) => " + "
          | (Joint, true) => "(+)"
          | (Breadcrumb(c), false) => " " ++ String.make(1, c) ++ " "
          | (Breadcrumb(c), true) => "(" ++ String.make(1, c) ++ ")"
          },
        row
      ),
    route.diagram
  )
  |> Array.map(Js.Array.joinWith(""))
  |> Js.Array.joinWith("\n");

let start = (diagram: diagram) =>
  Js.Array.findIndex(
    (d) =>
      switch d {
      | Vertical => true
      | Empty => false
      | _ => raise(Failure("Invalid diagram: more than one start"))
      },
    diagram[0]
  )
  |> ((j) => (j, 0));

let get = (route, (x, y)) =>
  switch route.diagram[y][x] {
  | exception _ => Empty
  | v => v
  };

let make = (input) => {
  let diagram = parse(input);
  let position = start(diagram);
  let direction = Down;
  {position, diagram, direction, symbol: Vertical, finished: false}
};

let determineDirection = (route, (x, y)) =>
  switch (route.direction, get(route, (x, y))) {
  | (Down, Vertical) => Down
  | (Down, Breadcrumb(_)) => Down
  | (Down, Joint) =>
    switch (get(route, (x, y + 1)), get(route, (x - 1, y)), get(route, (x + 1, y))) {
    | (Empty, Empty, Empty) => raise(Failure("Dead end"))
    | (Empty, _v, Empty) => Left
    | (Empty, Empty, _v) => Right
    | (_v, _, _) => Down
    }
  | (Down, Empty) => NoWhere
  | (Down, _) => Down
  | (Right, Horizontal) => Right
  | (Right, Breadcrumb(_)) => Right
  | (Right, Joint) =>
    switch (get(route, (x + 1, y)), get(route, (x, y + 1)), get(route, (x, y - 1))) {
    | (Empty, Empty, Empty) => raise(Failure("Dead end"))
    | (Empty, _v, Empty) => Down
    | (Empty, Empty, _v) => Up
    | (_v, _, _) => Right
    }
  | (Right, Empty) => NoWhere
  | (Right, _) => Right
  | (Up, Vertical) => Up
  | (Up, Breadcrumb(_)) => Up
  | (Up, Joint) =>
    switch (get(route, (x, y - 1)), get(route, (x - 1, y)), get(route, (x + 1, y))) {
    | (Empty, Empty, Empty) => raise(Failure("Dead end"))
    | (Empty, _v, Empty) => Left
    | (Empty, Empty, _v) => Right
    | (_v, _, _) => Up
    }
  | (Up, Empty) => NoWhere
  | (Up, _) => Up
  | (Left, Horizontal) => Left
  | (Left, Breadcrumb(_)) => Left
  | (Left, Joint) =>
    switch (get(route, (x - 1, y)), get(route, (x, y + 1)), get(route, (x, y - 1))) {
    | (Empty, Empty, Empty) => raise(Failure("Dead end"))
    | (Empty, _v, Empty) => Down
    | (Empty, Empty, _v) => Up
    | (_v, _, _) => Left
    }
  | (Left, Empty) => NoWhere
  | (Left, _) => Left
  | (NoWhere, _) => raise(Failure("determineDirection called on 'Done'"))
  };

let move = (route: t) =>
  switch route.direction {
  | Down =>
    let (x, y) = route.position;
    let newPos = (x, y + 1);
    {
      ...route,
      position: newPos,
      symbol: get(route, newPos),
      direction: determineDirection(route, newPos)
    }
  | Right =>
    let (x, y) = route.position;
    let newPos = (x + 1, y);
    {
      ...route,
      position: (x + 1, y),
      symbol: get(route, newPos),
      direction: determineDirection(route, (x + 1, y))
    }
  | Up =>
    let (x, y) = route.position;
    let newPos = (x, y - 1);
    {
      ...route,
      position: newPos,
      symbol: get(route, newPos),
      direction: determineDirection(route, newPos)
    }
  | Left =>
    let (x, y) = route.position;
    let newPos = (x - 1, y);
    {
      ...route,
      position: (x - 1, y),
      symbol: get(route, newPos),
      direction: determineDirection(route, (x - 1, y))
    }
  | NoWhere => {...route, finished: true}
  };
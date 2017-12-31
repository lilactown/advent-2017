module InfiniteArray = {
  type t('a) = array('a);
  let get = (~default, n, ia) =>
    switch ia[n] {
    | v => (v, n, ia)
    | exception _ =>
      if (n < 0) {
        let len = Array.length(ia) - n;
        let newIA =
          Array.init(len, i =>
            if (i > abs(n) - 1) {
              ia[i + n];
            } else {
              default;
            }
          );
        (default, abs(n) - 1, newIA);
      } else {
        let newIA =
          Array.init(n + 1, i =>
            if (i < Array.length(ia)) {
              ia[i];
            } else {
              default;
            }
          );
        (default, n, newIA);
      }
    };
  let set = (~default, n, v, ia) => {
    let (_, n', ia') = get(~default, n, ia);
    ia'[n'] = v;
    ia';
  };
};

external ofNullable : array(Js.Nullable.t('a)) => array('a) = "%identity";

module Machine = {
  type tape = InfiniteArray.t(bool);
  type direction =
    | Left
    | Right;
  type action = {
    writeValue: bool,
    direction,
    next: Js.Dict.key
  }
  and state = (action, action);
  type cursor = int;
  type t = {
    tape,
    currentState: Js.Dict.key,
    states: Js.Dict.t(state),
    cursor
  };
  let make = input => {
    let regex = [%bs.re
      {|/In state ([A-Z]):\n\s\sIf the current value is 0:\n\s{4}- Write the value (\d).\n\s{4}- Move one slot to the (.+).\n\s{4}- Continue with state ([A-Z]).\n\s\sIf the current value is 1:\n\s{4}- Write the value (\d).\n\s{4}- Move one slot to the (.+).\n\s{4}- Continue with state ([A-Z])./g|}
    ];
    let capture = ref(Js.Re.exec(input, regex));
    let states = Js.Dict.empty();
    while (capture^ != None) {
      switch (ofNullable(Js.Re.captures(Js.Option.getExn(capture^)))) {
      | [|
          _,
          name,
          writeIf0,
          directionIf0,
          nextIf0,
          writeIf1,
          directionIf1,
          nextIf1
        |] =>
        let if0 = {
          writeValue: writeIf0 == "1" ? true : false,
          direction: directionIf0 == "left" ? Left : Right,
          next: nextIf0
        };
        let if1 = {
          writeValue: writeIf1 == "1" ? true : false,
          direction: directionIf1 == "left" ? Left : Right,
          next: nextIf1
        };
        Js.Dict.set(states, name, (if0, if1));
        capture := Js.Re.exec(input, regex);
      | _ => raise(Failure("Could not make Turing machine"))
      };
    };
    {tape: [|false|], currentState: "A", states, cursor: 0};
  };
  let exec = machine => {
    let state =
      Js.Option.getExn(Js.Dict.get(machine.states, machine.currentState));
    let (value, cursor, tape) =
      InfiniteArray.get(~default=false, machine.cursor, machine.tape);
    if (! value) {
      let {writeValue, direction, next} = fst(state);
      tape[cursor] = writeValue;
      let cursor =
        switch direction {
        | Left => cursor - 1
        | Right => cursor + 1
        };
      let (_, cursor, tape) = InfiniteArray.get(~default=false, cursor, tape);
      {tape, cursor, states: machine.states, currentState: next};
    } else {
      let {writeValue, direction, next} = snd(state);
      tape[cursor] = writeValue;
      let cursor =
        switch direction {
        | Left => cursor - 1
        | Right => cursor + 1
        };
      let (_, cursor, tape) = InfiniteArray.get(~default=false, cursor, tape);
      {tape, cursor, states: machine.states, currentState: next};
    };
  };
  let toString = machine => {
    let {currentState, cursor, tape} = machine;
    tape
    |> Array.mapi((i, v) =>
         switch (i == cursor, v) {
         | (true, true) => "[1]"
         | (true, false) => "[0]"
         | (false, true) => " 1 "
         | (false, false) => " 0 "
         }
       )
    |> Array.append([|"(" ++ currentState ++ ")   "|])
    |> Js.Array.joinWith("");
  };
  let statesToString = machine =>
    Js.Dict.entries(machine.states)
    |> Array.map(((key, (if0, if1))) => {
         let direction0 = if0.direction == Left ? "Left" : "Right";
         let direction1 = if1.direction == Left ? "Left" : "Right";
         let writeValue0 = if0.writeValue;
         let writeValue1 = if1.writeValue;
         let next0 = if0.next;
         let next1 = if1.next;
         {j|In state $key :
  If the current value is 0:
  - Write the value $writeValue0
  - Move one slot to the $direction0
  - Continue with state $next0
  If the current value is 1:
  - Write the value $writeValue1
  - Move one slot to the $direction1
  - Continue with state $next1|j};
       })
    |> Js.Array.joinWith("\n\n");
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    (
      "Begin in state A.\nPerform a diagnostic checksum after 6 steps.\n\nIn state A:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state B.\n\nIn state B:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A.",
      3
    )
  ];
  let matchSteps = [%bs.re
    {|/Perform a diagnostic checksum after (\d+) steps./|}
  ];
  let solve = input => {
    let count =
      switch (Js.Re.exec(input, matchSteps)) {
      | Some(c) => int_of_string(ofNullable(Js.Re.captures(c))[1])
      | None => raise(Failure("Could not parse input"))
      };
    let machine = ref(Machine.make(input));
    /* Js.log(Machine.statesToString(machine^)); */
    for (_ in 0 to count - 1) {
      /* Js.log(machine^); */
      machine := Machine.exec(machine^);
    };
    /* Js.log(Machine.toString(machine^)); */
    Array.fold_left((t, b) => b ? t + 1 : t, 0, machine^.tape);
  };
};

let part1 = Part1.solve;

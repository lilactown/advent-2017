module Processor = {
  type registers = Js.Dict.t(int);
  type name = Js.Dict.key;
  type value =
    | Name(name)
    | Int(int);
  type instruction =
    | Set(name, value)
    | Subtract(name, value)
    | Multiply(name, value)
    | JumpNotZero(value, value);
  type state = {
    stack: array(instruction),
    stackPos: int,
    registers,
    locked: bool
  };
  let toValue = (s: string) => {
    let c = s.[0];
    switch c {
    | 'a'..'z' => Name(s)
    | _ => Int(IntUtils.ofString(s))
    };
  };
  let getRegister = (registers, name) =>
    switch (Js.Dict.get(registers, name)) {
    | Some(v) => v
    | None => 0
    };
  let setRegister = (registers: registers, name: name, value) =>
    Js.Dict.set(registers, name, value);
  let parse = input : array(instruction) =>
    StringUtils.splitWith("\n", input)
    |> Array.map(StringUtils.splitWith(" "))
    |> Array.map(line =>
         switch line {
         | [|"set", x, y|] => Set(x, toValue(y))
         | [|"sub", x, y|] => Subtract(x, toValue(y))
         | [|"mul", x, y|] => Multiply(x, toValue(y))
         | [|"jnz", x, y|] => JumpNotZero(toValue(x), toValue(y))
         | _ => raise(Failure("Could not parse instructions"))
         }
       );
  let makeProgram = (input, ~initialReg) => {
    stack: parse(input),
    stackPos: 0,
    registers: initialReg,
    locked: false
  };
  let run = state => {
    let finished =
      state.stackPos > Array.length(state.stack) - 1 || state.stackPos < 0;
    if (finished) {
      {...state, locked: true};
    } else {
      let current = state.stack[state.stackPos];
      let get = getRegister(state.registers);
      let set = setRegister(state.registers);
      switch current {
      | Set(n, Int(f)) =>
        set(n, f);
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Set(n, Name(n2)) =>
        set(n, get(n2));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Subtract(n, Int(f)) =>
        set(n, IntUtils.add(get(n), - f));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Subtract(n, Name(n2)) =>
        set(n, IntUtils.add(get(n), - get(n2)));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Multiply(n, Int(f)) =>
        set(n, IntUtils.mul(get(n), f));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Multiply(n, Name(n2)) =>
        set(n, IntUtils.mul(get(n), get(n2)));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | JumpNotZero(Name(n1), Name(n2)) when get(n1) !== 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, get(n2))
        }
      | JumpNotZero(Name(n), Int(f)) when get(n) !== 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, f)
        }
      | JumpNotZero(Int(f), Name(n2)) when f !== 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, get(n2))
        }
      | JumpNotZero(Int(f1), Int(f2)) when f1 !== 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, f2)
        }
      | JumpNotZero(_, _) => {
          ...state,
          locked: false,
          stackPos: state.stackPos + 1
        }
      };
    };
  };
  let printValue = v =>
    switch v {
    | Name(n) => n
    | Int(f) => IntUtils.toString(f)
    };
  let print = ({stack, stackPos}) =>
    switch stack[stackPos] {
    | Set(name, value) => "Set(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Subtract(name, value) =>
      "Subtract(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Multiply(name, value) =>
      "Mul(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | JumpNotZero(v1, v2) =>
      "JNZ(" ++ printValue(v1) ++ ", " ++ printValue(v2) ++ ")"
    };
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [];
  let solve = input => {
    let program =
      ref(Processor.makeProgram(input, ~initialReg=Js.Dict.empty()));
    let count = ref(0);
    while (! program^.locked) {
      program := Processor.run(program^);
      switch program^.stack[program^.stackPos] {
      | Multiply(_, _) => count := count^ + 1
      | _ => ()
      | exception _ => Js.log("wat.")
      };
    };
    count^;
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [];
  let isPrime = n => {
    let rec check = k =>
      if (k == n) {
        true;
      } else if (IntUtils.modulo(n, k) == 0) {
        false;
      } else {
        check(k + 1);
      };
    check(2);
  };
  let solve = _input => {
    /* This is the completely optimized Reason code.
       See optimized.js for the notes in JavaScript. */
    let rec solver = (n, h) =>
      if (n == 125400) {
        /* 125400 is not prime */
        h + 1;
      } else if (! isPrime(n)) {
        solver(n + 17, h + 1);
      } else {
        solver(n + 17, h);
      };
    solver(108400, 0);
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;

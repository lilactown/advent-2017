module Processor = {
  type registers = Js.Dict.t(int);
  type name = Js.Dict.key;
  type value =
    | Name(name)
    | Int(int);
  type instruction =
    | Snd(value)
    | Set(name, value)
    | Add(name, value)
    | Multiply(name, value)
    | Modulo(name, value)
    | Rcv(value)
    | JumpIfGreaterThanZero(value, value);
  type state = {
    stack: array(instruction),
    stackPos: int,
    registers,
    /* finished: bool, */
    onRcv: (state, value) => state,
    onSnd: int => unit,
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
         | [|"snd", x|] => Snd(toValue(x))
         | [|"set", x, y|] => Set(x, toValue(y))
         | [|"add", x, y|] => Add(x, toValue(y))
         | [|"mul", x, y|] => Multiply(x, toValue(y))
         | [|"mod", x, y|] => Modulo(x, toValue(y))
         | [|"rcv", x|] => Rcv(toValue(x))
         | [|"jgz", x, y|] => JumpIfGreaterThanZero(toValue(x), toValue(y))
         | _ => raise(Failure("Could not parse instructions"))
         }
       );
  let make = (input, ~onRcv, ~onSnd, ~initialReg) => {
    stack: parse(input),
    stackPos: 0,
    registers: initialReg,
    onRcv,
    onSnd,
    locked: false
  };
  let play = state => {
    let finished =
      state.stackPos > Array.length(state.stack) - 1 || state.stackPos < 0;
    if (finished) {
      {...state, locked: true};
    } else {
      let current = state.stack[state.stackPos];
      let get = getRegister(state.registers);
      let set = setRegister(state.registers);
      switch current {
      | Snd(Int(f)) =>
        state.onSnd(f);
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Snd(Name(n)) =>
        let f = get(n);
        state.onSnd(f);
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Set(n, Int(f)) =>
        set(n, f);
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Set(n, Name(n2)) =>
        set(n, get(n2));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Add(n, Int(f)) =>
        set(n, IntUtils.add(get(n), f));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Add(n, Name(n2)) =>
        set(n, IntUtils.add(get(n), get(n2)));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Multiply(n, Int(f)) =>
        set(n, IntUtils.mul(get(n), f));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Multiply(n, Name(n2)) =>
        set(n, IntUtils.mul(get(n), get(n2)));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Modulo(n, Int(f)) =>
        set(n, IntUtils.modulo(get(n), f));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Modulo(n, Name(n2)) =>
        set(n, IntUtils.modulo(get(n), get(n2)));
        {...state, locked: false, stackPos: state.stackPos + 1};
      | Rcv(Int(f)) => state.onRcv(state, Int(f))
      | Rcv(Name(n)) => state.onRcv(state, Name(n))
      /* | Rcv(_) => {...state, locked: false, stackPos: state.stackPos + 1} */
      | JumpIfGreaterThanZero(Name(n1), Name(n2)) when get(n1) > 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, get(n2))
        }
      | JumpIfGreaterThanZero(Name(n), Int(f)) when get(n) > 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, f)
        }
      | JumpIfGreaterThanZero(Int(f), Name(n2)) when f > 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, get(n2))
        }
      | JumpIfGreaterThanZero(Int(f1), Int(f2)) when f1 > 0 => {
          ...state,
          locked: false,
          stackPos: IntUtils.add(state.stackPos, f2)
        }
      | JumpIfGreaterThanZero(_, _) => {
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
    | Snd(value) => "Snd(" ++ printValue(value) ++ ")"
    | Set(name, value) => "Set(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Add(name, value) => "Add(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Multiply(name, value) =>
      "Mul(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Modulo(name, value) => "Mod(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Rcv(value) => "Rcv(" ++ printValue(value) ++ ")"
    | JumpIfGreaterThanZero(v1, v2) =>
      "JGZ(" ++ printValue(v1) ++ ", " ++ printValue(v2) ++ ")"
    };
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = input => 6;
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = input => 6;
};

let part1 = Part1.solve;

let part2 = Part2.solve;

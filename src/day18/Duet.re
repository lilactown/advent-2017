type frequency = int;

type registers = Js.Dict.t(frequency);

type name = Js.Dict.key;

type value =
  | Name(name)
  | Frequency(frequency);

type instruction =
  | Sound(value)
  | Set(name, value)
  | Add(name, value)
  | Multiply(name, value)
  | Modulo(name, value)
  | Recover(value)
  | JumpIfGreaterThanZero(value, value);

type state = {
  stack: array(instruction),
  stackPos: int,
  lastSound: frequency,
  registers,
  finished: bool,
  recovered: frequency,
  onRcv: frequency => unit
};

let toValue = (s: string) => {
  let c = s.[0];
  switch c {
  | 'a'..'z' => Name(s)
  | _ => Frequency(IntUtils.ofString(s))
  }
};

let getRegister = (registers, name) =>
  switch (Js.Dict.get(registers, name)) {
  | Some(v) => v
  | None => 0
  };

let setRegister = (registers: registers, name: name, value) => Js.Dict.set(registers, name, value);

let parse = (input) : array(instruction) =>
  StringUtils.splitWith("\n", input)
  |> Array.map(StringUtils.splitWith(" "))
  |> Array.map(
       (line) =>
         switch line {
         | [|"snd", x|] => Sound(toValue(x))
         | [|"set", x, y|] => Set(x, toValue(y))
         | [|"add", x, y|] => Add(x, toValue(y))
         | [|"mul", x, y|] => Multiply(x, toValue(y))
         | [|"mod", x, y|] => Modulo(x, toValue(y))
         | [|"rcv", x|] => Recover(toValue(x))
         | [|"jgz", x, y|] => JumpIfGreaterThanZero(toValue(x), toValue(y))
         | _ => raise(Failure("Could not parse instructions"))
         }
     );

let make = (input, ~onRcv) => {
  stack: parse(input),
  stackPos: 0,
  lastSound: 0,
  registers: Js.Dict.empty(),
  finished: false,
  recovered: 0,
  onRcv
};

let play = (state) => {
  let current = state.stack[state.stackPos];
  let get = getRegister(state.registers);
  let set = setRegister(state.registers);
  switch current {
  | Sound(Frequency(f)) => {...state, stackPos: state.stackPos + 1, lastSound: f}
  | Sound(Name(n)) =>
    let f = get(n);
    {...state, stackPos: state.stackPos + 1, lastSound: f}
  | Set(n, Frequency(f)) =>
    set(n, f);
    {...state, stackPos: state.stackPos + 1}
  | Set(n, Name(n2)) =>
    set(n, get(n2));
    {...state, stackPos: state.stackPos + 1}
  | Add(n, Frequency(f)) =>
    set(n, IntUtils.add(get(n), f));
    {...state, stackPos: state.stackPos + 1}
  | Add(n, Name(n2)) =>
    set(n, IntUtils.add(get(n), get(n2)));
    {...state, stackPos: state.stackPos + 1}
  | Multiply(n, Frequency(f)) =>
    set(n, IntUtils.mul(get(n), f));
    {...state, stackPos: state.stackPos + 1}
  | Multiply(n, Name(n2)) =>
    set(n, IntUtils.mul(get(n), get(n2)));
    {...state, stackPos: state.stackPos + 1}
  | Modulo(n, Frequency(f)) =>
    set(n, IntUtils.modulo(get(n), f));
    {...state, stackPos: state.stackPos + 1}
  | Modulo(n, Name(n2)) =>
    set(n, IntUtils.modulo(get(n), get(n2)));
    {...state, stackPos: state.stackPos + 1}
  | Recover(Frequency(f)) when f != 0 =>
    state.onRcv(state.lastSound);
    {...state, stackPos: state.stackPos + 1, recovered: state.lastSound}
  | Recover(Name(n)) when get(n) != 0 =>
    state.onRcv(state.lastSound);
    {...state, stackPos: state.stackPos + 1, recovered: state.lastSound}
  | Recover(_) => {...state, stackPos: state.stackPos + 1}
  | JumpIfGreaterThanZero(Name(n1), Name(n2)) when get(n1) > 0 => {
      ...state,
      stackPos: state.stackPos + get(n2)
    }
  | JumpIfGreaterThanZero(Name(n), Frequency(f)) when get(n) > 0 => {
      ...state,
      stackPos: state.stackPos + f
    }
  | JumpIfGreaterThanZero(Frequency(f), Name(n2)) when f > 0 => {
      ...state,
      stackPos: state.stackPos + get(n2)
    }
  | JumpIfGreaterThanZero(Frequency(f1), Frequency(f2)) when f1 > 0 => {
      ...state,
      stackPos: state.stackPos + f2
    }
  | JumpIfGreaterThanZero(_, _) => {...state, stackPos: state.stackPos + 1}
  }
};

let printValue = (v) =>
  switch v {
  | Name(n) => n
  | Frequency(f) => IntUtils.toString(f)
  };

let print = ({stack, stackPos}) =>
  switch stack[stackPos] {
  | Sound(value) => "Sound(" ++ printValue(value) ++ ")"
  | Set(name, value) => "Set(" ++ name ++ ", " ++ printValue(value) ++ ")"
  | Add(name, value) => "Add(" ++ name ++ ", " ++ printValue(value) ++ ")"
  | Multiply(name, value) => "Mul(" ++ name ++ ", " ++ printValue(value) ++ ")"
  | Modulo(name, value) => "Mod(" ++ name ++ ", " ++ printValue(value) ++ ")"
  | Recover(value) => "Recover(" ++ printValue(value) ++ ")"
  | JumpIfGreaterThanZero(v1, v2) => "JGZ(" ++ printValue(v1) ++ ", " ++ printValue(v2) ++ ")"
  };
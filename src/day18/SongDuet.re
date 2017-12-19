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
  recovered: frequency
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

let make = (input) => {
  stack: parse(input),
  stackPos: 0,
  lastSound: 0,
  registers: Js.Dict.empty(),
  finished: false,
  recovered: 0
};

let play = ({stack, stackPos, lastSound, registers, recovered, finished}) => {
  let current = stack[stackPos];
  let get = getRegister(registers);
  let set = setRegister(registers);
  switch current {
  | Sound(Frequency(f)) => {
      stack,
      stackPos: stackPos + 1,
      lastSound: f,
      registers,
      recovered,
      finished
    }
  | Sound(Name(n)) =>
    let f = get(n);
    {stack, stackPos: stackPos + 1, lastSound: f, registers, recovered, finished}
  | Set(n, Frequency(f)) =>
    set(n, f);
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Set(n, Name(n2)) =>
    set(n, get(n2));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Add(n, Frequency(f)) =>
    set(n, IntUtils.add(get(n), f));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Add(n, Name(n2)) =>
    set(n, IntUtils.add(get(n), get(n2)));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Multiply(n, Frequency(f)) =>
    set(n, IntUtils.mul(get(n), f));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Multiply(n, Name(n2)) =>
    set(n, IntUtils.mul(get(n), get(n2)));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Modulo(n, Frequency(f)) =>
    set(n, IntUtils.modulo(get(n), f));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Modulo(n, Name(n2)) =>
    set(n, IntUtils.modulo(get(n), get(n2)));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Recover(Frequency(f)) when f != 0 => {
      stack,
      stackPos: stackPos + 1,
      lastSound,
      registers,
      recovered: lastSound,
      finished
    }
  | Recover(Name(n)) when get(n) != 0 => {
      stack,
      stackPos: stackPos + 1,
      lastSound,
      registers,
      recovered: lastSound,
      finished
    }
  | Recover(_) => {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | JumpIfGreaterThanZero(Name(n1), Name(n2)) when get(n1) > 0 => {
      stack,
      stackPos: stackPos + get(n2),
      lastSound,
      registers,
      recovered,
      finished
    }
  | JumpIfGreaterThanZero(Name(n), Frequency(f)) when get(n) > 0 => {
      stack,
      stackPos: stackPos + f,
      lastSound,
      registers,
      recovered,
      finished
    }
  | JumpIfGreaterThanZero(Frequency(f), Name(n2)) when f > 0 => {
      stack,
      stackPos: stackPos + get(n2),
      lastSound,
      registers,
      recovered,
      finished
    }
  | JumpIfGreaterThanZero(Frequency(f1), Frequency(f2)) when f1 > 0 => {
      stack,
      stackPos: stackPos + f2,
      lastSound,
      registers,
      recovered,
      finished
    }
  | JumpIfGreaterThanZero(_, _) => {
      stack,
      stackPos: stackPos + 1,
      lastSound,
      registers,
      recovered,
      finished
    }
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
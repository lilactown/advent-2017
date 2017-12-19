type registers = Js.Dict.t(int);

type name = Js.Dict.key;

type value =
  | Name(name)
  | Number(int);

type instruction =
  | Snd(value)
  | Set(name, value)
  | Add(name, value)
  | Multiply(name, value)
  | Modulo(name, value)
  | Rcv(value)
  | JumpIfGreaterThanZero(value, value);

let toValue = (s: string) => {
  let c = s.[0];
  switch c {
  | 'a'..'z' => Name(s)
  | _ => Number(IntUtils.ofString(s))
  }
};

let parse = (input) : array(instruction) =>
  StringUtils.splitWith("\n", input)
  |> Array.map(StringUtils.splitWith(" "))
  |> Array.map(
       (line) =>
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

module type DuetType = {
  type state;
  let getInstruction: state => instruction;
  let snd: (state, value) => state;
  let set: (state, name, value) => state;
  let add: (state, name, value) => state;
  let multiply: (state, name, value) => state;
  let modulo: (state, name, value) => state;
  let rcv: (state, value) => state;
  let jgz: (state, value, value) => state;
  let make: string => state;
  let getLastRcvd: state => int;
};

module Make = (D: DuetType) => {
  type state = D.state;
  let make: string => state = D.make;
  let play = (state: state) =>
    switch (D.getInstruction(state)) {
    | Snd(x) => D.snd(state, x)
    | Set(x, y) => D.set(state, x, y)
    | Add(x, y) => D.add(state, x, y)
    | Multiply(x, y) => D.multiply(state, x, y)
    | Modulo(x, y) => D.modulo(state, x, y)
    | Rcv(x) => D.rcv(state, x)
    | JumpIfGreaterThanZero(x, y) => D.jgz(state, x, y)
    };
  let printValue = (v) =>
    switch v {
    | Name(n) => n
    | Number(f) => IntUtils.toString(f)
    };
  let print = (state: state) =>
    switch (D.getInstruction(state)) {
    | Snd(value) => "Snd(" ++ printValue(value) ++ ")"
    | Set(name, value) => "Set(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Add(name, value) => "Add(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Multiply(name, value) => "Mul(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Modulo(name, value) => "Mod(" ++ name ++ ", " ++ printValue(value) ++ ")"
    | Rcv(value) => "Rcv(" ++ printValue(value) ++ ")"
    | JumpIfGreaterThanZero(v1, v2) => "JGZ(" ++ printValue(v1) ++ ", " ++ printValue(v2) ++ ")"
    };
  let getLastRcvd = D.getLastRcvd;
};
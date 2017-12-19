type state = {
  stack: array(Duet.instruction),
  stackPos: int,
  lastSound: int,
  registers: Duet.registers,
  finished: bool,
  recovered: int
};

let make = (input) => {
  stack: Duet.parse(input),
  stackPos: 0,
  lastSound: 0,
  registers: Js.Dict.empty(),
  finished: false,
  recovered: 0
};

let getInstruction: state => Duet.instruction = (state) => state.stack[state.stackPos];

let getRegister = (registers, name) =>
  switch (Js.Dict.get(registers, name)) {
  | Some(v) => v
  | None => 0
  };

let setRegister = (registers: Duet.registers, name: Duet.name, value) =>
  Js.Dict.set(registers, name, value);

let snd = ({stack, stackPos, registers, recovered, finished}, value) =>
  switch value {
  | Duet.Name(s) =>
    let f = getRegister(registers, s);
    {stack, stackPos: stackPos + 1, lastSound: f, registers, recovered, finished}
  | Duet.Number(_) => raise(Failure("`snd` does not take a number"))
  };

let set = ({stack, stackPos, lastSound, registers, recovered, finished}, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(registers, name, getRegister(registers, s));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Duet.Number(n) =>
    setRegister(registers, name, n);
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  };

let add = ({stack, stackPos, lastSound, registers, recovered, finished}, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(
      registers,
      name,
      IntUtils.add(getRegister(registers, name), getRegister(registers, s))
    );
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Duet.Number(n) =>
    setRegister(registers, name, IntUtils.add(getRegister(registers, name), n));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  };

let multiply = ({stack, stackPos, lastSound, registers, recovered, finished}, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(
      registers,
      name,
      IntUtils.mul(getRegister(registers, name), getRegister(registers, s))
    );
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Duet.Number(n) =>
    setRegister(registers, name, IntUtils.mul(getRegister(registers, name), n));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  };

let modulo = ({stack, stackPos, lastSound, registers, recovered, finished}, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(
      registers,
      name,
      IntUtils.modulo(getRegister(registers, name), getRegister(registers, s))
    );
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  | Duet.Number(n) =>
    setRegister(registers, name, IntUtils.modulo(getRegister(registers, name), n));
    {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  };

let rcv = ({stack, stackPos, lastSound, registers, recovered, finished}, value) =>
  switch value {
  | Duet.Number(n) when n != 0 => {
      stack,
      stackPos: stackPos + 1,
      lastSound,
      registers,
      recovered: lastSound,
      finished
    }
  | Duet.Name(s) when getRegister(registers, s) != 0 => {
      stack,
      stackPos: stackPos + 1,
      lastSound,
      registers,
      recovered: lastSound,
      finished
    }
  | _ => {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  };

let jgz = ({stack, stackPos, lastSound, registers, recovered, finished}, v1, v2) =>
  switch (v1, v2) {
  | (Duet.Name(n1), Duet.Name(n2)) when getRegister(registers, n1) > 0 => {
      stack,
      stackPos: stackPos + getRegister(registers, n2),
      lastSound,
      registers,
      recovered,
      finished
    }
  | (Duet.Name(n), Duet.Number(f)) when getRegister(registers, n) > 0 => {
      stack,
      stackPos: stackPos + f,
      lastSound,
      registers,
      recovered,
      finished
    }
  | (Duet.Number(f), Duet.Name(n2)) when f > 0 => {
      stack,
      stackPos: stackPos + getRegister(registers, n2),
      lastSound,
      registers,
      recovered,
      finished
    }
  | (Duet.Number(f1), Duet.Number(f2)) when f1 > 0 => {
      stack,
      stackPos: stackPos + f2,
      lastSound,
      registers,
      recovered,
      finished
    }
  | (_, _) => {stack, stackPos: stackPos + 1, lastSound, registers, recovered, finished}
  };
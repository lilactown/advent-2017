type state = {
  stack: array(Duet.instruction),
  stackPos: int,
  lastSound: int,
  registers: Duet.registers,
  finished: bool,
  lastRcvd: int,
  onRcv: int => unit
};

let make = (input, ~onRcv) => {
  stack: Duet.parse(input),
  stackPos: 0,
  lastSound: 0,
  registers: Js.Dict.empty(),
  finished: false,
  lastRcvd: 0,
  onRcv
};

let getInstruction: state => Duet.instruction = (state) => state.stack[state.stackPos];

let getRegister = (registers, name) =>
  switch (Js.Dict.get(registers, name)) {
  | Some(v) => v
  | None => 0
  };

let setRegister = (registers: Duet.registers, name: Duet.name, value) =>
  Js.Dict.set(registers, name, value);

let snd = (state, value) =>
  switch value {
  | Duet.Name(s) =>
    let f = getRegister(state.registers, s);
    {...state, stackPos: state.stackPos + 1, lastSound: f}
  | Duet.Number(_) => raise(Failure("`snd` does not take a number"))
  };

let set = (state, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(state.registers, name, getRegister(state.registers, s));
    {...state, stackPos: state.stackPos + 1}
  | Duet.Number(n) =>
    setRegister(state.registers, name, n);
    {...state, stackPos: state.stackPos + 1}
  };

let add = (state, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(
      state.registers,
      name,
      IntUtils.add(getRegister(state.registers, name), getRegister(state.registers, s))
    );
    {...state, stackPos: state.stackPos + 1}
  | Duet.Number(n) =>
    setRegister(state.registers, name, IntUtils.add(getRegister(state.registers, name), n));
    {...state, stackPos: state.stackPos + 1}
  };

let multiply = (state, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(
      state.registers,
      name,
      IntUtils.mul(getRegister(state.registers, name), getRegister(state.registers, s))
    );
    {...state, stackPos: state.stackPos + 1}
  | Duet.Number(n) =>
    setRegister(state.registers, name, IntUtils.mul(getRegister(state.registers, name), n));
    {...state, stackPos: state.stackPos + 1}
  };

let modulo = (state, name, value) =>
  switch value {
  | Duet.Name(s) =>
    setRegister(
      state.registers,
      name,
      IntUtils.modulo(getRegister(state.registers, name), getRegister(state.registers, s))
    );
    {...state, stackPos: state.stackPos + 1}
  | Duet.Number(n) =>
    setRegister(state.registers, name, IntUtils.modulo(getRegister(state.registers, name), n));
    {...state, stackPos: state.stackPos + 1}
  };

let rcv = (state, value) =>
  switch value {
  | Duet.Number(n) when n != 0 =>
    state.onRcv(state.lastSound);
    {...state, stackPos: state.stackPos + 1, lastRcvd: state.lastSound}
  | Duet.Name(s) when getRegister(state.registers, s) != 0 =>
    state.onRcv(state.lastSound);
    {...state, stackPos: state.stackPos + 1, lastRcvd: state.lastSound}
  | _ => {...state, stackPos: state.stackPos + 1}
  };

let jgz = (state, v1, v2) =>
  switch (v1, v2) {
  | (Duet.Name(n1), Duet.Name(n2)) when getRegister(state.registers, n1) > 0 => {
      ...state,
      stackPos: state.stackPos + getRegister(state.registers, n2)
    }
  | (Duet.Name(n), Duet.Number(f)) when getRegister(state.registers, n) > 0 => {
      ...state,
      stackPos: state.stackPos + f
    }
  | (Duet.Number(f), Duet.Name(n2)) when f > 0 => {
      ...state,
      stackPos: state.stackPos + getRegister(state.registers, n2)
    }
  | (Duet.Number(f1), Duet.Number(f2)) when f1 > 0 => {...state, stackPos: state.stackPos + f2}
  | (_, _) => {...state, stackPos: state.stackPos + 1}
  };

let getLastRcvd = (state) => state.lastRcvd;
module Context = {
  type t = Js.Dict.t(int);
  let unwrapRegisterValue = (register) =>
    switch register {
    | None => 0
    | Some(value) => value
    };
  let set = (context: t, register: string) => Js.Dict.set(context, register);
  let get = (context: t, register: string) => unwrapRegisterValue(Js.Dict.get(context, register));
  let values = (context: t) => Js.Dict.values(context);
  let make: unit => t = Js.Dict.empty;
};

let assertPredicate = (context, register, predicate, amount) => {
  let value = Context.get(context, register);
  switch predicate {
  | ">" => value > amount
  | "<" => value < amount
  | ">=" => value >= amount
  | "<=" => value <= amount
  | "==" => value == amount
  | "!=" => value != amount
  | _ => raise(Failure("Could not parse predicate"))
  }
};

let doOp = (context, register, op, amount) => {
  let value = Context.get(context, register);
  switch op {
  | "inc" => value + amount
  | "dec" => value - amount
  | _ => raise(Failure("Could not parse operation"))
  }
};
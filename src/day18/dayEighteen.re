module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    ({|set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2|}, 4)
  ];
  let solve = (input) => {
    let stack = [||];
    let break = ref(false);
    let duet =
      ref(
        Duet.make(
          input,
          ~onRcv=(state: Duet.state, _) => {...state, locked: true},
          ~onSnd=(n) => Js.Array.push(n, stack) |> ignore,
          ~initialReg=Js.Dict.empty()
        )
      );
    while (! duet^.locked) {
      duet := Duet.play(duet^)
    };
    stack[Array.length(stack) - 1]
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d|}, 3)];
  let playUntil = (duet: Duet.state) => {
    let state = ref(duet);
    while (! state^.locked) {
      state := Duet.play(state^)
    };
    state^
  };
  let recieve = (q, state: Duet.state, value) =>
    switch (value, Queue.dequeue(q)) {
    | (Duet.Name(n), Some(v)) =>
      Js.log3("recieving:", v, state.stackPos);
      Duet.setRegister(state.registers, n, v);
      {...state, locked: false, stackPos: state.stackPos + 1}
    | (Duet.Name(_), None) =>
      Js.log("waiting");
      {...state, locked: true}
    | _ => raise(Failure("Invalid rcv instruction"))
    };
  let solve = (input) => {
    let count = ref(0);
    let q0 = Queue.make(0);
    let q1 = Queue.make(0);
    let program0 =
      ref(
        Duet.make(
          input,
          ~onSnd=
            (n) => {
              Js.log2("sending from 0:", n);
              Queue.enqueue(q1, n) |> ignore
            },
          ~onRcv=recieve(q0),
          ~initialReg=Js.Dict.fromList([("p", 0)])
        )
      );
    let program1 =
      ref(
        Duet.make(
          input,
          ~onSnd=
            (n) => {
              Js.log2("sending from 1:", n);
              Queue.enqueue(q0, n) |> ignore;
              count := count^ + 1
            },
          ~onRcv=recieve(q1),
          ~initialReg=Js.Dict.fromList([("p", 1)])
        )
      );
    let break = ref(false);
    while (! break^) {
      program0 := playUntil(program0^);
      program1 := playUntil(program1^);
      program0 := Duet.play(program0^);
      if (program0^.locked && program1^.locked) {
        break := true
      }
    };
    count^
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
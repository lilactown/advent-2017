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
    let q = Queue.make(0);
    let duet = ref(Duet.make(input, ~onRcv=(n) => Queue.enqueue(q, n) |> ignore));
    let break = ref(false);
    while (! break^) {
      duet := Duet.play(duet^);
      switch (Queue.peek(q)) {
      | Some(_) => break := true
      | None => ()
      }
    };
    OptionUtils.unsafeUnwrap(Queue.dequeue(q))
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = (input) => 6;
};

let part1 = Part1.solve;

let part2 = Part2.solve;
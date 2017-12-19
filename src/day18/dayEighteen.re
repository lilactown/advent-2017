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
  module ReallySongDuet = Duet.Make(Song);
  let solve = (input) => {
    let duet = ReallySongDuet.make(input);
    let break = ref(false);
    let state = ref(duet);
    let recovered = ref(0);
    while (! break^) {
      let newState: ReallySongDuet.state = ReallySongDuet.play(state^);
      if (ReallySongDuet.getLastRcvd(newState) != 0) {
        recovered := ReallySongDuet.getLastRcvd(newState);
        state := newState;
        break := true
      } else {
        state := newState
      }
    };
    recovered^
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
module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|..#
#..
...|}, 5587)];
  let solve = input => {
    let seed =
      StringUtils.splitWith("\n", input)
      |> Array.map(StringUtils.split)
      |> Array.map(
           Array.map(k =>
             switch k {
             | "#" => true
             | "." => false
             | _ => raise(Failure("Could not parse"))
             }
           )
         );
    let virus = ref(Virus.make(seed));
    for (_ in 1 to 10_000) {
      virus := Virus.burst(virus^);
    };
    virus^.infected;
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|..#
#..
...|}, 2511944)];
  let solve = input => {
    let seed =
      StringUtils.splitWith("\n", input)
      |> Array.map(StringUtils.split)
      |> Array.map(
           Array.map(k =>
             switch k {
             | "#" => EvolvedVirus.Infected
             | "." => EvolvedVirus.Clean
             | _ => raise(Failure("Could not parse"))
             }
           )
         );
    let virus = ref(EvolvedVirus.make(seed));
    for (_ in 1 to 10_000_000) {
      virus := EvolvedVirus.burst(virus^);
    };
    virus^.infected;
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;

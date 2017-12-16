let log = (k) => {
  Js.log(Disk.printRegion(~x=0, ~y=0, ~size=8, k));
  k
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("flqrgnkx", 8108)];
  let solve = (input) =>
    Disk.make(input) |> log |> Disk.flatten |> Js.Array.filter(Disk.eltToBool) |> Array.length;
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("flqrgnkx", 1242)];
  let solve = (input) => {
    let disk = Disk.make(input);
    let size = Disk.size(disk);
    let group = ref(0);
    for (y in 0 to size - 1) {
      for (x in 0 to size - 1) {
        switch (Disk.get(x, y, disk)) {
        | None => ()
        | Some(v) when v == Used =>
          group := group^ + 1;
          Disk.unsafe_changeUsedNeighbors((_) => Group(group^), x, y, disk)
        | _ => ()
        }
      }
    };
    log(disk) |> ignore;
    group^
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;
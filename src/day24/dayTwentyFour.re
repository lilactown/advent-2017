let makePart = s => {
  let ss = StringUtils.splitWith("/", s);
  switch ss {
  | [|s0, s1|] => (int_of_string(s0), int_of_string(s1))
  | _ => raise(Failure("makePart called with invalid arg: " ++ s))
  };
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10|}, 31)];
  let rec search = (parts, ~cur=0, ~strength=0, ()) =>
    Array.(
      ArrayUtils.(
        parts
        |> filter(~f=((i, o)) => i == cur || o == cur)
        |> map(x =>
             search(
               remove(~f=x' => x' == x, parts),
               ~cur=fst(x) == cur ? snd(x) : fst(x),
               ~strength=strength + fst(x) + snd(x),
               ()
             )
           )
        |> append([|strength|])
        |> IntUtils.max
      )
    );
  let solve = input => {
    let parts = StringUtils.splitWith("\n", input) |> Array.map(makePart);
    search(parts, ());
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10|}, 19)];
  let rec search = (parts, ~cur=0, ~strength=0, ~len=0, ()) =>
    Array.(
      ArrayUtils.(
        parts
        |> filter(~f=((i, o)) => i == cur || o == cur)
        |> map(x =>
             search(
               remove(~f=x' => x' == x, parts),
               ~cur=fst(x) == cur ? snd(x) : fst(x),
               ~strength=strength + fst(x) + snd(x),
               ~len=len + 1,
               ()
             )
           )
        |> append([|(strength, len)|])
        |> sort(~f=((str1, len1), (str2, len2)) =>
             if (len1 < len2) {
               1;
             } else if (len1 > len2) {
               (-1);
             } else if (str1 < str2) {
               1;
             } else if (str1 > str2) {
               (-1);
             } else {
               0;
             }
           )
        |> (x => x[0])
      )
    );
  let solve = input => {
    let parts = StringUtils.splitWith("\n", input) |> Array.map(makePart);
    fst(search(parts, ()));
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;

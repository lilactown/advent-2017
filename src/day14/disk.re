type elt =
  | Free
  | Used
  | Group(int);

type t = array(array(elt));

let eltToBool = (elt) =>
  switch elt {
  | Free => false
  | Used => true
  | _ => raise(Failure("Group used in part 1!"))
  };

let printRegion = (~x, ~y, ~size, disk: t) =>
  (
    Array.init(
      size,
      (i) =>
        Array.init(
          size,
          (j) =>
            switch disk[y + i][x + j] {
            | Used => "  #"
            | Free => "  ."
            | Group(n) => StringUtils.padStart(~length=3, ~padWith=" ", string_of_int(n))
            }
        )
        |> Js.Array.joinWith(" ")
    )
    |> Js.Array.joinWith("\n")
  )
  ++ "\n";

let flatten: t => array(elt) = ArrayUtils.concat;

let size: t => int = Array.length;

let make = (input) : t =>
  Array.init(128, (i) => input ++ "-" ++ string_of_int(i))
  |> Array.map(
       (r) =>
         KnotHash.make(r)
         |> Js.String.split("")
         /* TODO: Improve performance here */
         |> Array.map(IntUtils.hexToBinString)
         |> Array.map(Js.String.split(""))
         |> ArrayUtils.concat
         |> Array.map(
              (b) =>
                switch b {
                | "0" => Free
                | "1" => Used
                | _ => raise(Failure("Could not parse input"))
                }
            )
     );

let get = (x, y, disk: t) =>
  switch disk[y][x] {
  | exception _ => None
  | el => Some(el)
  };

let printElt = (elt) =>
  switch elt {
  | Used => "Used"
  | Free => "Free"
  | Group(n) => "Group(" ++ string_of_int(n) ++ ")"
  };

let rec unsafe_changeUsedNeighbors = (tf: elt => 'a, x, y, disk) =>
  switch (get(x, y, disk)) {
  | Some(elt) when elt == Used =>
    disk[y][x] = tf(elt);
    unsafe_changeUsedNeighbors(tf, x + 0, y + 1, disk);
    unsafe_changeUsedNeighbors(tf, x + 0, y - 1, disk);
    unsafe_changeUsedNeighbors(tf, x + 1, y + 0, disk);
    unsafe_changeUsedNeighbors(tf, x - 1, y + 0, disk)
  | _ => ()
  };
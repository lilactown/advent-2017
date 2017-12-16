let flattenArray: array(array('a)) => array('a) = [%bs.raw {|([s, ...a]) => s.concat(...a)|}];

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
            | Group(n) => KnotHash.padStart(~length=3, ~padWith=" ", string_of_int(n))
            }
        )
        |> Js.Array.joinWith(" ")
    )
    |> Js.Array.joinWith("\n")
  )
  ++ "\n";

let hexToDec = (s) => int_of_string("0x" ++ s);

let decToBin = (n) => {
  let rec toBin = (k) =>
    switch (k / 2, k mod 2) {
    | (0, d) => string_of_int(d)
    | (k', d) => toBin(k') ++ string_of_int(d)
    };
  KnotHash.padStart(~length=4, ~padWith="0", toBin(n))
};

let hexToBin = (s) => hexToDec(s) |> decToBin;

let flatten: t => array(elt) = [%bs.raw {|([s, ...a]) => s.concat(...a)|}];

let size: t => int = Array.length;

let make = (input) : t =>
  Array.init(128, (i) => input ++ "-" ++ string_of_int(i))
  |> Array.map(
       (r) =>
         KnotHash.make(r)
         |> Js.String.split("")
         |> Array.map(hexToBin)
         |> Array.map(Js.String.split(""))
         |> flattenArray
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
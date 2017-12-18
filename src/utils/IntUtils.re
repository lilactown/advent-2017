type binary = string;

/* BuckleScript maintains OCaml's 32-bit precision in integers
   which ends up truncating a lot of numbers - so we define our
   own platform-specific integer operations here */
let add: (int, int) => int = [%bs.raw {| (a, b) => a + b |}];

let mul: (int, int) => int = [%bs.raw {| (a, b) => a * b|}];

let modulo: (int, int) => int = [%bs.raw {| (a, b) => a % b|}];

let maxi = (numbers) => {
  let (max, index, _) =
    Array.fold_left(
      ((curMax, lastMaxIndex, index), n: int) =>
        curMax >= n ? (curMax, lastMaxIndex, index + 1) : (n, index, index + 1),
      (numbers[0], 0, 0),
      numbers
    );
  (max, index)
};

let max = (numbers) => fst(maxi(numbers));

let abs = Js.Math.abs_int;

let toHex = (n) => StringUtils.padStart(Printf.sprintf("%x", n), ~length=2, ~padWith="0");

let xorMany = Array.fold_left((accum, n) => accum lxor n, 0);

let hexToDec = (s) => int_of_string("0x" ++ s);

let decToBinString = (~bits=32, n) : binary => {
  let rec toBin = (k) =>
    switch (k / 2, k mod 2) {
    | (0, d) => string_of_int(d)
    | (k', d) => toBin(k') ++ string_of_int(d)
    };
  StringUtils.padStart(~length=bits, ~padWith="0", toBin(n))
};

let hexToBinString = (s) => hexToDec(s) |> decToBinString(~bits=4);

[@bs.send] external toString : int => string = "";

let ofString: string => int = [%bs.raw {|(a) => parseInt(a, 10)|}];
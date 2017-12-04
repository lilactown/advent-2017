module StringSet = Set.Make(String);

let arrayToStringSet = (arr) => StringSet.of_list(Array.to_list(arr));

let noDuplicates = (pass) => {
  let phrases = Js.String.split(" ", pass);
  StringSet.cardinal(arrayToStringSet(phrases)) == Array.length(phrases)
};

let part1 = (input) =>
  Array.length(
    Js.String.split("\n", input) |> Array.map(noDuplicates) |> Js.Array.filter((v) => v)
  );

module Anagram = {
  type t = string;
  let compare = (s1, s2) => {
    let sa1 = Js.Array.join(Js.Array.sortInPlace(Js.String.split("", s1)));
    let sa2 = Js.Array.join(Js.Array.sortInPlace(Js.String.split("", s2)));
    sa1 == sa2 ? 0 : sa1 < sa2 ? (-1) : 1
  };
};

module AnagramSet = Set.Make(Anagram);

let arrayToAnagramSet = (ss) => AnagramSet.of_list(Array.to_list(ss));

let noAnagrams = (pass) => {
  let phrases = Js.String.split(" ", pass);
  AnagramSet.cardinal(arrayToAnagramSet(phrases)) == Array.length(phrases)
};

let part2 = (input) =>
  Array.length(Js.String.split("\n", input) |> Array.map(noAnagrams) |> Js.Array.filter((v) => v));
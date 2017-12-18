[@bs.send] external padStart : (string, ~length: int, ~padWith: string) => string = "padStart";

let explode = (s) => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l
    } else {
      exp(i - 1, [s.[i], ...l])
    };
  exp(String.length(s) - 1, [])
};

let splitWith = Js.String.split;

let split = splitWith("");
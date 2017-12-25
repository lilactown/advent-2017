/**
 * Array.find
 * Array.includes
 * Array.every
 * Array.some
 * Array.join/joinWith
 * Array.filter
 */
let insert = (array, n, v) =>
  Array.init(Array.length(array) + 1, i =>
    if (i == n) {
      v;
    } else if (i > n) {
      array[i - 1];
    } else {
      array[i];
    }
  );

let concat = arrays => {
  let newArray = ref([||]);
  for (i in 0 to Array.length(arrays) - 1) {
    newArray := Array.append(newArray^, arrays[i]);
  };
  newArray^;
};

/* let concat = [%bs.raw "([hd, ...tl]) => hd.concat(...tl)"]; */
let sub = (array, ~start, ~length) =>
  Array.init(length, i => array[i + start]);

let replace = (array, ~start, ~withArray) => {
  let newArray = Array.copy(array);
  for (pos in 0 to Array.length(withArray) - 1) {
    newArray[start + pos] = withArray[pos];
  };
  newArray;
};

let sum = Array.fold_left((total, n) => total + n);

let copy = Js.Array.copy;

let reverse = arr => copy(arr) |> Js.Array.reverseInPlace;

let zip = (a, b) => Array.mapi((i, el_a) => [|el_a, b[i]|], a);

let zip3 = (a, b, c) => Array.init(Array.length(a), i => [|a[i], b[i], c[i]|]);

let zip4 = (a, b, c, d) =>
  Array.init(Array.length(a), i => [|a[i], b[i], c[i], d[i]|]);

let partition = (length, arr) =>
  Array.init(Array.length(arr) / length, i =>
    sub(arr, ~start=i * length, ~length)
  );

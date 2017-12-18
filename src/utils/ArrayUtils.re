/**
 * Array.find
 * Array.includes
 * Array.every
 * Array.some
 * Array.join/joinWith
 * Array.filter
 */
let insert = (array, n, v) =>
  Array.init(
    Array.length(array) + 1,
    (i) =>
      if (i == n) {
        v
      } else if (i > n) {
        array[i - 1]
      } else {
        array[i]
      }
  );

let concat = (arrays) => {
  let newArray = ref([||]);
  for (i in 0 to Array.length(arrays) - 1) {
    newArray := Array.append(newArray^, arrays[i])
  };
  newArray^
};

/* let concat = [%bs.raw "([hd, ...tl]) => hd.concat(...tl)"]; */
let sub = (array, ~start, ~length) => {
  let newArray = Array.make(length, 0);
  let get' = Array.get(array);
  for (position in start to start + length - 1) {
    newArray[position - start] = get'(position)
  };
  newArray
};

let replace = (array, ~start, ~withArray) => {
  let newArray = Array.copy(array);
  for (pos in 0 to Array.length(withArray) - 1) {
    newArray[start + pos] = withArray[pos]
  };
  newArray
};

let sum = Array.fold_left((total, n) => total + n);
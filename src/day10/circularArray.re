/* include Array; */
type t('a) = array('a);

let make: (int, 'a) => t('a) = Array.make;

let copy: t('a) => t('a) = Array.copy;

let computePos = (array: t('a), n) => n mod Array.length(array);

let get = (array: t('a), n) => array[computePos(array, n)];

let set = (array: t('a), n, v) => array[computePos(array, n)] = v;

let length: t('a) => int = Array.length;

let insert = (array: t('a), n, v) : t('a) =>
  Array.init(
    length(array) + 1,
    (i) =>
      if (i == n) {
        v
      } else if (i > n) {
        array[i - 1]
      } else {
        array[i]
      }
  );

let rev = Js.Array.reverseInPlace;

let sub = (array, ~start, ~length) => {
  let newArray: t('a) = make(length, 0);
  let get' = get(array);
  for (position in start to start + length - 1) {
    newArray[position - start] = get'(position)
  };
  newArray
};

let replace = (array: t('a), ~start, ~withArray: t('a)) => {
  let newArray: t('a) = copy(array);
  let start' = computePos(newArray, start);
  for (pos in 0 to length(withArray) - 1) {
    set(newArray, start' + pos, withArray[pos])
  };
  newArray
};
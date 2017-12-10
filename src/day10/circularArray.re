include Array;

type t('a) = array('a);

let computePos = (array: t('a), n) => n mod length(array);

let get = (array: t('a), n) => array[computePos(array, n)];

let set = (array: t('a), n, v) => array[computePos(array, n)] = v;

let rev = Js.Array.reverseInPlace;

let sub = (array, start, end_) => {
  let length = end_ - start;
  let newArray: t('a) = make(length, 0);
  let get' = get(array);
  for (position in start to end_ - 1) {
    newArray[position - start] = get'(position)
  };
  newArray
};

let replace = (array: t('a), start, subArray: t('a)) => {
  let newArray: t('a) = copy(array);
  let start' = computePos(newArray, start);
  for (pos in 0 to length(subArray) - 1) {
    set(newArray, start' + pos, subArray[pos])
  };
  newArray
};
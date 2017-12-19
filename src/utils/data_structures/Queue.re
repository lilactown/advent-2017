type t('a) = array('a);

let make = (a) : t('a) => Array.make(0, a);

let enqueue = (q: t('a), e: 'a) => {
  Js.Array.push(e, q) |> ignore;
  q
};

let dequeue = (q: t('a)) => Js.Array.shift(q);

let peek = (q: t('a)) =>
  switch q[Array.length(q) - 1] {
  | exception _ => None
  | a => Some(a)
  };
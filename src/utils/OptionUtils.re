let unsafeUnwrap = (option) =>
  switch option {
  | None => raise(Not_found)
  | Some(k) => k
  };
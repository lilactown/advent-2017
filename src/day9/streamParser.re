type t =
  | None
  | Group(int, t)
  | Garbage(int, t)
  | Canceled(t);

let rec debugState = (t) =>
  switch t {
  | Group(n, t) => "Group (" ++ string_of_int(n) ++ ", " ++ debugState(t) ++ ")"
  | Garbage(n, t) => "Garbage (" ++ string_of_int(n) ++ ", " ++ debugState(t) ++ ")"
  | Canceled(t) => "Canceled (" ++ debugState(t) ++ ")"
  | None => ""
  };

let parse = (stream) => {
  let rec tokenizer = (input, state, groups) =>
    switch input {
    | [] => List.rev(groups)
    | [head, ...tail] =>
      let next = tokenizer(tail);
      /*The order of this switch statement is very important.
        We make sure to match on (_, Canceled) before
        anything else that matches on "any character", since canceling
        has highest precedence (including over itself, e.g. "!!")
        Next we make sure to match on (_, Garbage) before (<, state)
        to preserve the fact that we don't nest Garbage. */
      switch (head, state, groups) {
      | ('{', None, groups) => next(Group(1, None), [Group(1, None), ...groups])
      | ('}', Group(1, None), groups) => List.rev(groups)
      | ('{', Group(n, prevState), groups) =>
        /* Put the new Group state on the list */
        let newState = Group(n + 1, Group(n, prevState));
        next(newState, [newState, ...groups])
      | ('}', Group(_, prevState), groups) => next(prevState, groups)
      | (_, Canceled(prevState), groups) => next(prevState, groups)
      | ('!', state, groups) => next(Canceled(state), groups)
      | ('>', Garbage(n, prevState), groups) =>
        /* We're done counting characters - put it in the list */
        next(prevState, [Garbage(n, prevState), ...groups])
      | (_, Garbage(n, state), groups) => next(Garbage(n + 1, state), groups)
      | ('<', state, groups) => next(Garbage(0, state), groups)
      | (_, state, groups) => next(state, groups)
      }
    };
  tokenizer(stream, None, [])
};

let sumGroups =
  List.fold_left(
    (total, state) =>
      switch state {
      | Group(n, _) => total + n
      | _ => total
      },
    0
  );

let sumGarbage =
  List.fold_left(
    (total, state) =>
      switch state {
      | Garbage(n, _) => total + n
      | _ => total
      },
    0
  );
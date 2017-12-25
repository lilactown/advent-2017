type t('a) = array(array('a));

let size: t('a) => int = Array.length;

let rotateClockwise = (grid: t('a)) : t('a) => {
  let grid = ArrayUtils.reverse(grid);
  switch (size(grid)) {
  | 2 => ArrayUtils.zip(grid[0], grid[1])
  | 3 => ArrayUtils.zip3(grid[0], grid[1], grid[2])
  | 4 => ArrayUtils.zip4(grid[0], grid[1], grid[2], grid[3])
  | _ =>
    Js.log(grid);
    raise(Failure("Rotation only implemented for sizes 3 and 4"));
  };
};

let rotateCounterClockwise = (grid: t('a)) : t('a) =>
  rotateClockwise(grid) |> ArrayUtils.reverse;

let flip = (grid: t('a)) : t('a) => Array.map(ArrayUtils.reverse, grid);

let col = (grid, n) => Array.init(size(grid), i => grid[i][n]);

let cols = (grid, ~start, ~length) =>
  Array.init(size(grid), i => ArrayUtils.sub(grid[i], ~start, ~length));

let rows = ArrayUtils.sub;

/* I stole this from /u/hardmathproblem */
let flatten = (grids: t(t('a))) : t('a) => {
  let n = Array.length(grids);
  let j = size(grids[0][0]);
  let size = n * j;
  let state = Array.make_matrix(size, size, grids[0][0][0][0]);
  Array.iteri(
    (y, row) =>
      Array.iteri(
        (x, grid) =>
          Array.iteri(
            (y', row') =>
              Array.iteri(
                (x', value) => state[y * j + y'][x * j + x'] = value,
                row'
              ),
            grid
          ),
        row
      ),
    grids
  );
  state;
};

let partitionBy2 = (grid: t('a)) : t(t('a)) => {
  let rows = Array.map(ArrayUtils.partition(2), grid);
  Array.init(size(grid) / 2, i =>
    ArrayUtils.zip(rows[i * 2], rows[i * 2 + 1])
  );
};

let partitionBy3 = (grid: t('a)) : t(t('a)) => {
  let rows = Array.map(ArrayUtils.partition(3), grid);
  Array.init(size(grid) / 3, i =>
    ArrayUtils.zip3(rows[i * 3], rows[i * 3 + 1], rows[i * 3 + 2])
  );
};

let neighbors = (grid, ~x, ~y) => {
  let get = (x, y) =>
    switch grid[y][x] {
    | v => Some(v)
    | exception _ => None
    };
  (
    get(x - 1, y + 1),
    get(x, y + 1),
    get(x + 1, y + 1),
    get(x - 1, y),
    get(x + 1, y),
    get(x - 1, y - 1),
    get(x, y - 1),
    get(x + 1, y - 1)
  );
};

let toString = (grid: t('a), ~f) =>
  Array.mapi(
    (i, line) => Js.Array.joinWith("", Array.mapi((j, e) => f(j, i, e), line)),
    grid
  )
  |> Js.Array.joinWith("\n");

let test1 =
  flatten(
    partitionBy3([|
      [|1, 1, 1, 1, 1, 1, 1, 1, 1|],
      [|2, 2, 2, 2, 2, 2, 2, 2, 2|],
      [|3, 3, 3, 3, 3, 3, 3, 3, 3|],
      [|4, 4, 4, 4, 4, 4, 4, 4, 4|],
      [|5, 5, 5, 5, 5, 5, 5, 5, 5|],
      [|6, 6, 6, 6, 6, 6, 6, 6, 6|],
      [|7, 7, 7, 7, 7, 7, 7, 7, 7|],
      [|8, 8, 8, 8, 8, 8, 8, 8, 8|],
      [|9, 9, 9, 9, 9, 9, 9, 9, 9|]
    |])
  );

let test2 =
  partitionBy2([|
    [|1, 1, 1, 1, 1, 1, 1, 1|],
    [|2, 2, 2, 2, 2, 2, 2, 2|],
    [|3, 3, 3, 3, 3, 3, 3, 3|],
    [|4, 4, 4, 4, 4, 4, 4, 4|],
    [|5, 5, 5, 5, 5, 5, 5, 5|],
    [|6, 6, 6, 6, 6, 6, 6, 6|],
    [|7, 7, 7, 7, 7, 7, 7, 7|],
    [|8, 8, 8, 8, 8, 8, 8, 8|]
  |])
  |> flatten;

let test3 =
  partitionBy2([|
    [|1, 1, 1, 1|],
    [|2, 2, 2, 2|],
    [|3, 3, 3, 3|],
    [|4, 4, 4, 4|]
  |])
  |> flatten;

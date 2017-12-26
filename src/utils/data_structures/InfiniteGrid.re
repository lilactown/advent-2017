include Grid;

let grow = (grid, ~padAmount, ~default) =>
  ArrayUtils.padBothF(
    ~padAmount,
    ~f=(_) => Array.make(Grid.size(grid) + padAmount * 2, default),
    Array.map(ArrayUtils.padBoth(~padAmount, ~withEl=default), grid)
  );

let get = (grid: Grid.t('a), ~x, ~y, ~default: 'a) =>
  switch grid[y][x] {
  | v => (v, x, y, grid)
  | exception _ =>
    let size = Grid.size(grid) - 1;
    let xGrow =
      if (x < 0) {
        - x;
      } else {
        x - size;
      };
    let yGrow =
      if (y < 0) {
        - y;
      } else {
        y - size;
      };
    let padAmount = max(xGrow, yGrow);
    let newX = x + padAmount;
    let newY = y + padAmount;
    (default, newX, newY, grow(grid, ~padAmount, ~default));
  };

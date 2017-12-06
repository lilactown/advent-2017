let max = (list) => {
  let (max, index, _) =
    List.fold_left(
      ((curMax, lastMaxIndex, index), n: int) =>
        curMax >= n ? (curMax, lastMaxIndex, index + 1) : (n, index, index + 1),
      (List.hd(list), 0, 0),
      list
    );
  (max, index)
};

let rec print_list = (list) =>
  switch list {
  | [] => print_newline()
  | [hd, ...tl] =>
    print_int(hd);
    print_char(' ');
    print_list(tl)
  };

module Part1 = {
  type input = string;
  type answer = int;
  let cases = [("0 2 7 0", 5)];
  let reallocate = (banks, index, blocksToAllocate) => {
    let (allocation, _, _) =
      List.fold_left(
        ((banks, blocksLeft, curIndex), blocks) => {
          Js.log((
            blocks,
            Js.Boolean.to_js_boolean(curIndex == index),
            Js.Boolean.to_js_boolean(blocksLeft > 0),
            blocks + 1
          ));
          switch (curIndex == index, blocksLeft > 0) {
          | (true, true) => ([1, ...banks], blocksLeft - 1, curIndex + 1)
          | (true, false) => ([0, ...banks], blocksLeft, curIndex + 1)
          | (false, false) => ([blocks, ...banks], blocksLeft, curIndex + 1)
          | (false, true) => ([blocks + 1, ...banks], blocksLeft - 1, curIndex + 1)
          }
        },
        ([], blocksToAllocate, 0),
        banks
      );
    allocation
  };
  let rec cycle = (banks, allocations) => {
    let (blocks, bankIndex) = max(banks);
    print_list(banks);
    let newAllocation = reallocate(banks, bankIndex, blocks);
    let isRepeat =
      List.exists(
        (oldAllocation) => List.for_all2((a, b) => a == b, newAllocation, oldAllocation),
        allocations
      );
    if (isRepeat) {
      List.length(allocations)
    } else {
      cycle(newAllocation, [newAllocation, ...allocations])
    }
  };
  let solve = (input) => {
    let banks = Js.String.split(" ", input) |> Array.map(int_of_string) |> Array.to_list;
    cycle(banks, [])
  };
};

module Part2 = {
  type input = string;
  type answer = int;
  let cases = [];
  let solve = (_string) => 0;
};
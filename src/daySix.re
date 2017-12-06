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
  let rec reallocate = (banks, blocksToAllocate, start) => {
    let newAllocation = Array.copy(banks);
    let blocksLeft = ref(blocksToAllocate);
    for (index in start to Array.length(newAllocation) - 1) {
      if (blocksLeft^ > 0) {
        newAllocation[index] = newAllocation[index] + 1;
        blocksLeft := blocksLeft^ - 1
      }
    };
    if (blocksLeft^ > 0) {
      reallocate(newAllocation, blocksLeft^, 0)
    } else {
      newAllocation
    }
  };
  let zeroIndex = (index, array) => {
    let newArr = Array.copy(array);
    newArr[index] = 0;
    newArr
  };
  let rec cycle = (banks, allocations) => {
    let (blocks, maxIndex) = max(banks);
    let newAllocation =
      Array.to_list(reallocate(zeroIndex(maxIndex, Array.of_list(banks)), blocks, maxIndex + 1));
    let isRepeat =
      List.exists(
        (oldAllocation) => List.for_all2((a, b) => a == b, newAllocation, oldAllocation),
        allocations
      );
    if (isRepeat) {
      List.length(allocations) + 1
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

let part1 = Part1.solve;
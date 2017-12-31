type t = Js.Dict.t(array(string));

type element = string;

module StringSet = Set.Make(String);

let concatArray =
  Array.fold_left((newArr, arr) => Array.append(newArr, arr), [||]);

let unwrap = Js.Option.getExn;

let fromArray: array((element, array(element))) => t = Js.Dict.fromArray;

let fromList: list((element, array(element))) => t = Js.Dict.fromList;

let connections = (v: element, graph: t) => Js.Dict.get(graph, v);

let empty = () : t => Js.Dict.empty();

let add = (v: element, connections: array(string), graph: t) : unit =>
  switch (Js.Dict.get(graph, v)) {
  /* v already exists in graph */
  | Some(c) =>
    Js.Dict.set(
      graph,
      v,
      /* Make connections unique */
      StringSet.of_list(Array.concat([connections, c]) |> Array.to_list)
      |> StringSet.elements
      |> Array.of_list
    )
  | None => Js.Dict.set(graph, v, connections)
  };

let elements = (graph: t) => Js.Dict.keys(graph);

let size = (graph: t) => Array.length(Js.Dict.keys(graph));

let findAllConnected = (v, graph) => {
  let rec finder = (vs, seen) => {
    let connected =
      Array.map(v => unwrap(connections(v, graph)), vs) |> concatArray;
    let newSeen =
      Array.fold_left((s, w) => StringSet.add(w, s), seen, connected);
    let comp = StringSet.diff(newSeen, seen);
    if (StringSet.cardinal(comp) == 0) {
      seen
      |> StringSet.elements
      |> List.map(v => (v, unwrap(connections(v, graph))))
      |> fromList;
    } else {
      finder(StringSet.elements(comp) |> Array.of_list, newSeen);
    };
  };
  finder([|v|], StringSet.empty);
};

type mark =
  | Unmarked
  | Temporary
  | Permanent;

let sort = (graph: t) => {
  open Array;
  /* depth first search */
  let sorted = ref([]);
  let marked = Js.Dict.entries(graph) |> map(((k, _)) => (k, Unmarked));
  let select = () =>
    Js.Array.findIndex(((_, mark)) => mark == Unmarked ? true : false, marked);
  let selectV = v => Js.Array.findIndex(((k, _)) => k == v, marked);
  let n = ref(0);
  let rec visit = n =>
    switch marked[n] {
    | (_, Permanent) => ()
    | (_, Temporary) => raise(Failure("Graph is not a DAG"))
    | (v, Unmarked) =>
      let edges = Js.Option.getExn(Js.Dict.get(graph, v));
      iter(v' => visit(selectV(v')), edges);
      marked[n] = (v, Permanent);
      sorted := [v, ...sorted^];
      ();
    };
  while (n^ != (-1)) {
    visit(n^);
    n := select();
  };
  List.rev(sorted^);
};

let paths = (~seen=?, graph: t, v) => {
  let seen = Js.Option.getWithDefault((a: string, b: string) => a == b, seen);
  let sumLengths = Array.fold_left((t, a) => t + Array.length(a), 0);
  let appendUnique = (arr1, arr2) =>
    Array.fold_left(
      (newArr, el) =>
        if (! ArrayUtils.exists(el' => el' == el, newArr)) {
          Array.append([|el|], newArr);
        } else {
          newArr;
        },
      arr2,
      arr1
    );
  let rec traveler = paths => {
    /* Js.log2("paths", paths); */
    let connected =
      Array.map(path => unwrap(connections(path[0], graph)), paths)
      |> Array.mapi((i, n) =>
           ArrayUtils.filter(
             ~f=(v: string) => ! ArrayUtils.exists(seen(v), paths[i]),
             n
           )
         );
    if (sumLengths(connected) == 0) {
      paths;
    } else {
      let paths' =
        fst(
          Array.fold_left(
            ((paths', i), connections) => {
              let curPath = paths[i];
              let newPaths =
                Array.map(c => Array.append([|c|], curPath), connections);
              (appendUnique(newPaths, paths'), i + 1);
            },
            ([||], 0),
            connected
          )
        );
      traveler(paths');
    };
  };
  traveler([|[|v|]|]);
};

let compare = (g1, g2) => {
  let size1 = size(g1);
  let size2 = size(g2);
  if (size1 - size2 > 0) {
    1;
  } else if (size1 - size2 < 0) {
    (-1);
  } else {
    let el1 = StringSet.of_list(elements(g1) |> Array.to_list);
    let el2 = StringSet.of_list(elements(g2) |> Array.to_list);
    StringSet.compare(el1, el2);
  };
};

let diffElements = (g1, g2) => {
  let e1 = elements(g1) |> Array.to_list |> StringSet.of_list;
  let e2 = elements(g2) |> Array.to_list |> StringSet.of_list;
  StringSet.diff(e1, e2) |> StringSet.elements |> Array.of_list;
};

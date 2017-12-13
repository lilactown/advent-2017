type t = Js.Dict.t(array(string));

type element = string;

module StringSet = Set.Make(String);

let concatArray = Array.fold_left((newArr, arr) => Array.append(newArr, arr), [||]);

let unwrap = Js.Option.getExn;

let fromArray: array((element, array(element))) => t = Js.Dict.fromArray;

let fromList: list((element, array(element))) => t = Js.Dict.fromList;

let connections = (v: element, graph: t) => Js.Dict.get(graph, v);

let elements = (graph: t) => Js.Dict.keys(graph);

let size = (graph: t) => Array.length(Js.Dict.keys(graph));

let findAllConnected = (v, graph) => {
  let rec finder = (vs, seen) => {
    let connected = Array.map((v) => unwrap(connections(v, graph)), vs) |> concatArray;
    let newSeen = Array.fold_left((s, w) => StringSet.add(w, s), seen, connected);
    let comp = StringSet.diff(newSeen, seen);
    if (StringSet.cardinal(comp) == 0) {
      seen |> StringSet.elements |> List.map((v) => (v, unwrap(connections(v, graph)))) |> fromList
    } else {
      finder(StringSet.elements(comp) |> Array.of_list, newSeen)
    }
  };
  finder([|v|], StringSet.empty)
};

let compare = (g1, g2) => {
  let size1 = size(g1);
  let size2 = size(g2);
  if (size1 - size2 > 0) {
    1
  } else if (size1 - size2 < 0) {
    (-1)
  } else {
    let el1 = StringSet.of_list(elements(g1) |> Array.to_list);
    let el2 = StringSet.of_list(elements(g2) |> Array.to_list);
    StringSet.compare(el1, el2)
  }
};

let diffElements = (g1, g2) => {
  let e1 = elements(g1) |> Array.to_list |> StringSet.of_list;
  let e2 = elements(g2) |> Array.to_list |> StringSet.of_list;
  StringSet.diff(e1, e2) |> StringSet.elements |> Array.of_list
};
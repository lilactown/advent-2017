let abs = Js.Math.abs_float;

module Particle = {
  type t = {
    name: int,
    p: (float, float, float),
    v: (float, float, float),
    a: (float, float, float)
  };
  let origin = {name: 0, p: (0., 0., 0.), v: (0., 0., 0.), a: (0., 0., 0.)};
  let tick = (time, particle) => {
    let (x, y, z) = particle.p;
    let (x', y', z') = particle.v;
    let (x'', y'', z'') = particle.a;
    let v = (x' +. time *. x'', y' +. time *. y'', z' +. time *. z'');
    let (x', y', z') = v;
    let p = (x +. time *. x', y +. time *. y', z +. time *. z');
    {...particle, v, p};
  };
  let distance = (a, b) => {
    let (x1, y1, z1) = a.p;
    let (x2, y2, z2) = b.p;
    abs(x1 -. x2) +. abs(y1 -. y2) +. abs(z1 -. z2);
  };
  let compare = (a, b) => {
    /**
     * We want to do a comparison of their values over time
     * (find out who is slower).
     * I did a crapton of algebra to prove it to myself,
     * but the TL;DR is: compare their accelerations
     */
    let (x''_a, y''_a, z''_a) = a.a;
    let (x''_b, y''_b, z''_b) = b.a;
    let accel_a = abs(x''_a) +. abs(y''_a) +. abs(z''_a);
    let accel_b = abs(x''_b) +. abs(y''_b) +. abs(z''_b);
    let diff = accel_a -. accel_b;
    if (diff > 0.) {
      1;
    } else if (diff < 0.) {
      (-1);
    } else {
      0;
    };
  };
};

module ParticleBuffer = {
  type t = array(Particle.t);
  let pattern = [%bs.re {|/.=<(-?\d+),(-?\d+),(-?\d+)>/|}];
  let make = buffer : t =>
    buffer
    |> StringUtils.splitWith("\n")
    |> Array.map(StringUtils.splitWith(", "))
    |> Array.map(
         Array.map(s
           /* Js.log(s); */
           => Js.Re.exec(s, pattern))
       )
    |> Array.map(
         Array.map(o =>
           switch o {
           | Some(result) =>
             /* Js.log(result); */
             Js.Re.captures(result)
           | None => raise(Failure("Could not parse buffer"))
           }
         )
       )
    |> Array.map(Array.map(Array.map(Js.Nullable.to_opt)))
    |> Array.map(
         Array.map(captured =>
           switch captured {
           | [|_, Some(x), Some(y), Some(z)|] => (
               float_of_string(x),
               float_of_string(y),
               float_of_string(z)
             )
           | _ => raise(Failure("Could not match on parse result"))
           }
         )
       )
    |> Array.mapi((i, line) =>
         (
           switch line {
           | [|(x, y, z), (x', y', z'), (x'', y'', z'')|] => {
               name: i,
               p: (x, y, z),
               v: (x', y', z'),
               a: (x'', y'', z'')
             }
           | _ => raise(Failure("Could not parse all particle definition"))
           }: Particle.t
         )
       );
  let ofList = (list: list(Particle.t)) : t => Array.of_list(list);
  let tick: (float, t) => t = time => Array.map(Particle.tick(time));
};

module ParticleSet = Set.Make(Particle);

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    ({|p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>|}, 0)
  ];
  let solve = input => {
    let buffer = ParticleBuffer.make(input);
    let minP =
      ParticleSet.of_list(buffer |> Array.to_list) |> ParticleSet.min_elt;
    minP.name;
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [
    (
      {|p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>|},
      1
    )
  ];
  module CollisionBuffer = {
    include ParticleBuffer;
    let remove = (buffer: t, p: Particle.t) =>
      Js.Array.filter(p' => p' != p, buffer);
    let didCollide = (p0: Particle.t, p1: Particle.t) =>
      if (p1 != p0) {
        let (x0, y0, z0) = p0.p;
        let (x1, y1, z1) = p1.p;
        x1 == x0 && y1 == y0 && z1 == z0;
      } else {
        false;
      };
    let removeCollisions = buffer => {
      let collisions = [||];
      Array.iter(
        p0 =>
          Js.Array.some(didCollide(p0), buffer) ?
            Js.Array.push(p0, collisions) |> ignore : (),
        buffer
      );
      Array.fold_left((buffer, p) => remove(buffer, p), buffer, collisions);
    };
  };
  let solve = input => {
    let buffer = ref(CollisionBuffer.make(input));
    let t = ref(1.);
    /* I took the lazy way */
    let n = 1_000.;
    while (t^ < n) {
      if (Array.length(buffer^) > 1) {
        buffer := CollisionBuffer.removeCollisions(buffer^);
        buffer := CollisionBuffer.tick(1., buffer^);
        t := t^ +. 1.;
      } else {
        t := n +. 1.;
      };
    };
    Array.length(buffer^);
  };
};

let part1 = Part1.solve;

let part2 = Part2.solve;

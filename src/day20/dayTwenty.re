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
    /**
     * x'(t+1) = x'(t) + x''
     *         = x'(t-1) + 2x''
     *         ...
     *         = x'(0) + tx'';
     *
     * x(t+1) = x(t) + x'(t)
     *        ...
     *        = x(t) + (x'(0) + tx'')
     *        = (x(t-1) + x'(0) + x'' * t) + x'(0) + tx''
     *        = x(t-1) + 2(x'(0) + tx'')
     *        ...
     *        = x(0) + t(x'(0) + tx'')
     *        or
     *        = x(0) + tx'(0) + (t**2)x''
     */
    let v = (x' +. time *. x'', y' +. time *. y'', z' +. time *. z'');
    let (x', y', z') = v;
    let p = (x +. time *. x', y +. time *. y', z +. time *. z');
    {...particle, v, p}
  };
  /* let distance = (a, b) => {
       let (x1, y1, z1) = a.p;
       let (x2, y2, z2) = b.p;
       abs(x1 -. x2) +. abs(y1 -. y2) +. abs(z1 -. z2)
     }; */
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
      1
    } else if (diff < 0.) {
      (-1)
    } else {
      0
    }
  };
};

module ParticleBuffer = {
  type t = array(Particle.t);
  let pattern = [%bs.re {|/.=<(-?\d+),(-?\d+),(-?\d+)>/|}];
  let make = (buffer) : t =>
    buffer
    |> StringUtils.splitWith("\n")
    |> Array.map(StringUtils.splitWith(", "))
    |> Array.map(
         Array.map(
           (s) =>
             /* Js.log(s); */
             Js.Re.exec(s, pattern)
         )
       )
    |> Array.map(
         Array.map(
           (o) =>
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
         Array.map(
           (captured) =>
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
    |> Array.mapi(
         fun (i, line) => (
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
};

module Part1: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [({|p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>|}, 0)];
  module ParticleSet = Set.Make(Particle);
  let solve = (input) => {
    let buffer = ParticleBuffer.make(input);
    let minP = ParticleSet.of_list(buffer |> Array.to_list) |> ParticleSet.min_elt;
    minP.name
  };
};

module CollidingParticle = {
  type t = Particle.t;
  let compare = (a, b) => {
    /**
     * We want to know if pos_a(t) == pos_b(t)
     * for some t (time).
     * => (x_a(t), y_a(t), z_a(t)) == (x_b(t), y_b(t), z_b(t))
     * Let's focus just on x:
     * => x_a(t) == x_b(t)
     * => x_a(0) + tx'_a(0) + (t**2)x''_a == x_b(0) + tx'_b(0) + (t**2)x''_b
     * => x_a(0) - x_b(0) + tx'_a(0) - tx'_b(0) + (t**2)x''_a - (t**2)x''_b = 0
     * =>
     */
  };
};

module Part2: Solution.Solver = {
  type input = string;
  type answer = int;
  let cases = [("", 5)];
  let solve = (input) => 6;
};

let part1 = Part1.solve;

let part2 = Part2.solve;
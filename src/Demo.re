[@bs.val] external setTimeout: ('a => unit, int, 'a) => unit = "setTimeout";
[@bs.val] [@bs.scope ("process", "stdout")] external columns: int = "columns";
[@bs.val] [@bs.scope ("process", "stdout")] external rows: int = "rows";

let (<.) = (f, g, a) => f(g(a));

let acorn = [(0, 1), (1, 3), (2, 0), (2, 1), (2, 4), (2, 5), (2, 6)];
let blinker = [(1, 0), (1, 1), (1, 2)];
let engine = [
  (0, 1),
  (0, 3),
  (1, 0),
  (2, 1),
  (2, 4),
  (3, 3),
  (3, 4),
  (3, 5),
  (4, 26),
  (4, 27),
  (5, 26),
  (5, 27),
];
let glider = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)];
let growth = [
  (0, 6),
  (1, 4),
  (1, 6),
  (1, 7),
  (2, 4),
  (2, 6),
  (3, 4),
  (4, 2),
  (5, 0),
  (5, 2),
];
let rabbits = [
  (0, 0),
  (0, 4),
  (0, 5),
  (0, 6),
  (1, 0),
  (1, 1),
  (1, 2),
  (1, 5),
  (2, 1),
];
let r_pentomino = [(1, 2), (1, 3), (2, 1), (2, 2), (3, 2)];
let diehard = [(1, 7), (2, 1), (2, 2), (3, 2), (3, 6), (3, 7), (3, 8)];

let g = Life.Graph.make(rows - 2, columns, rabbits);

let rec main = (i, current) => {
  Js.log(string_of_int(i + 1));
  Life.Graph.show(current);
  let next = Life.Graph.next(current);

  current != next ? setTimeout(_ => main(i + 1, next), 50, g) : ();
};

main(0, g);

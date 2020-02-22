let (<.) = (f, g, a) => f(g(a));
let (>.) = (f, g, a) => g(f(a));

module rec Cell: {
  type t =
    | Live
    | Dead;
  let empty: t;
  let is_alive: t => bool;
  let show: t => string;

  let get_neighbors: (Graph.t, Graph.coords) => list(t);
  let count_neighbors: (Graph.t, Graph.coords) => int;

  let next: (Graph.t, Graph.coords, t) => t;
} = {
  type t =
    | Live
    | Dead;

  let empty = Dead;

  let is_alive =
    fun
    | Live => true
    | Dead => false;

  let show = c => is_alive(c) ? "0" : ".";

  let get_neighbors = (g, (x, y)) =>
    [
      (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y - 1),
      (x + 1, y),
      (x + 1, y + 1),
    ]
    |> List.map(Graph.get(g));

  let count_neighbors = g =>
    List.(get_neighbors(g) >. filter(is_alive) >. length);

  let next = (g, coords, cell) => {
    let neighbours = count_neighbors(g, coords);

    switch (cell, neighbours) {
    /* Any live cell with two or three live neighbours lives on to the next generation. */
    | (Live, 2)
    | (Live, 3) => Live
    /* Any live cell with fewer than two live neighbours dies, as if caused by underpopulation. */
    /* Any live cell with more than three live neighbours dies, as if by overpopulation. */
    | (Live, _) => Dead
    /* Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction. */
    | (Dead, 3) => Live
    | _ => Dead
    };
  };
}

and Graph: {
  type t = array(array(Cell.t));
  type coords = (int, int);

  let make: (int, int, list(coords)) => t;
  let show: t => unit;
  let get: (t, coords) => Cell.t;

  let next: t => t;
} = {
  type t = array(array(Cell.t));
  type coords = (int, int);

  let make = (height, width, coords) => {
    let offX = height / 2 - 1;
    let offY = width / 2 - 1;

    Array.make_matrix(height, width, Cell.empty)
    |> Array.mapi(x =>
         Array.mapi((y, cell) =>
           List.mem((x - offX, y - offY), coords) ? Cell.Live : cell
         )
       );
  };

  let get = (g, (x, y)) =>
    switch (g[x][y]) {
    | exception _ => Cell.empty
    | cell => cell
    };

  let show =
    Array.iter(Js.log <. Array.fold_left(a => (++)(a) <. Cell.show, ""));

  let next = g => Array.(mapi(x => mapi(y => Cell.next(g, (x, y))), g));
};

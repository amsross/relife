type t = list(Life.Coords.t);

let (>.) = (f, g, a) => g(f(a));
let (<.) = (f, g, a) => f(g(a));
let isSome = Belt.Option.isSome;

let patternToList: string => list(list(string)) =
  Js.String.(
    Array.(split("\n") >. map(trim >. split("") >. to_list) >. to_list)
  );

let fromPattern: string => t =
  List.(
    patternToList
    >. mapi((x, str) =>
         mapi((y, c) => c == "O" ? Some((x, y)) : None, str)
       )
    >. fold_left(
         a =>
           append(a)
           <. fold_left(
                a =>
                  fun
                  | Some(v) => [v, ...a]
                  | _ => a,
                [],
              ),
         [],
       )
  );

let random = () => {
  Random.self_init();

  let coords = ref([]);

  for (_ in 0 to 50) {
    if (Random.int(2) == 1) {
      coords := [(Random.int(10), Random.int(10)), ...coords^];
    };
  };

  coords^;
};

let get =
  fun
  | "metamorphosis" =>
    fromPattern(
      "...................O.........
       ....................O........
       ..................OOO........
       .............................
       .............................
       .............................
       .............................
       .............................
       ............O...O.....O.OO...
       OO.........O.....O....O.O.O..
       OO.........O.........O....O..
       ...........OO...O.....O.O.O..
       .............OOO......O.OO...
       .............................
       .............OOO.............
       ...........OO...O............
       OO.........O...............OO
       OO.........O.....O.........OO
       ............O...O............",
    )
  | "psuedo_barberpole" =>
    fromPattern(
      "..........OO
       ...........O
       .........O..
       .......O.O..
       ............
       .....O.O....
       ............
       ...O.O......
       ............
       ..OO........
       O...........
       OO..........",
    )
  | "protein" =>
    fromPattern(
      "....OO.......
       ....O........
       ......O......
       ..OOOO.O.OO..
       .O.....O.O..O
       .O..OO.O.O.OO
       OO.O.....O...
       ...O..OO.O...
       ...O....O....
       ....OOOO.....
       .............
       ....OO.......
       ....OO.......",
    )
  | "pressure_cooker" =>
    fromPattern(
      ".....O.....
       ....O.O....
       ....O.O....
       ...OO.OO...
       O.O.....O.O
       OO.O.O.O.OO
       ...O...O...
       ...O...O...
       ....OOO....
       ...........
       ...O.OO....
       ...OO.O....",
    )
  | "mini_pressure_cooker" =>
    fromPattern(
      ".....O.....
       ....O.O....
       ....O.O....
       ...OO.OO...
       O.O.....O.O
       OO.O.O.O.OO
       ...O...O...
       ...O.O.O...
       ....O.O....
       .....O.....",
    )
  | "puffer_train" =>
    fromPattern(
      ".OOO...........OOO
       O..O..........O..O
       ...O....OOO......O
       ...O....O..O.....O
       ..O....O........O.",
    )
  | "acorn" => [(0, 1), (1, 3), (2, 0), (2, 1), (2, 4), (2, 5), (2, 6)]
  | "blinker" => [(1, 0), (1, 1), (1, 2)]
  | "engine" => [
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
    ]
  | "glider" => [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
  | "growth" => [
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
    ]
  | "rabbits" => [
      (0, 0),
      (0, 4),
      (0, 5),
      (0, 6),
      (1, 0),
      (1, 1),
      (1, 2),
      (1, 5),
      (2, 1),
    ]
  | "r_pentomino" => [(1, 2), (1, 3), (2, 1), (2, 2), (3, 2)]
  | "diehard" => [
      (1, 7),
      (2, 1),
      (2, 2),
      (3, 2),
      (3, 6),
      (3, 7),
      (3, 8),
    ]
  | _ => random();

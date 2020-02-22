[@bs.val] external setTimeout: ('a => unit, int, 'a) => unit = "setTimeout";
[@bs.val] [@bs.scope ("process", "stdout")] external columns: int = "columns";
[@bs.val] [@bs.scope ("process", "stdout")] external rows: int = "rows";

let (<.) = (f, g, a) => f(g(a));

let pattern =
  try(Sys.argv[2]) {
  | Invalid_argument(_) => Js.Exn.raiseError("No Pattern Provided")
  };

let g = Life.Graph.make(rows - 2, columns, Patterns.get(pattern));

let rec main = (i, current) => {
  let next = Life.Graph.next(current);

  Js.log @@ Life.Graph.show(current);
  Js.log @@
  String.uppercase_ascii(pattern)
  ++ " GENERATION "
  ++ string_of_int(i + 1);

  current != next ? setTimeout(main(i + 1), 100, next) : ();
};

main(0, g);

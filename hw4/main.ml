type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
         | Branch of map * map
         | Guide of string * map

exception IMPOSSIBLE

let getReady (expression: map): key list =
  let rec inference (expression: map): key =
    match expression with
    | End(value) -> begin
      (* Term *)
      match value with
      | StarBox -> (* Constant *) Bar
      | NameBox(name) -> begin
        (* Variable, TODO *)
        Bar
      end
    end
    | Branch(efunc, eparam) -> begin
      let tfunc = inference efunc in
      match tfunc with
      | Bar -> raise IMPOSSIBLE
      | Node(tparam_expected, treturn) -> begin
        let tparam_actual = inference eparam in
        if tparam_expected = tparam_actual then
          treturn
        else
          raise IMPOSSIBLE
      end
    end
    | Guide(name, body) -> begin
      (* Function definition, TODO *)
      Bar
    end
  in
  [inference expression]

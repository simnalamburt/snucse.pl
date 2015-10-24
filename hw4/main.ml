type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
         | Branch of map * map
         | Guide of string * map

exception IMPOSSIBLE

module Dict = Map.Make(String)
type dict = key Dict.t

let getReady (expression: map): key list =
  let rec inference (expression: map) (env: dict): key =
    match expression with
    | End(value) -> begin
      (* Term *)
      match value with
      | StarBox -> (* Constant *) Bar
      | NameBox(name) -> begin
        (* Variable *)
        try Dict.find name env
        with Not_found -> Bar
      end
    end
    | Branch(efunc, eparam) -> begin
      let tfunc = inference efunc env in
      match tfunc with
      | Bar -> raise IMPOSSIBLE
      | Node(tparam_expected, treturn) -> begin
        let tparam_actual = inference eparam env in
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
  [inference expression Dict.empty]

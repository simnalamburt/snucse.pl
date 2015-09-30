module type Queue = sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ = struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q
  let emptyQ = ([], [])
  let enQ (((left, right), elem): queue * element): queue = (elem::left, right)
  let rec deQ ((left, right): queue): element * queue =
    match right with
    | [] -> begin
      match left with
      | [] -> raise EMPTY_Q
      | left -> deQ ([], List.rev left)
    end
    | r0::rest -> (r0, (left, rest))
end

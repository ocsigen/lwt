let may f = function
  | Some x -> f x
  | None -> Lwt.return ()

let lift = function
	| None -> Lwt.return None
	| Some x -> Lwt.map (fun x -> Some x) x

module Opt = struct
	let map f = function
  	| None -> None
  	| Some x -> Some (f x)
end

let map f x = lift @@ Opt.map f x

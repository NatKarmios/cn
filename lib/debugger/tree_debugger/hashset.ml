include Hashtbl

type 'a t = ('a, unit) Hashtbl.t

let add tbl x = Hashtbl.replace tbl x ()

let iter f tbl = Hashtbl.iter (fun x () -> f x) tbl

let fold f tbl acc = Hashtbl.fold (fun x () acc -> f x acc) tbl acc

let to_seq = Hashtbl.to_seq_keys

module Generators = struct
  let temp_var () =
    let id = ref 0 in
    fun () ->
      let c = !id in
      let ret = "temp__" ^ string_of_int c in
      incr id;
      ret

  let temp_lvar () =
    let id = ref 0 in
    fun () ->
      let c = !id in
      let ret = "#lvar_" ^ string_of_int c in
      incr id;
      ret
end

type t = {
  prog : Program.t;
  fresh_v : unit -> string;
  fresh_lv : unit -> string;
  locals : Utils.Containers.SS.t;
}

let make ~prog () =
  {
    prog;
    fresh_v = (fun () -> failwith "uninitialized var generator");
    fresh_lv = (fun () -> failwith "uninitialized lvar generator");
    locals = Utils.Containers.SS.empty;
  }

let with_new_generators t =
  {
    t with
    fresh_v = Generators.temp_var ();
    fresh_lv = Generators.temp_lvar ();
  }
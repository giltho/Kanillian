open Cgil_lib
open Kanillian
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)

module CLI =
  Gillian.CommandLine.Make (CMemory) (SMemory) (External.M) (ParserAndCompiler)
    (struct
      let runners : Gillian.Bulk.Runner.t list = []
    end)
    (Debugger.Gil_to_tl_lifter.Default (SMemory) (ParserAndCompiler))

let () = CLI.main ()

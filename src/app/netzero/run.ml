open Async
open Async_kernel
open Core_kernel
open Snark_params.Tick.Run
module Nat = Pickles_types.Nat
include Test_circuit.Add_rule

let time lab f =
  let open Core_kernel in
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "%s: %s\n%!" lab (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

let dtime label (d : 'a Deferred.t) =
  let open Core_kernel in
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x

let _, _, _, Pickles.Provers.[ init_; merge_ ] =
  time "compile" (fun () ->
      Pickles.compile () ~override_wrap_domain:Pickles_base.Proofs_verified.N1
        ~cache:Cache_dir.cache ~public_input:(Output Field.typ)
        ~auxiliary_typ:Typ.unit
        ~branches:(module Nat.N2)
        ~max_proofs_verified:(module Nat.N2)
        ~name:"add rules"
        ~choices:(fun ~self -> [ Init.rule; Merge.rule self ]) )

let init a b =
  let%map stmt, _, proof = init_ ~handler:(Init.handler a b) () in
  ({ stmt; proof } : Snark.t)

let merge (s1 : Snark.t) (s2 : Snark.t) =
  let%map stmt, _, proof = merge_ ~handler:(Merge.handler s1 s2) () in
  ({ stmt; proof } : Snark.t)

let () =
  Thread_safe.block_on_async_exn (fun () ->
      let%bind first =
        dtime "first" (init Field.Constant.(of_int 4) Field.Constant.(of_int 5))
      in

      let%bind second =
        dtime "second"
          (init Field.Constant.(of_int 1) Field.Constant.(of_int 2))
      in

      let%bind sum = dtime "sum" (merge first second) in

      print_endline @@ Field.Constant.to_string sum.stmt ;

      Deferred.unit )

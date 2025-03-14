[@@@ocaml.warning "-33"]

open Async_kernel
open Core_kernel
open Snark_params.Tick.Run
module Nat = Pickles_types.Nat
open Pickles_types

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

let () =
  let feature_flag =
    Plonk_types.Features.{ none_bool with lookup = true; runtime_tables = true }
  in
  let tag, _cache_handle, proof, Pickles.Proof.[ lkproof ] =
    time "compile" (fun () ->
        Pickles.compile ~public_input:(Pickles.Inductive_rule.Input Typ.unit)
          ~override_wrap_domain:Pickles_base.Proofs_verified.N1
          ~auxiliary_typ:Typ.unit
          ~branches:(module Nat.N1)
          ~max_proofs_verified:(module Nat.N2)
          ~name:"Lookup table runtime"
          ~choices:(fun ~self:_ ->
            [ { identifier = "main"
              ; prevs = []
              ; feature_flags = feature_flag
              ; main =
                  (fun _ ->
                    Test_lookup.main_runtime_table_cfg () ;
                    { previous_proof_statements = []
                    ; public_output = ()
                    ; auxiliary_output = ()
                    } )
              }
            ] )
          () )
  in
  let _vk =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Pickles.Side_loaded.Verification_key.of_compiled tag )
  in
  let public_input1, (), proof1 =
    Async.Thread_safe.block_on_async_exn (fun () ->
        dtime "proof generation" (lkproof ()) )
  in
  let module Proof = (val proof) in
  Or_error.ok_exn
    (Async.Thread_safe.block_on_async_exn (fun () ->
         dtime "proof verification" (Proof.verify [ (public_input1, proof1) ]) )
    )

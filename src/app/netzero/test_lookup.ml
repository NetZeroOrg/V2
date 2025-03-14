open Core_kernel
open Pickles.Impls.Step
open Pickles_optional_custom_gates_circuits

let add_constraint c = assert_ c

let add_plonk_constraint c = add_constraint c

let state = Random.State.make [| Random.int 1_000_000_000 |]

(* Testing the maximum number of lookup tables *)
let max_runtime_lt_n = 10

(* The number of runtime lookup queries *)
let runtime_lt_queries_n = 100000

let runtime_lt_data =
  let runtime_table_ids =
    let ids =
      List.init (max_runtime_lt_n - 1) ~f:(fun _ ->
          1 + Random.State.int state 100 )
    in
    (* making sure they're all unique *)
    Int.Set.to_array (Int.Set.of_list ids)
  in

  Array.map runtime_table_ids ~f:(fun table_id ->
      let max_table_size = 1 + Random.State.int state 100 in
      let first_column =
        Int.Set.to_array
          (Int.Set.of_list
             (List.init max_table_size ~f:(fun _ ->
                  1 + Random.State.int state (max_table_size * 4) ) ) )
      in
      let table_size = Array.length first_column in
      let second_column =
        Array.init table_size ~f:(fun _ -> Random.State.int state 1_000_000)
      in
      (table_id, first_column, second_column) )

let runtime_lookups =
  Array.init runtime_lt_queries_n ~f:(fun _ ->
      let table_id, first_column, second_column =
        runtime_lt_data.(Random.State.int state (Array.length runtime_lt_data))
      in
      let table_size = Array.length first_column in
      let idx1 = Random.State.int state table_size in
      let idx2 = Random.State.int state table_size in
      let idx3 = Random.State.int state table_size in
      ( table_id
      , (first_column.(idx1), second_column.(idx1))
      , (first_column.(idx2), second_column.(idx2))
      , (first_column.(idx3), second_column.(idx3)) ) )

let main_runtime_table_cfg () =
  Array.iter runtime_lt_data ~f:(fun (table_id, first_column, _) ->
      add_plonk_constraint
        (AddRuntimeTableCfg
           { id = Int32.of_int_exn table_id
           ; first_column = Array.map ~f:Field.Constant.of_int first_column
           } ) ) ;
  Array.iter runtime_lookups ~f:(fun (table_id, (k1, v1), (k2, v2), (k3, v3)) ->
      add_plonk_constraint
        (Lookup
           { w0 = fresh_int table_id
           ; w1 = fresh_int k1
           ; w2 = fresh_int v1
           ; w3 = fresh_int k2
           ; w4 = fresh_int v2
           ; w5 = fresh_int k3
           ; w6 = fresh_int v3
           } ) )

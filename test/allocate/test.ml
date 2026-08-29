open Bechamel
open Bechamel.Toolkit

let all_released kind =
  Alcotest.test_case "all released" `Quick @@ fun () ->
  let global = ref 0 in
  let called = ref 0 in
  let test =
    Test.make_with_resource ~name:"test" kind
      ~allocate:(fun () ->
        incr called;
        incr global)
      ~free:(fun () -> decr global)
      (Staged.stage (Fun.const ()))
  in
  let[@warning "-partial-match"] [ test ] = Test.elements test in
  let cfg = Benchmark.cfg ~limit:10 ~kde:None () in
  let _ = Benchmark.run cfg Instance.[ monotonic_clock ] test in
  Alcotest.(check int) "all released" !global 0;
  if !called = 0 then Alcotest.failf "Benchmark does not allocate"

let with_kde kind =
  Alcotest.test_case "with kde" `Quick @@ fun () ->
  let global = ref 0 in
  let called = ref 0 in
  let test =
    Test.make_with_resource ~name:"test" kind
      ~allocate:(fun () ->
        incr called;
        incr global)
      ~free:(fun () -> decr global)
      (Staged.stage (Fun.const ()))
  in
  let[@warning "-partial-match"] [ test ] = Test.elements test in
  let cfg = Benchmark.cfg ~limit:10 ~kde:(Some 1000) () in
  let _ = Benchmark.run cfg Instance.[ monotonic_clock ] test in
  Alcotest.(check int) "with kde" !global 0;
  if !called = 0 then Alcotest.failf "Benchmark does not allocate"

let uniq_resources kind =
  Alcotest.test_case "uniq resources" `Quick @@ fun () ->
  let tbl = Hashtbl.create 0x100 in
  let idx = ref 0 in
  let test =
    Test.make_with_resource ~name:"test" kind
      ~allocate:(fun () ->
        let value = !idx in
        incr idx;
        Hashtbl.add tbl value ();
        value)
      ~free:(Hashtbl.remove tbl)
      (Staged.stage (Fun.const ()))
  in
  let[@warning "-partial-match"] [ test ] = Test.elements test in
  let cfg = Benchmark.cfg ~limit:10 ~kde:(Some 1000) () in
  let _ = Benchmark.run cfg Instance.[ monotonic_clock ] test in
  Alcotest.(check int) "uniq resources" (Hashtbl.length tbl) 0

let double_free kind =
  Alcotest.test_case "double free" `Quick @@ fun () ->
  let tbl = Hashtbl.create 0x100 in
  let idx = ref 0 in
  let test =
    Test.make_with_resource ~name:"test" kind
      ~allocate:(fun () ->
        let value = !idx in
        incr idx;
        Hashtbl.add tbl value ();
        value)
      ~free:(fun value ->
        match Hashtbl.find_opt tbl value with
        | None -> Alcotest.failf "Double free"
        | Some () -> Hashtbl.remove tbl value)
      (Staged.stage (Fun.const ()))
  in
  let[@warning "-partial-match"] [ test ] = Test.elements test in
  let cfg = Benchmark.cfg ~limit:10 ~kde:(Some 1000) () in
  let _ = Benchmark.run cfg Instance.[ monotonic_clock ] test in
  Alcotest.(check int) "double free" (Hashtbl.length tbl) 0

let every_resource_is_used =
  Alcotest.test_case "every resource is used" `Quick @@ fun () ->
  let seen = Hashtbl.create 0x100 in
  let idx = ref 0 in
  let test =
    Test.make_with_resource ~name:"test" Test.multiple
      ~allocate:(fun () ->
        let value = !idx in
        incr idx;
        value)
      ~free:ignore
      (Staged.stage (fun value -> Hashtbl.replace seen value ()))
  in
  let[@warning "-partial-match"] [ test ] = Test.elements test in
  let cfg =
    Benchmark.cfg ~limit:10 ~kde:None ~start:8 ~sampling:(`Linear 0) ()
  in
  let _ = Benchmark.run cfg Instance.[ monotonic_clock ] test in
  (* [Benchmark.run] allocates one extra resource which it never runs. *)
  let allocated = !idx - 1 and used = Hashtbl.length seen in
  if used < allocated then
    Alcotest.failf "%d resources allocated but only %d of them were used"
      allocated used

let () =
  Alcotest.run "allocate"
    [ ( "uniq"
      , [ all_released Test.uniq
        ; with_kde Test.uniq
        ; uniq_resources Test.uniq
        ; double_free Test.uniq
        ] )
    ; ( "multiple"
      , [ all_released Test.multiple
        ; with_kde Test.multiple
        ; uniq_resources Test.multiple
        ; double_free Test.multiple
        ; every_resource_is_used
        ] )
    ]

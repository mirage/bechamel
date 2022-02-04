(** {1 Bechamel, a simple and agnostic micro-benchmarking framework.}

    Bechamel is a simple and {i agnostic} micro-benchmarking framework to help
    the developer to prove some metrics and compare them for a {b small} given
    function. It's measuring the performance of something "small", like a
    system call. Bechamel does not do, as we say, a macro-benchmark which can
    show performance regression or I/O congestion for instance. It permits just
    to assert that a simple call of a small function [fn1] can be faster
    (if you use a {i time} metric) than a call of another small function
    [fn2].

    In this way, it asserts that [fn1] should be more efficient than [fn2] and
    it lets the developer {b deduce} the best choice according to the
    runtime context.

    Bechamel should {b not} lead to premature optimization. It gives only
    clues/metrics about what you use, but you {b must} recontextualize results
    according to your case to lead to {i certain} optimization.

    {2 How to use Bechamel?}

    Bechamel is split into 3 parts:
    - A user interface to define {i tests} (your small function)
    - A {i runner} which will record required metrics
    - An {i analyzer} which will analyze {i raw} metrics and give you a
      stated result

    This is the core of Bechamel where the user is able to:
    - define its own tests
    - use its own metrics
    - have a choice between 2 analyses (for instance, Ordinary Least Square
      analysis or RANdom SAmple Consensus analysis)

    {3 Make a test.}

    The {!module:Test} gives an API which permits defining your tests. Let's
    take the example of the recursive factorial and the "imperative" factorial:
    {[
      let rec fact0 n =
        if n = 0 then 1
        else n * fact0 (n - 1)

      let fact1 n =
        let m = ref 0 in
        let v = ref 1 in
        while !m < n do
          m := !m + 1 ;
          v := !v * !m ;
        done ; !v
    ]}

    From these small functions, we are able to make a test for each function
    and group them into one test:
    {[
      let test0 = Test.make ~name:"recursive"
        (Staged.stage @@ fun () -> fact0 120)
      let test1 = Test.make ~name:"imperative"
        (Staged.stage @@ fun () -> fact1 120)
      let test  = Test.make_grouped ~name:"factorial" ~fmt:"%s %s"
        [ test0; test1; ]
    ]}

    The user is able to make multiple kinds of tests:
    - A simple one as we did above
    - An indexed one which can take an [int] as an argument. For instance,
      we can execute our [fact] function with a set of [int]s.
    - A test which requires a "resource" which must be allocated before the
      {i benchmark} and released after. For instance, we can allocate a
      {i socket}, run {!val:Unix.write} and record metrics and release
      ({!val:Unix.close}) the resource then.
    - Finally, we can define an {i indexed} with a required resource test

    {3 Run the benchmark.}

    Then, you need to run the benchmark and record metrics. Bechamel is
    {i agnostic} to the system: it permits recording a few metrics like the
    {!val:Toolkit.Instance.monotonic_clock} or how many words were allocated
    into the minor heap {!val:Toolkit.Instance.minor_allocated}.

    Depending on the execution context, the user is able to add some new
    metrics. For instance, on Linux, you can record the
    {!val:Bechamel_perf.Instance.cpu_clock} - but it's not a part of the core
    distribution. More abstractly, Bechamel is able to record any metrics as
    far as the user is able to provide a {!modtype:Bechamel.S.MEASURE}.

    For instance, we will try to record the monotonic clock: it represents the
    absolute elapsed {i wall-clock} time since an arbitrary, fixed point in
    the past (usually, the time since the program began running).

    {[
      let benchmark () =
        let instances = Instance.[ monotonic_clock ] in
        let cfg = Benchmark.cfg ~limit:2000 ~stabilize:true
          ~quota:(Time.second 0.5) () in
        Benchmark.all cfg instances tests
    ]}

    The benchmark has many options and you should take a look on
    {!val:Benchmark.cfg}. They permit to refine the context of the execution.
    For instance, you can {i stabilize} the garbage-collector.

    The function gives you {i raw} results (see {!module:Measurement_raw}). You
    can manipulate it as is or analyze it to extract useful information.

    {3 Analyze results.}

    Finally, you probably want to know the time spent by our factorial
    functions! This result requires an analyze from metrics. Indeed, if you run
    one time [fact0] and record the monotonic clock, you will
    probably get a {i partial} result which fluctuated a lot per run:
    {[
      $ cat >main.ml <<EOF
      > let rec fact0 x =
      >   if x = 0 then 1
      >   else x * fact0 (x - 1)
      > 
      > let () =
      >   let t0 = Unix.gettimeofday () in
      >   let _  = fact0 200 in
      >   let t1 = Unix.gettimeofday () in
      >   Format.printf "%f\n%!" (t1 -. t0)
      > EOF
      $ ocamlfind opt -package unix -linkpkg main.ml
      $ ./a.out
      0.000001
      $ ./a.out
      0.000003
    ]}

    This is why Bechamel exists. From metrics, it can estimate the time spent
    by our test. It exists 2 methods to do that:
    - calculate the Ordinary Least Square from metrics
    - calculate the RANdom Sample Consensus from metrics

    In our cases, we will use {!val:Analyze.ols}:
    {[
      let analyze results =
        let ols = Analyze.ols ~bootstrap:0 ~r_square:true
          ~predictors:[| Measure.run |] in
        let results = Analyze.all ols Instance.monotonic_clock results in
        Analyze.merge ols [ Instance.monotonic_clock ] [ results ]
    ]}

    The main question behind this function is: I would like to compare what
    with what? By default, the benchmark iterates a {i certain} time on your
    function. For each iteration, it will execute {i run} time(s) your function
    and this number increases for each iteration:

    {v
  +-----+------+------------+
  | run | time |call of [fn]|
  +-----+------+------------+
  | 1   | 19   | 1          |
  | 2   | 25   | 2          |
  | 3   | 37   | 3          |
  | 4   | 47   | 4          |
  | 5   | 56   | 5          |
  +-----+------+------------+
    v}

    From these metrics, we can guess a curve: [a * x + b = y] where, from our
    code, [x = Measure.run] and [y = Instance.monotonic_clock]. OLS and
    RANSAC are algorithms which try to guess this curve. Then, [a] will becomes
    the time spent by our function for [x = 1] and this is what we want:

    > How much time do I spend if I call my function {b one time}?

    Some details differ between OLS and RANSAC but the documentation can help
    you to determine which one you should take.

    {3 Show results.}

    Bechamel has many ways to show results, but the core still is agnostic
    to the system and does not need anything (like {!module:Unix}) to show
    results. However, the distribution comes with many possibilities:
    - A [notty] which show you results into your terminal
    - An HTML + JavaScript which produces an [index.html]

    We will try to show the results {i via} our terminal, but the HTML +
    JavaScript support has the ability to show you more information (such as
    the curve for instance):
    {[
      let () = Bechamel_notty.Unit.add
        Instance.monotonic_clock
        (Measure.unit Instance.monotonic_clock)

      let img (window, results) =
        Bechamel_notty.Multiple.image_of_ols_results ~rect:window
          ~predictor:Measure.run results

      open Notty_unix

      let () =
        let window =
          match winsize Unix.stdout with
          | Some (w, h) -> { Bechamel_notty.w; h }
          | None -> { Bechamel_notty.w= 80; h= 1; } in
        let results = benchmark () in
        let results = analyze results in
        img (window, results) |> eol |> output_image
    ]}

    You can compile (with [dune]) the program with:
    {[
      $ cat >dune <<EOF
      > (executable
      >  (name example)
      >  (modules example)
      >  (libraries bechamel notty.unix bechamel-notty))
      > EOF
      $ dune build ./example.exe
      $ dune exec ./example.exe
      ╭────────────────────────┬───────────────────────────╮
      │name                    │  monotonic-clock          │
      ├────────────────────────┼───────────────────────────┤
      │  factorial functional  │            643.0477 ns/run│
      │  factorial imperative  │            129.1994 ns/run│
      ╰────────────────────────┴───────────────────────────╯
    ]}
*)

module S = S
module Measure = Measure
module Benchmark = Benchmark
module Test = Test
module Staged = Staged
module Measurement_raw = Measurement_raw
module Linear_algebra = Linear_algebra
module Analyze = Analyze
module Toolkit = Toolkit
module Time = Time

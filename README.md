Bechamel - Agnostic benchmark tool in OCaml
-------------------------------------------

![output](https://raw.githubusercontent.com/dinosaure/bechamel/master/img/output0.png)

Bechamel is a toolkit to do a _micro_-benchmarking on your functions. It able to
be extended with your measures. Indeed, Bechamel provides only GC measures.
Then, from your target, you can choose to use `perf` (with `bechamel.perf`) or
something else available on your target.

The main purpose is to able to use Bechamel on GNU/Linux, Mac OSX, Windows and
MirageOS. Monotonic clock is available in all of these platforms.

Then, we provide 2 algorithms to analyze datas:
* OLS (Ordinary Least Square)
* RANSAC (Random Sample Consensus)

These algorithms come from `core_bench` (which uses a `REALTIME` clock), and
`operf-micro` (which is available only on GNU/Linux).

The main goal of `bechamel` (instead to be a yet another benchmark tool) is to
let the end-user to manipulate measures. We provide a _not-easy-to-use_ API
which give an access to collected informations (main difference with
`benchmark`). From it, we can produce a JSON output - and allow an other analyze
with an other tool From it, we can produce a JSON output - and allow an other
analyze with an other tool (main difference with `core_bench`).

And finally, because some counters can be available in some specific platforms
(like `perf`), we allow the user to define their own counters and use them in
`bechamel`. `bechamel.perf` is one of them (it needs a fork a `ocaml-perf`
however - a _dunification_ of this package).

### Measure

To define your measure, you need to make a new module which respect interface
`Measure.UNSAFE`. Main function is to blit/set your value (which can be
anything) from result of your counter.

For example, `perf` provides some counters like `Cpu_clock`, then, we need to
set an `int64 ref` to what the counter returns. We save it, we execute your
function some times and ask again to get value of your counter. Finally, we do
a `diff` between your first value and your second one and cast it to a float (to
be able to analyze it).

At the end, we generate plenty of values which can be analyze by an OLS
algorithm or RANSAC.

### Printer

`bechamel` has a little `notty` backend to be able to print results in a fancy
way. An example is available in the binary - image shows you the output.

Of course, `notty` is not a part of the core `bechamel`, so user can print
results like he wants - of course, it's a little annoying work already done in
`bechamel.notty` - but feel free to put an other backend.

### JSON

Finally, at the core, `bechamel` has converter to JSON and can generate JSON
output from collected values. From them, you can process a smarter analyze than
OLS or RANSAC for example - this is the main purpose of `bechamel`, be able to
manipulate results and check some assumptions.

### Purpose

`eqaf`, for example, needs to check if the equality function computes arguments
in a constant time - so it's not a benchmark strictly speaking but the main loop
to collect results and analyze algorithms are needed to check this kind of
assumption.

In other way, because `eqaf` want to be available in many platform, a
monotonic-clock in Mac OSX, Windows and GNU/Linux is needed too.

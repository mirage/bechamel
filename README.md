Bechamel - Agnostic benchmark tool in OCaml
-------------------------------------------

Bechamel is a toolkit to do a _micro_-benchmarking on your functions. It able to
be extended with your measures. Indeed, Bechamel provides only GC measures.
Then, from your target, you can choose to use `perf` (see binary example) or
something else available on your target.

The main purpose is to able to use Bechamel on GNU/Linux, Mac OSX, Windows and
MirageOS.

Then, we provide 2 algorithms to analyze datas:
* OLS
* RANSAC

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

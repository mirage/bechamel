Bechamel - Agnostic benchmark tool in OCaml
-------------------------------------------

![output](https://raw.githubusercontent.com/dinosaure/bechamel/master/img/output0.png)

Bechamel is a toolkit to do a _micro_-benchmarking on your functions. It able to
be extended with your measures. Indeed, Bechamel provides only GC measures.
Then, from your target, you can choose to use `perf` (with `bechamel-perf`) or
an other metric available in your system.

Some examples of `bechamel` are available in the `examples` directory. They want
to show:

- the standalone HTML output (available
  [here](https://mirage.github.io/bechamel/fact.html) this output is buildable with
  `dune build examples/fact.html`
- the CLI output (with `notty`)
- a benchmark with system specifics metrics (such as `perf`)

The end user is able to introspect result by himself and produce an other output
if he wants. Bechamel wants to be easy to use and extend. The documentation of
Bechamel is available [here](https://mirage.github.io/bechamel/). Then, the
library does not require too much dependencies.

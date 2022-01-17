# Bechamel - Agnostic benchmark tool in OCaml

Bechamel is a toolkit to do a _micro_-benchmarking on your functions. The user
is able to extend metrics (depending on your machine). Intially, Bechamel can
record monotonic clock & garbage collector. `bechamel-perf` can help you
for `perf` metrics if you are a Linux user.

Bechamel can show results:
- in your terminal with `bechamel-notty`
- _via_ a HTML + JavaScript page with `bechamel-js`

You can see an example of the produced HTML page [here][html-example]. Some
examples exist which take the opportunity of the output and metrics:
- [fact.ml][fact.ml] which produces an HTML + JavaScript report
- [list.ml][list.ml] which shows results into your terminal
- [sqrt.ml][sqrt.ml] which uses `perf` metrics

The documentation is available [here][documentation].

[html-example]: https://mirage.github.io/bechamel/fact.html
[fact.ml]: examples/fact.ml
[list.ml]: examples/list.ml
[sqrt.ml]: examples/sqrt.ml
[documentation]: https://mirage.github.io/bechamel

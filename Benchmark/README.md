## Stress Testing for the Traced Ticket Project

Run 

```
./xstress
```

The outcome is something like this:

```
--- measuring ---
'("16.6" "start:  ../Benchmark/xstress-base.out")
'("81.1" "start:  ../Benchmark/xstress-trace.out")
'("20" "start:  ../Benchmark/xstress-trace-no-load.out")
'("21.4" "start:  ../Benchmark/xstress-fold.out")
'("19.1" "start:  ../Benchmark/xstress-fold-no-load.out")
```

These timings are highly inaccurate, because it's just running five bash scripts five times.

- `xstress-base`
- `xstress-trace`
- `xstress-trace-no-load`
- `xstress-fold`
- `xstress-fold-no-load`

[ The names are suggestive. ]

Each of those compiles and then runs the test submodule of

- `xbench`

The final step is to run `measure.rkt` on the `*.out` files, which
collect the timing results. 

### The Benchmark

The benchmark runs a small number of tournaments as many times as `T#`
specifies. The variable is defined at the top:


```
(define T# 10) ;; how many times each tournament is run 
```

The tournaments rank from 5 players to 27 players, so one game to many
games in a row.

The benchmark includes a correctness check so changes to the code base
don't break the measurements (or we discover them quickly).


### The Timings

Even though `gtime` isn't very good with these bash scripts, it
suggests that the pure trace is "quadratic" in nature while fold seems
constant. That's not surprising because word recognition is called on
every call from the `referee` to the `player` and these traces get
longer over time.  Some of the players drop out so fewer word-check
problems are solved but still, it's N * K for some N and some K.

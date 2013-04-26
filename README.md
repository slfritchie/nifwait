Utility to test effect of blocking NIF on Erlang scheduler.

Test spawns several processes, all which will start in the run queue
of the current scheduler. Each spawned process then calls `usleep`
from within a NIF, sleeping for configurable duration. After the
sleep, the processes then performs a pure Erlang busy wait, looping
for configurable number of iterations. Process then exits.

There is a reporter process that periodically prints out the Erlang
run queues as well as an assessment of how balanced (or unbalanced)
the scheduler run queues are.

Compile with rebar: `rebar compile`

Run Erlang: `erl -pa ebin`

Use:
```erlang
  %%  wait:run(N, W, R, BW)
  %%  N = number of spawned procs
  %%  W = microseconds spent waiting in NIF
  %%  R = number of repeat calls to NIF
  %% BW = iterations of pure Erlang busy wait

  %% Example:
  wait:run(100,300000, 5, 10000).
```

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

While the schedulers may have a run queue of zero, all of the CPUs (or
as many schedulers as you have available, if you have specified a +S
argument to the `erl` command) are actually executing the custom NIF
functions -- see DTrace example below for details.  Geting scheduler
threads to actually go to sleep (i.e., stop executing useful work) is
a much trickier thing ... but it is indeed possible.  We have seen it
multiple customers affected by it, but it's a non-deterministic thing
to trigger, unfortunately.

When run on an OS that supports DTrace, the following (long) one-line
script can be used to get a better view of scheduler behavior:

    # dtrace -n 'pid$target::busywait_nif:entry {@[cpu] = count(); } pid$target::sleep_nif:entry {@[cpu] = count();  } tick-5s {printa(@); trunc(@); }' -p YOUR_BEAM_PID_NUMBER_HERE

Here is example output on a 8 core machine, running Erlang via `erl
-pz ebin +sbt db` and using the DTrace script above.  The test
function was run using `wait:run(4*100,300000, 5, 10000)`.

The DTrace output format is:

* Column #1: CPU number
* Column #2: number of NIF function entries per 5 second interval

      2  64905                         :tick-5s
            0                9
            1                9
            2                9
            3                9
            5                9
            7                9

      2  64905                         :tick-5s
            0               16
            4               16
            6               16
            1               17
            2               17
            3               17
            5               17
            7               17

      2  64905                         :tick-5s
            1               16
            2               16
            3               16
            4               16
            5               16
            6               16
            7               16
            0               17

      2  64905                         :tick-5s
            0               17
            1               17
            2               17
            3               17
            4               17
            5               17
            6               17
            7               17

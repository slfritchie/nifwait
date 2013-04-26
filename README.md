Utility to test effect of blocking NIF on Erlang scheduler.
===========================================================

Test spawns several processes, all which will start in the run queue
of the current scheduler. Each spawned process then calls `crypto` MD5
functions. Afterward, the processes then performs a pure Erlang busy
wait, looping for configurable number of iterations. Process then
exits.

There is a reporter process that periodically prints out the Erlang
run queues as well as an assessment of how balanced (or unbalanced)
the scheduler run queues are.

PLEASE NOTE: This branch of the https://github.com/slfritchie/nifwait
repository, `md5`, does not use any NIF code outside of the standard
Erlang/OTP distribution.  It only uses code that is included in
Erlang/OTP's `crypto` application.

Compile with rebar: `rebar compile` or simply run `make`.

Run Erlang: `erl -pa ebin`

Usage
-----

```erlang
  %%  wait:run(N, W, R, BW)
  %%  N = number of spawned procs
  %%  W = microseconds spent waiting in NIF
  %%  R = number of repeat calls to NIF
  %% BW = iterations of pure Erlang busy wait

  %% Example:
  wait:run(4*100, 1024*1024, 1100, 5).
```

When first run, the output looks something like this:

    1> wait:run(4*100, 1024*1024, 1100, 5).
    Ready ... go at {16,48,11}
    {16,48,11} All go messages sent
    {16,48,11} Start of status loop (0.004642 secs since first go)
    {16,48,11} Start of monitor loop (0.004658 secs since first go)
    {16,48,11} {unbalanced,105.0} {0,0,0,0,16,105,1,0}
    {16,48,12} {even,1.02} {37,37,36,38,37,38,18,38}
    [...]
    {16,49,55} {minority_zero,1.38} {49,49,49,50,48,68,0,19}
    {16,49,55} {even,1.38} {49,49,49,50,48,68,1,16}
    {16,50,1} {minority_zero,1.38} {49,49,49,50,48,68,1,0}
    [...]
    {16,50,14} {minority_zero,5.66} {0,12,36,50,30,68,0,0}
    {16,50,15} {minority_zero,68.0} {0,0,32,47,27,68,0,0}
    {16,50,20} {unbalanced,68.0} {0,0,0,0,1,68,0,0}
    {16,50,20} {unbalanced,62.0} {0,0,0,0,0,62,0,0}
    {16,50,21} {unbalanced,59.0} {0,0,0,0,0,59,1,0}
    [...]

At periodic intervals, there are reports to how well-balanced each of
the Erlang computation scheduler threads' run queues are balanced:

* `even`: All of the scheduler run queues have non-zero length
* `minority_zero`: A minority of schedulers have run queues with zero length.
* `unbalanced`: A majority of schedulers have run queues with zero length.

These three categories are accompanied by a floating point number.
This number is the ratio of the longest queue divided by the median
queue length.  (For the purposes of this calculation, a queue length
of zero is forced instead to be equal to one.)

The larger tuple of integers represents the output of the BIF
`erlang:statistics(run_queues)`.  Each integer represents the run
queue length of one of the Erlang scheduler threads.

The return value of run()
-------------------------

    {RunTimeList::list(ElapsedSeconds:float()),
     SWT_result::list({SchedulerNumber::integer(), PercentInUse:float())}

The `RunTimeList` is a list of wall-clock time from start to finish for
each of the worker processes.

By finding the min & max of the `RunTimeList`, we can see how
fair/unfair the scheduler was.  In many cases, the minimum and maximum
of a `wait:run(4*100, 1024*1024, 1100, 5)` run can differ by a factor
of 40x or more for example:

    R15B01
    {2.621872,126.171889}
    
    R16B
    {2.694947,139.574922}

... which illustrates tremendous unfairness in work scheduling with
this workload.

The `SWT_result` list uses the scheduled wall clock time feature of
the Erlang VM (R15B and later) to keep track of wall clock time used
for executing Erlang processes.  Percentages of X percent demonstrate
a scheduler thread that was executing Erlang/BIF/NIF code X percent of
wall clock time.


Problems making easy interpretation of scheduler thread run queue lengths
-------------------------------------------------------------------------

A queue length of zero may or may not indicate an idle scheduler
thread: using this statistic alone, it is impossible to tell the
idle/not-idle state of a scheduler thread.

While the schedulers may have a run queue of zero, all of the CPUs (or
as many schedulers as you have available, if you have specified a +S
argument to the `erl` command) are actually executing the `crypto`
module's MD5 functions -- see DTrace example below for details.
Geting scheduler threads to actually go to sleep (i.e., stop executing
useful work) is a much trickier thing ... but it is indeed possible.
We have seen it multiple customers affected by it, but it's a
non-deterministic thing to trigger, unfortunately.

"Scheduler Collapse", a.k.a. "stuck" or "sleeping" schedulers
-------------------------------------------------------------

The issue of scheduler collapse, or "stuck" or "sleeping" schedulers,
was discussed on the `erlang-questions` mailing list in October 2012.
Threads of that discussion can be found at:

* http://erlang.org/pipermail/erlang-questions/2012-October/069503.html
* http://erlang.org/pipermail/erlang-questions/2012-October/069585.html

At that time, the OTP team pointed out the incorrect use of the NIF
API by Riak (or, more specifically, by the `eleveldb` NIF that Riak
uses).  Basho invested a large amount of time re-writing and
re-optimizing the `eleveldb` to use a separate, asynchronous thread
pool to avoid blocking the Erlang VM inside NIF code for more than a
millisecond.  Those changes first appeared in the Riak 1.3.0 release.

However, there are several notable things (at least) that have
happened since October 2012:

* Ericsson released Erlang/OTP R16B.
* Basho has a confirmed case of scheduler collapse using Riak 1.3.1,
  which has all NIF code that has been coded to avoid all of the OTP
  team's restrictions on NIF API use.  Riak 1.3.1 is based on
  Erlang/OTP R15B01.
* Basho has confirmed cases of scheduler collapse in its Riak CS
  product.  Riak CS does not use any custom NIF code.  It does,
  however, make heavy use of the `crypto` module's MD5-related
  functions.
* It appears to be quite easy to reproduce scheduler collapse using
  Erlang/OTP R16B using the code in this repository ... Basho's
  testing efforts have a test case that only occasionally fails.
* It is still not possible to reproduce scheduler collapse
  consistently using the code in this repositiory ...  but it is
  possible to do, if you are sufficiently (un)lucky.

For R16B, the following test case has been sufficient on test machines
with 8, 12, and 16 HyperThread-enabled CPU cores -- it usually works.
When used with R15B01 and R15B03, it fails extremely rarely -- we have
not found a set of parameters that substantially changes the odds of
encountering the problem.

    erl +scl false -eval 'X1 = [begin io:format("\n\n** Iteration ~p\n", [X]), timer:tc(erlang, apply, [fun () -> XX = lists:sort(element(1,wait:run(4*100, 1024*1024, 1100, 5))), {hd(XX), lists:last(XX)} end, []]), timer:sleep(12*1000) end || X <- lists:seq(1,8)], io:format("X1 res = ~p\n", [X1]), timer:sleep(12*1000), erlang:system_flag(schedulers_online, 8), io:format("\n\nFinal iteration\n"), timer:sleep(12*1000), timer:tc(erlang, apply, [fun () -> XX = lists:sort(element(1,wait:run(4*100, 1024*1024, 1100, 5))), {hd(XX), lists:last(XX)} end, []]).'

NOTE: This was run on a 16 core machine.  The call to
`erlang:system_flag()` is intentionally cutting the number of
schedulers in use by 50%.  I don't know why this helps provoke the
problem, but it's usually effective.  I've also had luck by dropping
the number of schedulers by less than 50% (e.g., 10 of 16, or 12 of
16).

The faulty output looks like this:

    Final iteration
    Ready ... go at {18,46,25}
    {18,46,25} All go messages sent
    [...]
    SWT results (utilization per scheduler)
    [{1,19.240360369439895},
     {2,19.153491270981394},
     {3,19.707574790274847},
     {4,19.84002299398965},
     {5,100.0},
     {6,18.287743262256193},
     {7,18.30089343011695},
     {8,18.62498242809482},
     {9,7.713865632420574e-6},
     {10,6.531869350715166e-6},
     {11,6.4088225831743285e-6},
     {12,5.523523924585654e-6},
     {13,6.948950652722672e-6},
     {14,6.6810169928720635e-6},
     {15,6.88609543559383e-6},
     {16,4.0573491799633576e-6}]

Schedulers 9-16 are all idle.  That is fine.

Scheduler #5 is running at 100%.  That is fine.

Schedulers 1-4 and 6-8 are running at 18-19%.  That is bad.  This
demonstrates that those schedulers have gone to sleep for about 80% of
the test run.

I have other test runs that are more conclusive: 50% of the cores are
idle (expected), 1 core is 100% busy (expected), and all remaining
cores are less than 5% busy (buggy).

These results have been duplicated using R16B on OS X Mountain Lion,
CentOS 6.4, and SmartOS (IllumOS kernel) build 20130321T213641Z.

I'd love to hear reports from others about successful and unsuccessful
attempts to reproduce these results.

Examining the running system with DTrace
----------------------------------------

When run on an OS that supports DTrace, the following (long) one-line
script can be used to get a better view of scheduler behavior:

    # dtrace -n 'BEGIN {bytes = 0; total = 0;} pid$target::MD5_Update:entry {@[cpu] = count(); total++; bytes += arg2} tick-5s {printa(@); printf("Total calls: %d\n", total); printf("Total MBytes: %d\n", (bytes / (1024*1024))); trunc(@); total = 0; bytes = 0}' -p YOUR_BEAM_PID_NUMBER_HERE

Here is example output on a 8 core machine, running Erlang via `erl
-pz ebin +sbt db` and using the DTrace script above.  The test
function was run using `wait:run(4*100, 1024*1024, 1100, 5)`.

The DTrace output format is (on an 8 core machine):

* Column #1: CPU number
* Column #2: number of MD5_Update() function calls per 5 second interval

    dtrace: description 'BEGIN ' matched 3 probes
    CPU     ID                    FUNCTION:NAME
      4   2230                         :tick-5s
            4             1951
            6             1963
            2             1999
            0             2017
            1             2044
            3             2044
            5             2048
            7             2050
    Total calls: 16027
    Total MBytes: 16027

      4   2230                         :tick-5s
            0             2010
            2             2019
            6             2034
            4             2045
            5             2052
            3             2056
            7             2056
            1             2060
    Total calls: 16309
    Total MBytes: 16308

      4   2230                         :tick-5s
            6             1918
            0             1923
            4             1973
            2             1976
            5             2032
            3             2048
            7             2060
            1             2063
    Total calls: 15979
    Total MBytes: 15980

So, all of the schedulers are executing the `MD5-Update()` function at
an (approximately) even rate.  However, at the same time, it's
possible to see `wait:run()`'s output that says this (on a 16 core
machine):

    {18,44,9} {unbalanced,87.0} {0,0,0,0,0,0,0,0,0,0,0,0,0,87,3,0}
    {18,44,9} {unbalanced,84.0} {0,0,0,0,0,0,0,0,0,0,0,0,0,84,1,0}
    {18,44,10} {unbalanced,81.0} {0,0,0,0,0,0,0,0,0,0,0,0,0,81,1,0}
    {18,44,11} {unbalanced,78.0} {0,0,0,0,0,0,0,0,0,0,0,0,0,78,4,0}
    {18,44,12} {unbalanced,69.0} {0,0,0,0,0,0,0,0,0,0,0,0,0,69,1,0}
    {18,44,13} {unbalanced,68.0} {0,0,0,0,0,0,0,0,0,0,0,0,0,68,4,0}

DTrace is confirming that the schedulers with queue length of zero are
doing real work in this case.  However, they appear to be taking only
a single queue item from (probably) scheduler #14; thus their run
queue lengths remain zero.

In the event of scheduler collapse, DTrace confirms that the same
number of CPU cores reported by the SWT information are indeed the
number of CPU cores that are actually executing `MD5_Update()`.

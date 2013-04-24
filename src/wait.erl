-module(wait).
-compile(export_all).
%% -on_load(init/0).

%%  N = number of spawned procs
%%  W = size of binary (in bytes) to MD5
%%  R = number of repeat calls MD5
%% BW = iterations of pure Erlang busy wait
%%
run([NS, WS, RS, BWS]) ->
    run(list_to_integer(NS),
        list_to_integer(WS),
        list_to_integer(RS),
        list_to_integer(BWS)).

run(N,W,R,BW) ->
    Repeat = lists:seq(0,R-1),
    Bin = random_binary(W, <<>>),
    Parent = self(),
    Pids = spawn_n(N, fun() ->
                              receive
                                  go ->
                                      ok
                              end,
                              Start = now(),
                              put(rrr, N),
                              lists:foreach(fun(_) ->
                                                    %report(Parent),
                                                    M1 = crypto:md5_init(),
                                                    M2 = crypto:md5_update(M1, Bin),
                                                    crypto:md5_final(M2),
                                                    busywait(BW)
                                            end, Repeat),
                              %report(Parent),
                              Diff = timer:now_diff(now(), Start) / 1000000,
                              %io:format("~p done, elapsed ~p seconds\n", [self(), Diff]),
                              Parent ! {done, self(), Diff}
                      end),
    io:format("Ready ... "),
    timer:sleep(1000),
    io:format("go\n"),
    Start = now(),
    [Child ! go || Child <- Pids],
    MonPid = spawn_link(fun() -> process_flag(priority, high),
                                 monitor_loop(Parent)
                        end),
    Res = status_loop(Pids, Start),
    MonPid ! stop,
    Res.

status_loop(Pids, Start) ->
    status_loop(Pids, Start, 0.0).

status_loop([], _Start, _LastBalance) ->
    [];
status_loop(Pids, Start, LastBalance) ->
    receive
         {done, Child, Diff} ->
             [Diff|status_loop(Pids -- [Child], Start, LastBalance)];
         {status, _Child, RQ, _Reds, _Now} ->
            {Balance, RQ} = calc_balance(RQ),
            if Balance /= LastBalance ->
                    io:format("~p ~p ~p\n", [time(), Balance, RQ]);
               true ->
                    ok
            end,
            status_loop(Pids, Start, Balance)
     end.

monitor_loop(Parent) ->
    receive
        stop ->
            ok
    after 0 ->
            report(0, Parent),
            Start = erlang:now(),
            timer:sleep(500),
            io:format("slept ~p, ", [timer:now_diff(now(), Start)]),
            monitor_loop(Parent)
    end.

calc_balance(RQ) ->
    L0 = tuple_to_list(RQ),
    Type = case lists:foldl(fun(0, Acc) -> Acc+1;
                               (_, Acc) -> Acc
                            end, 0, L0) of
               0 ->
                   even;
               Len when Len =< (size(RQ) / 2) ->
                   minority_zero;
               _ ->
                   unbalanced
           end,
    Max = lists:max(L0),
    Median = erlang:max(1, lists:nth(length(L0) div 2, lists:sort(L0))),
    Ratio = trunc(100 * Max / Median) / 100,
    %% {Type, Ratio, RQ}.
    {{Type, Ratio}, RQ}.

report(Parent) ->
    report(get(rrr), Parent).

report(0, Parent) ->
    put(rrr, 1),
    {reductions, Reds} = process_info(self(), reductions),
    RQ = erlang:statistics(run_queues),
    Parent ! {status, self(), RQ, Reds, now()};
report(N, _Parent) ->
    put(rrr, (N + 1) rem 1000).
   
spawn_n(0, _) ->
    [];
spawn_n(N, F) ->
    [spawn_link(F)|spawn_n(N-1, F)].

busywait(0) ->
    ok;
busywait(N) ->
    busywait(N-1).

busywait_nif(_) ->
    not_loaded.

sleep_nif(_) ->
    not_loaded.

init() ->
    case code:which(?MODULE) of
        Filename when is_list(Filename) ->
            NIF = filename:join([filename:dirname(Filename),"../priv", "wait"]),
            erlang:load_nif(NIF, 0)
    end.

random_binary(0, Bin) ->
    Bin;
random_binary(N, Bin) ->
    X = random:uniform(255),
    random_binary(N-1, <<Bin/binary, X:8/integer>>).

%% Using DTrace command:
%%
%% dtrace -n 'BEGIN {bytes = 0; total = 0;} pid68511::MD5_Update:entry {@[cpu] = count(); total++; bytes += arg2} tick-5s {printa(@); printf("Total calls: %d\n", total); printf("Total MBytes: %d\n", (bytes / (1024*1024))); trunc(@); total = 0; bytes = 0}'
%%
%% On my (new) MacBook Pro (4 core + HT), R15B03:
%%
%% wait:run(1, 1024*1024, 10000, 500).
%% typical output:
%%    Total calls: 2373
%%    Total MBytes: 2373
%% wait:run(2, 1024*1024, 10000, 500).
%% typical output:
%%    Total calls: 4661
%%    Total MBytes: 4658
%% wait:run(4, 1024*1024, 10000, 500).
%% typical output:
%%    Total calls: 8933
%%    Total MBytes: 8934
%% wait:run(8, 1024*1024, 10000, 500).
%% typical output:
%%    Total calls: 14152
%%    Total MBytes: 14150
%% wait:run(16, 1024*1024, 10000, 500).
%% typical output:
%%    Total calls: 14446
%%    Total MBytes: 14443

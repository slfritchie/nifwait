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
    Repeat = lists:seq(1,R),
    Bin = random_binary(W, <<>>),
    Parent = self(),
    Pids = spawn_n(N, fun() ->
                              receive
                                  go ->
                                      ok
                              end,
                              Start = now(),
                              put(rrr, 0),
                              report(),
                              lists:foreach(fun(_) ->
                                                    report(),
                                                    M1 = crypto:md5_init(),
                                                    M2 = crypto:md5_update(M1, Bin),
                                                    crypto:md5_final(M2),
                                                    busywait(BW)
                                            end, Repeat),
                              report(),
                              report(),
                              Diff = timer:now_diff(now(), Start) / 1000000,
                              io:format("~p done, elapsed ~p seconds\n", [self(), Diff]),
                              Parent ! {done, self(), Diff}
                      end),
    io:format("Ready ... "),
    timer:sleep(1000),
    io:format("go\n"),
    [Child ! go || Child <- Pids],
    [receive
         {done, Child, Diff} ->
             Diff
     end || Child <- Pids].

report() ->
    report(get(rrr)).

report(0) ->
    put(rrr, 1),
    {reductions, Reds} = process_info(self(), reductions),
    RQ = erlang:statistics(run_queues),
    erlang:display({RQ, Reds, time(), self()});
report(N) ->
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

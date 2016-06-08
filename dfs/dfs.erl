-module(dfs).
-export([execute/2, waitforpids/2, printall/1, test/0]).
%% This code returns Asynchronous concurrent-initiator depth first search spanning tree
%% The code takes input of a graph data file of the form
%% 3. %%Number of nodes
%% [2, 3].  %%1st node is connected to 2 and 3
%% [1, 3].  %%2nd node is connected to 1 and 3
%% [1, 2].  %%3rd node is connected to 1 and 2

%% It also takes input of the machines it has in a file
%% 't1@home'.
%% 't2@home'.
%% etc

test() ->
    execute('data.txt', 'computer.txt').

execute(Graphfile, Machinefile) ->
    {ok, Graph} = file:consult(Graphfile),
    {ok, Machines} = file:consult(Machinefile),
    start(Graph, Machines).

%%This function is used to spawn all the nodes and recieve the result
%%ListofNodes of the form [{Nodenumber, Pid}]
start(Graph, Machines) ->
    [Numberofnodes | Graphn] = Graph,
    Self = self(),
    ListofNodes = for(1, Numberofnodes, Machines, Self, fun(Mach_name, Curr_nodenumber, Selfi) -> spawn(Mach_name, ?MODULE, waitforpids, [Curr_nodenumber, Selfi])end),
    sendlist(1, length(Graphn), Graphn, ListofNodes),
    receive
        {iamroot, Node, Children, _Pid} -> 
            io:format("~p is the root ~n",[Node]),
            printall(Children),
            Printothers = lists:keydelete(Node, 1, ListofNodes),
            returnall(true, 1, length(Printothers),Printothers),
            killall(ListofNodes)
    end.

returnall(false, Max, Max, _ListofNodes) ->
    receive
        {returning, Node, Children, _pid} ->
            io:format("Children of Node ~p~n", [Node]),
            printall(Children),
            io:format("~nAll the nodes children are given~n")
    after 8000 ->
              io:format("Timeout received~n")
    end;
returnall(Start, I, Max, ListofNodes) ->
    case Start of
        true ->
            lists:foreach(fun({_node, Pid})-> Pid ! {return} end, ListofNodes),
            returnall(false, I, Max, ListofNodes);
        false ->
            receive
                {returning, Node, Children, _pid} ->
                    io:format("Children of Node ~p~n", [Node]),
                    printall(Children),
                    returnall(false, I+1, Max, ListofNodes)
            after 8000 ->
                      io:format("Timeout received~n")
            end
    end.

killall([]) -> io:format("All the nodes are killed~n");
killall([{_, Pid} |T]) ->
    exit(Pid, kill),
    killall(T).

%%This sends the list of neighbours of the form [{Nodenumber, Pid}]
sendlist(_,_,[],_) -> io:format("All are sent :\)~n");
sendlist(I, Max, Graph, ListofNodes) ->
    [H|T] = Graph,
    sendeach(I, H, ListofNodes, []),
    sendlist(I+1, Max, T, ListofNodes).

%%This is a helper function for sending the list to the particular node
sendeach(I, [], ListofNodes, Ans) -> 
    io:format("Send at last~n"),
    Sendto = proplists:get_value(I, ListofNodes),
    Sendto ! {neighbours, Ans};
sendeach(I, [H|T], ListofNodes, Ans) ->
    {Nodenumber, Pid} = lists:nth(H, ListofNodes),
    sendeach(I, T, ListofNodes, [{Nodenumber, Pid}|Ans]).


%%This is the buffer period of the node when it is waiting for its neighbour pids and then it starts executing the actual dfs
waitforpids(Nodenumber, Adam) ->
    io:format("~p Waiting for pid with Adam ~p self ~p~n", [Nodenumber, Adam, self()]),
    receive
        {neighbours, Neighbours} -> 
            %%call dfs
            io:format("Hello~n"),
            dfs(Adam, Nodenumber, -1, -1, Neighbours, [], [])
            %io:format("Received the List~n"),
            %printall(Neighbours)
    end.

%%dfs
%%Parent = 0 means there is no parent as of now but it has sent a query to itself
%%Parent = -1 means there is no parent and it has not even sent a query to itself
dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children) ->
    case Parent of
        -1 -> 
            self() ! {queryy, Nodenumber, {from, Nodenumber, self()}},
            io:format("Node ~p has sent query to itself~n", [Nodenumber]),
            dfs(Adam, Nodenumber, 0, 0, Neighbours, [], Children);
        _ ->
            receive
                {return} ->
                    Adam ! {returning, Nodenumber, Children, self()};
                {queryy, Newroot, {from, Jnodeno, Jpid}} ->
                    case Newroot > Myroot of
                        true ->
                            Newparent = Jnodeno,
                            Mynewroot = Newroot,
                            NewUnknown = lists:keydelete(Jnodeno, 1, Neighbours), %%Unknown = Neighbours\{J}
                            case length(NewUnknown) > 0 of
                                true -> 
                                    [H | LeftUnknown] = NewUnknown,
                                    {_Node, Sendto} = H,
                                    Sendto ! {queryy, Mynewroot, {from, Nodenumber, self()}},
                                    dfs(Adam, Nodenumber, Newparent, Mynewroot, Neighbours, LeftUnknown, Children);
                                false ->
                                    Jpid ! {accept, Mynewroot, {from, Nodenumber, self()}},
                                    dfs(Adam, Nodenumber, Newparent, Mynewroot, Neighbours, NewUnknown, Children)
                            end;
                        false ->
                            case Newroot == Myroot of
                                true ->
                                    Jpid ! {reject, Myroot, {from, Nodenumber, self()}},
                                    dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children);
                                false ->
                                    io:format("From Query Ignore this query as Newroot < Myroot ~n"),
                                    dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children)
                            end
                    end;

                {accept, Newroot, {from, Jnodeno, Jpid}} ->
                    case Newroot == Myroot of
                        true ->
                            NewChildren = [{Jnodeno, Jpid} | Children],
                            case length(Unknown) > 0 of
                                false ->
                                    case Parent == Nodenumber of
                                        false ->
                                            Parentpid = proplists:get_value(Parent, Neighbours),
                                            Parentpid ! {accept, Myroot, {from, Nodenumber, self()}},
                                            dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, NewChildren);
                                        true ->
                                            Adam ! {iamroot, Nodenumber, NewChildren, self()},
                                            dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, NewChildren)
                                    end;
                                true ->
                                    [H | LeftUnknown] = Unknown,
                                    {_Node, Sendto} = H,
                                    Sendto ! {queryy, Myroot, {from, Nodenumber, self()}},
                                    dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, LeftUnknown, NewChildren)
                            end;

                        false ->
                            io:format("From Accept Condition where newroot < myroot~n"),
                            dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children)
                    end;

                {reject, Newroot, {from, _Jnodeno, _Jpid}} ->
                    case Newroot == Myroot of
                        true ->
                            case length(Unknown) > 0 of
                                false ->
                                    case Parent == Nodenumber of
                                        false ->
                                            Parentpid = proplists:get_value(Parent, Neighbours),
                                            Parentpid ! {accept, Myroot, {from, Nodenumber, self()}},
                                            dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children);
                                        true ->
                                            Adam ! {iamroot, Nodenumber, Children, self()},
                                            dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children)
                                    end;
                                true ->
                                    [H | LeftUnknown] = Unknown,
                                    {_Node, Sendto} = H,
                                    Sendto ! {queryy, Myroot, {from, Nodenumber, self()}},
                                    dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, LeftUnknown, Children)
                            end;
                        false ->
                            io:format("From Reject Condition where newroot < myroot~n"),
                            dfs(Adam, Nodenumber, Parent, Myroot, Neighbours, Unknown, Children)
                    end

            end
    end.

printall([]) -> io:format("~n");
printall([H|T]) ->
    io:format("~p ", [H]),
    printall(T).

for(Max, Max, Machines, Adam, F) -> [{Max, F(lists:nth(random:uniform(length(Machines)), Machines), Max, Adam)}];
for(I, Max, Machines, Adam, F) -> [{I, F(lists:nth(random:uniform(length(Machines)), Machines), I, Adam)}|for(I+1, Max, Machines, Adam, F)].

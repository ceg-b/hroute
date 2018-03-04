-module(parse).
-export([linee/1,content/0,to_lines/2,is_prefix_of/2,is_infix_of/2,exclude/1,mmain/0]).


linee ([]) ->
    {[],[]};

linee (X) ->
    Tmp = lists:takewhile(fun(A)-> A /= $\n end,X),
    Res1 = lists:dropwhile(fun(A) ->A /= $\n end,X),
    Res = case Res1 of
	      [] -> Res1;
	      [_|T] -> T
	  end,
    {Tmp,Res}.


content() ->
    {ok,Cont}=file:read_file("RMServer.cpp"),
    Cont1 = binary:bin_to_list(Cont),
    Cont1.

to_lines(State,Rest) ->		
    Parsed=linee(Rest),
    case Parsed of
        {Line,[]}  -> [ Line | State ];
        {[],[]}    -> State;
        {Line,Sth} -> to_lines([Line|State],Sth)
    end.
             
is_prefix_of(Pattern,String) ->
%    io:format("isPrefix: ~p~n", {Pattern,String}),
    Ll = erlang:length(Pattern),
    if 
        Ll > erlang:length(String) -> never;
        true -> case Pattern == lists:sublist(String,Ll) of
                    true -> yes;
                    _    -> no
                end
    end.
             
is_infix_of(Pattern,String) ->		   
    Tt=is_prefix_of(Pattern,String),
    case Tt of
        yes -> true;
        no  -> is_infix_of(Pattern,lists:nthtail(1,String));
        never -> false
    end.
                       


sfilter(Line,init) ->            
    case is_infix_of("#ifdef LOGGER",Line) of
        true  -> ifdef;
        false -> init
    end;

sfilter(Line,ifdef) -> 
    T1=is_infix_of("#else",Line),
    T2=is_infix_of("#endif",Line),
    case {T1,T2} of
        {true,_} -> else;
        {_,true} -> init;
        {_,_}    -> ifdef
    end;

sfilter(Line,else) -> 
    case is_infix_of("#endif",Line) of
        true -> init;
        false -> else
    end.
             
pf(X,{Lines,State}) ->
    NState = sfilter(X,State),
    case {State,NState} of
        {init,init} -> {[X|Lines],init};
        {init,ifdef} -> {Lines,ifdef};
        
        {ifdef,else} -> {Lines,else};
        {ifdef,ifdef} ->{Lines,ifdef};
        {ifdef,init} ->{Lines,init};
        
        {else,init}-> {Lines,init};
        {else,else} -> {[X|Lines],ifdef}
    end.

exclude(Content) ->
    {T,_}=lists:foldl(fun pf/2,{[],init},Content),
    T.

mmain() ->
    XX=content(),
    YY=to_lines([],XX),
    ZZ=lists:reverse(exclude(lists:reverse(YY))),
    LineSep = io_lib:nl(),
    Print = [string:join(ZZ, LineSep), LineSep],
    file:write_file("xxx", Print).




                                 

             


                    
             
   

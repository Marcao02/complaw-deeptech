/*
20170613-20:56:31 mengwong@venice2:~/non-db-src/l/compiler/learn-prolog% swipl marriage.pl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.3.20-79-g41f0976-DIRTY)
Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- possibleWeddings(mon, fri, X).
If you were single on mon and married by fri then the marriage could have happened on any of [tue,wed,thu,fri]
X = [tue, wed, thu, fri].

?-
% halt
*/


%% 
%% for extra credit, reimplement this using http://www.swi-prolog.org/man/clpfd.html
%% 


use_module(library(lists)).

% a simple logic to infer marriage events

% our temporal logic has exactly seven days, one after the other.
nextDay( sun, mon).
nextDay(      mon, tue).
nextDay(           tue, wed).
nextDay(                wed, thu).
nextDay(                     thu, fri).
nextDay(                          fri, sat).

% lastDay/1: the last day has no successor.
lastDay(X)  :- not(nextDay(X,_)).

% firstDay/1: the first day has no predecessor.
firstDay(X) :- not(nextDay(_,X)).

% after/2: a list of all the days that come after the given day
after(X,Out) :- lastDay(X), Out=[].
after(X,Out) :- nextDay(X,Y), after(Y,Z), Out = [Y|Z].

% before/2: a list of all the days that come before the given day
before_(X,Out) :- firstDay(X), Out=[].
before_(X,Out) :- nextDay(W,X), before_(W,Z), Out = [W|Z].

before(X,Out) :- before_(X,Y), reverse(Y,Out).

% between/3: a list of all the days between X and Y.
between(X,Y,Out) :- after(X,Xs), before(Y,Ys), intersection(Xs,Ys,Out).

% in this simple example, which is set prior to Henry VIII, a person can only ever get married.

% traceMarriage/2 computes the trace of all days on which the person was single, and when the person was married.
traceMarriage(KnownSingle, KnownMarried, SingleDays, MarriedDays) :-
    AllDays=[sun,mon,tue,wed,thu,fri,sat],
    append(SingleDays,MarriedDays,AllDays), % this is where the magic happens
    member(KnownSingle,  SingleDays),
    member(KnownMarried, MarriedDays).

% weddingDay solves for a day on which the person could have gotten married. hit space to walk through all the possibilities.
weddingDay(KS, KM, X) :- traceMarriage(KS, KM, _,[X|_]).

% possibleWeddings collects all the weddingDays into a list.
possibleWeddings(KS, KM) :- possibleWeddings_(KS, KM, Bag),
                            format("If you were single on ~w and married by ~w then the wedding ", [KS, KM]),
                            length(Bag, L),
                            L is 1 -> ( Bag = [B], format("must have happened on ~w!", [B]) );
                            format("could have happened on any of ~w.", [KS, KM, Bag]).

% utility function to hide the bag output text.
possibleWeddings_(KS, KM, Bag) :- bagof(X, weddingDay(KS, KM, X), Bag).

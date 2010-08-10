% File:     loader.mm_variants.pl
% Module:   MM Variants
% Author:   Lan
% Purpose:  Loads MM Variants

:- use_module(mm_variants,[
    go/0,
    go/1,
    go/2,
    stop_mm_variants/0
    ]).

:- use_module(skr_lib(nls_signal),[
    establish_signal_handling/0
    ]).


runtime_entry(start) :-
    establish_signal_handling,
    go.
runtime_entry(abort) :-
    format(user_output,'~nDisconnecting servers and closing files...',[]),
    ttyflush,
    stop_mm_variants,
    format(user_output,'Done.~n',[]).

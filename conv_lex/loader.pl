% File:     loader.mm_variants.pl
% Module:   MM Variants
% Author:   Lan
% Purpose:  Loads MM Variants

:- use_module(conv_lex, [
	go/0
    ]).

:- use_module(skr_lib(nls_signal), [
	establish_signal_handling/0
    ]).

:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
    ]).

runtime_entry(start) :-
	establish_signal_handling,
	go.
runtime_entry(abort) :-
	ttyflush,
	format(user_output, 'Done.~n', []).

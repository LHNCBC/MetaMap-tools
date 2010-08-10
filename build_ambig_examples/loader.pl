% File:     loader.pl
% Module:   Build Ambiguity Examples
% Author:   Lan
% Purpose:  Loads Build Ambiguity Examples

:- use_module(build_ambig_examples, [
	go/0
    ]).

:- use_module(skr_lib(nls_signal), [
	establish_signal_handling/0
    ]).

:- use_module(skr_lib(sicstus_utils),[
        ttyflush/0
    ]).

:- use_module(library(file_systems), [
	close_all_streams/0
    ]).


runtime_entry(start) :-
	establish_signal_handling,
	go.
runtime_entry(abort) :-
	format(user_output, '~nClosing files...', []),
	ttyflush,
	close_all_streams,
	format(user_output, 'Done.~n', []).

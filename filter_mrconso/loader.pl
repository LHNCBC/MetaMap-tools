% File:     loader.pl
% Module:   Filter Mrconso
% Author:   Lan
% Purpose:  Loads filter_mrconso


:- use_module(filter_mrconso,[
	go/0,
	stop_filter_mrconso/0
   ]).

:- use_module(skr_lib(nls_signal),[
	establish_signal_handling/0
   ]).

:- use_module(skr_lib(sicstus_utils),[
        ttyflush/0
    ]).

runtime_entry(start) :-
	establish_signal_handling,
	go.
runtime_entry(abort) :-
	format(user_output, '~nDisconnecting servers and closing files...', []),
	ttyflush,
	stop_filter_mrconso,
	format(user_output, 'Done.~n', []).

% File:     loader.pl
% Module:   mm_print
% Author:   Lan
% Purpose:  Load the module.

:- use_module(mm_print, [
	go/0,
	go/1,
	go/2,
	gt/0,
	stop_mm_print/0
    ]).

:- use_module(skr_lib(sicstus_utils), [
        ttyflush/0
    ]).

runtime_entry(start) :-
	go.
runtime_entry(abort) :-
	format(user_output, '~nDisconnecting servers and closing files...', []),
	ttyflush,
	stop_mm_print,
	format(user_output, 'Done.~n', []).

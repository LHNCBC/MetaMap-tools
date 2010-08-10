% File:     loader.pl
% Module:   Glean Mrcon
% Author:   Lan
% Purpose:  Loads glean_mrcon

:- use_module(glean_mrcon, [
	go/0
   ]).

:- use_module(skr_lib(sicstus_utils), [
        ttyflush/0
   ]).

:- use_module(library(file_systems), [
	close_all_streams/0
   ]).


runtime_entry(start) :-
	go.
runtime_entry(abort) :-
	format(user_output, '~nClosing files...', []),
	ttyflush,
	close_all_streams,
	format(user_output, 'Done.~n', []).

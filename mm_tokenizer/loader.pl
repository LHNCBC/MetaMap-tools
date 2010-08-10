% File:     loader.pl
% Module:   MM Tokenizer
% Author:   Lan
% Purpose:  Provide batch tokenization based on the metamap_tokenization
%           module
:- use_module(mm_tokenizer,[
	go/0,
	go/1,
	go/2
    ]).

:- use_module(skr_lib(nls_signal),[
	establish_signal_handling/0
    ]).

:- use_module(skr_lib(sicstus_utils),[
        ttyflush/0
    ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

runtime_entry(start) :-
	establish_signal_handling,
	go.
runtime_entry(abort) :-
	format(user_output, '~nClosing all files...', []),
	ttyflush,
	close_all_streams,
	format(user_output, 'Done.~n', []).

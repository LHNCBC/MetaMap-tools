% File:     mm_tokenizer.pl
% Module:   MM Tokenizer
% Author:   Lan
% Purpose:  Provide batch tokenization based on the metamap_tokenization
%           module

:- module(mm_tokenizer,[
	go/0,
	go/1,
	go/2
    ]).

:- use_module(lexicon(lexical),[
	lowercase_list/2
    ]).

:- use_module(mm_tools_lib(mwi_utilities), [
	compute_unique_filename/3
   ]).

:- use_module(metamap(metamap_tokenization),[
	tokenize_text/2,
	tokenize_text_more/2,
	tokenize_text_more_lc/2,
	tokenize_text_mm/2,
	tokenize_text_mm_lc/2
    ]).

:- use_module(skr_db(db_access),[
	default_release/1
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_io),[
	fget_line/2
    ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2
   ]).

:- use_module(skr_lib(nls_system), [
	get_control_options_for_modules/2,
	reset_control_options/1,
	toggle_control_options/1,
	set_control_values/2,
	display_control_options_for_modules/2,
	display_current_control_options/2,
	control_option/1,
	parse_command_line/1,
	interpret_options/4,
	interpret_args/4,
	get_from_iargs/4
    ]).

/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for MM Tokenizer.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
	go(halt).

go(HaltOption) :-
	parse_command_line(CLTerm),
	go(HaltOption, CLTerm).

go(HaltOption,command_line(Options,Args)) :-
	% use_module(library(printchars)),
	reset_control_options(mm_tokenizer),
	format('~nMetaMap Tokenizer~n~n', []),
	( initialize_mm_tokenizer(Options, Args, InterpretedArgs) ->
	    ( mm_tokenizer(InterpretedArgs) ->
	      true
	    ; true
	    )
	; usage
	),
	( HaltOption == halt ->
	  halt
	; true
	).

/* initialize_mm_tokenizer(+Options, +Args, -InterpretedArgs)

initialize_mm_tokenizer/3 interprets command line options and arguments (opening
files as necessary) and sets and displays the MM Tokenizer control options
discovered.  */

initialize_mm_tokenizer(Options,Args,IArgs) :-
    get_control_options_for_modules([mm_tokenizer],AllOptions),
    interpret_options(Options,AllOptions,mm_tokenizer,IOptions),
    \+memberchk(iopt(help,_),IOptions),
    ArgSpecs=[aspec(infile,mandatory,file,read,
                           no_default,
                           'Input file containing text to be tokenized'),
             aspec(outfile,mandatory,file,write,
                           ['<infile>','.','out'],
                           'Output file')
            ],
    interpret_args(IOptions,ArgSpecs,Args,IArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,IArgs),
    default_release(Release),
    display_current_control_options(mm_tokenizer, Release),
    find_tokenization_options(IOptions,TokOptions),
    length(TokOptions,NTokOptions),
    (NTokOptions =:= 1 ->
        true
    ;   format('ERROR: Exactly one form of tokenization (-s, -w, -m, or -c) must be specified.~n',[]),
        !,
        fail
    ),
    !.


find_tokenization_options([],[]).
find_tokenization_options([First|Rest],[First|FilteredRest]) :-
    First=iopt(TokOption,_),
    (TokOption=='whitespace_tokenization';
     TokOption=='wordind_tokenization';
     TokOption=='metamap_tokenization';
     TokOption=='complete_tokenization'),
    !,
    find_tokenization_options(Rest,FilteredRest).
find_tokenization_options([_|Rest],FilteredRest) :-
    find_tokenization_options(Rest,FilteredRest).


/* usage

usage/0 displays MM Tokenizer usage.  */

usage :-
    format('~nUsage: mm_tokenizer [<options>] <infile> [<outfile>]~n~n',[]),
    format('  <infile> contains arbitrary text to be tokenized,~n',[]),
    format('  and <outfile> is a file for results (default is <infile>.out).~n~n',[]),
    display_control_options_for_modules(mm_tokenizer,[]).


/* mm_tokenizer(+InterpretedArgs)

mm_tokenizer/1 redirects I/O streams and then calls process_all/2.  */

mm_tokenizer(InterpretedArgs) :-
    get_from_iargs(infile,name,InterpretedArgs,InputFile),
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    get_from_iargs(outfile,name,InterpretedArgs,OutputFile),
    get_from_iargs(outfile,stream,InterpretedArgs,OutputStream),
    format('~n~nBeginning to process ~a sending output to ~a.~n~n',
           [InputFile,OutputFile]),
    process_all(InputStream,OutputStream),
    close(InputStream),
    close(OutputStream),
    format('~nBatch processing is finished.~n',[]).
mm_tokenizer(_InterpretedArgs).


/* process_all(+InputStream, OutputStream)

process_all/0 reads text from InputStream and writes the text and all
specified tokenizations of the text (pipe-separated).  */

process_all(InputStream,OutputStream) :-
    repeat,
    maybe_atom_gc(_,_),
    (fget_line(InputStream,Text) ->
        process_text(Text,OutputStream),
        fail
    ;   true
    ),
    !.


/* process_text(+Text, +OutputStream)

process_text/2 tokenizes Text according to the specified options and writes
Text and tokens to OutputStream.  */

process_text("",_OutputStream) :-
    !.
process_text(Text,OutputStream) :-
    control_option(whitespace_tokenization),
    !,
    (control_option(lower_case) ->
        tokenize_text_lc(Text,NormTText)
    ;   tokenize_text(Text,NormTText)
    ),
    write_tokens(NormTText,Text,OutputStream).
process_text(Text,OutputStream) :-
    control_option(wordind_tokenization),
    !,
    (control_option(lower_case) ->
        tokenize_text_more_lc(Text,MoreTText)
    ;   tokenize_text_more(Text,MoreTText)
    ),
    write_tokens(MoreTText,Text,OutputStream).
process_text(Text,OutputStream) :-
    control_option(metamap_tokenization),
    !,
    (control_option(lower_case) ->
        tokenize_text_mm_lc(Text,MMTText)
    ;   tokenize_text_mm(Text,MMTText)
    ),
    write_tokens(MMTText,Text,OutputStream).
process_text(Text,OutputStream) :-
    control_option(complete_tokenization),
    !,
    (control_option(lower_case) ->
        tokenize_text_completely_lc(Text,CompleteTText)
    ;   tokenize_text_completely(Text,CompleteTText)
    ),
    write_tokens(CompleteTText,Text,OutputStream).

write_tokens(Tokens0,Text,OutputStream) :-
    \+control_option(prolog_output),
    !,
    (control_option(unique_tokens) ->
	sort(Tokens0,Tokens)
    ;   Tokens=Tokens0
    ),
    write_each_token(Tokens,Text,OutputStream).
write_tokens(Tokens0,Text,OutputStream) :-
    control_option(prolog_output),
    !,
    (control_option(unique_tokens) ->
	sort(Tokens0,Tokens)
    ;   Tokens=Tokens0
    ),
    write_term(OutputStream,Text,[quoted(true),portrayed(true)]),
    format(OutputStream,'|',[]),
    write_term(OutputStream,Tokens,[quoted(true),portrayed(true)]),
    format(OutputStream,'.~n',[]).

write_each_token([], _, _).
write_each_token([Token|Rest], Text, OutputStream) :-
	( control_option(brief_output) ->
	  format(OutputStream,'~s~n' ,[Token])
	; format(OutputStream, '~s|~s~n', [Text,Token])
	),
	flush_output(OutputStream),
	write_each_token(Rest, Text, OutputStream).


tokenize_text_lc(Text,TokText) :-
    tokenize_text(Text,TokText0),
    lowercase_list(TokText0,TokText).

tokenize_text_completely(Text,TokText) :-
    (   atom(Text) ->
        atom_codes(Text,String),
        phrase(ttc_string(TokString),String),
        atom_codes_list(TokText,TokString)
%    ;   is_print_string(Text) ->
    ;   phrase(ttc_string(TokText),Text)
    ),
    !.


tokenize_text_completely_lc(Text, TokText) :-
	tokenize_text_completely(Text, TokText0),
	lowercase_list(TokText0, TokText).

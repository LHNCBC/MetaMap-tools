% File:     flip_variants.pl
% Module:   Flip Variants
% Author:   Lan
% Purpose:  Transforms files of variants of the form
%           <word>|<wcat>|<variant>|<vcat>|<var level>|<history>|<roots>
%           to those of the form
%           <variant>|<vcat>|<word>|<wcat>|<var level>|<flipped history>|[]
%           effectively reversing the order of words and their variants, nulling
%           roots in the process (because we don't know them or need them).
%           To make sure that a variant generates itself, records of the form
%           <variant>|<vcat>|<variant>|<vcat>|0||[]
%           are added, and records of the form
%           <variant>|<vcat>|<variant>|none|0||[]
%           are suppressesd.
%           Warning: <flipped history> is an idiosyncratic flip that does not
%           reverse the history as you might expect. It flips individual
%           elements in the history but does not reverse the order except that
%           an initial 'i' is moved to the end of the history. This is
%           because <history> is thought to be backwards by some people anyway.


:- module(flip_variants,[
	go/0
    ]).

:- use_module(skr_db(db_access),[
	default_release/1
    ]).

:- use_module(skr_lib(addportray),[
	add_portray/1
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/3
    ]).

:- use_module(skr_lib(nls_io),[
	fget_line/2
    ]).

:- use_module(skr_lib(nls_strings),[
	portray_strings_double_quoted/1,
	split_string_completely/3
    ]).

:- use_module(skr_lib(nls_system), [
	get_control_options_for_modules/2,
	toggle_control_options/1,
	set_control_values/2,
	display_control_options_for_modules/2,
	display_current_control_options/2,
	control_option/1,
	control_value/2,
	parse_command_line/1,
	interpret_options/4,
	interpret_args/4,
	get_from_iargs/4
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	compute_unique_filename/3
    ]).


/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for explore_intractables.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    format('~nFlip Variants~n',[]),
    (initialize_flip_variants(Options,Args,InterpretedArgs) ->
        (flip_variants(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_flip_variants(+Options, +Args, -InterpretedArgs)

initialize_flip_variants/3 interprets command line options and arguments
(opening files as necessary), and sets and displays the Flip Variants
control options discovered.  It returns InterpretedArgs for later use
(e.g., the stream associated with a file).  */

initialize_flip_variants(Options,Args,InterpretedArgs) :-
    get_control_options_for_modules([flip_variants],AllOptions),
    interpret_options(Options,AllOptions,flip_variants,IOptions),
    \+memberchk(iopt(help,_),IOptions),
    ArgSpec=[aspec(infile,mandatory,file,read,no_default,
                          'Input file of previously variants.'),
             aspec(outfile,mandatory,file,write,no_default,
                           'Output file of flipped variants.')
            ],
    interpret_args(IOptions,ArgSpec,Args,InterpretedArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,InterpretedArgs),
    default_release(Release),
    display_current_control_options(flip_variants, Release),
    !.


/* usage

usage/0 displays flip_variants usage.  */

usage :-
    format('~nUsage: flip_variants [<options>] <infile> <outfile>~n~n',[]),
    format('  <infile> should have lines of the form~n',[]),
    format('    <word>|<wcat>|<variant>|<vcat>|<var level>|<history>|<roots> and~n',[]),
    format('  <outfile> will contain corresponding lines of the form~n',[]),
    format('    <variant>|<vcat>|<word>|<wcat>|<var level>|<flipped history>|[].~n~n',[]),
    display_control_options_for_modules(flip_variants,[]).




/* flip_variants(+InterpretedArgs)

flip_variants/1 controls all flip_variants processing.  */

flip_variants(InterpretedArgs) :-
    get_from_iargs(infile,name,InterpretedArgs,InputFile),
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    get_from_iargs(outfile,name,InterpretedArgs,OutputFile),
    get_from_iargs(outfile,stream,InterpretedArgs,OutputStream),
    format('Processing ~a --> ~a.~n',[InputFile,OutputFile]),
    process_input(InputStream,OutputStream),
    close(OutputStream),
    close(InputStream),
    format('~nFinished.~n~n',[]),
    !.


/* process_input(+InputStream, +OutputStream)

process_input/2 reads lines from InputStream and writes flipped lines to
OutputStream.  */

process_input(InputStream,OutputStream) :-
    repeat,
    maybe_atom_gc(2, _, _),
    (fget_line(InputStream,Line) ->
        process_line(OutputStream,Line),
        fail
    ;   true
    ),
    !.


process_line(OutputStream,Line) :-
    split_string_completely(Line,"|",All),
    All=[Word,WCat,Variant,VCat,VarLevel,History,_Roots],
    flip_elements(History,FlippedHistory0),
    move_inflection_element(FlippedHistory0,FlippedHistory),
	( History \= FlippedHistory ->
	  format(user_output, '~s|~s~n', [History,FlippedHistory])
	; true
	),
    ((Variant==Word, WCat=="none", VarLevel=="0") ->
        true % suppress <variant>|<vcat>|<variant>|none|0||[] records
    ;   format(OutputStream,'~s|~s|~s|~s|~s|~s|[]~n',[Variant,VCat,Word,WCat,
						      VarLevel,FlippedHistory])
    ),
    format(OutputStream,'~s|~s|~s|~s|0||[]~n',[Variant,VCat,Variant,VCat]),
    !.
process_line(_OutputStream,Line) :-
    format('~NFatal error: Bad input ~s~n',[Line]),
    halt.

flip_elements([],[]) :-
    !.
flip_elements([0'i|Rest],[0'i|FlippedRest]) :-
    !,
    flip_elements(Rest,FlippedRest).
flip_elements([0'd|Rest],[0'd|FlippedRest]) :-
    !,
    flip_elements(Rest,FlippedRest).
flip_elements([0's|Rest],[0's|FlippedRest]) :-
    !,
    flip_elements(Rest,FlippedRest).
flip_elements([0'p|Rest],[0'p|FlippedRest]) :-
    !,
    flip_elements(Rest,FlippedRest).
flip_elements([0'a|Rest],[0'e|FlippedRest]) :-
    !,
    flip_elements(Rest,FlippedRest).
flip_elements([0'e|Rest],[0'a|FlippedRest]) :-
    !,
    flip_elements(Rest,FlippedRest).
flip_elements([Char|Rest],FlippedRest) :-
    format('Unknown history element ~c; ignored.~n',[Char]),
    flip_elements(Rest,FlippedRest).

move_inflection_element([0'i|Rest],Result) :-
    !,
    append(Rest,[0'i],Result).
move_inflection_element(History,History).

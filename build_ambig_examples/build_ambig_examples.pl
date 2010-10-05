% File:     build_ambig_examples.pl
% Module:   Build Ambiguity Examples
% Author:   Lan
% Purpose:  Creates files of <CUI>|<SUI> lines representing examples of
%           ambiguity.


:- module(build_ambig_examples,[
	go/0
    ]).

:- use_module(mm_tools_lib(factbase),[
	erase_all_facts/2,
	erase_fact/3,
	put_fact/3
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	compute_unique_filename/3,
	fget_non_null_line/2
    ]).

:- use_module(skr_lib(addportray),[
	add_portray/1
    ]).

:- use_module(skr_lib(ctypes),[
	is_alpha/1
    ]).

:- use_module(skr_db(db_access),[
        default_full_year/1
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_strings),[
	portray_strings_double_quoted/1,
	split_string_completely/3
    ]).

:- use_module(skr_lib(nls_system), [
	get_control_options_for_modules/2,
	reset_control_options/1,
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

:- use_module(skr_lib(sicstus_utils),[
	lower/2
    ]).

:- use_module(library(lists),[
	append/2,
	rev/2
    ]).

:- dynamic casenum/1.



/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for build_ambig_examples.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    reset_control_options(build_ambig_examples),
    format('~nBuild Ambiguity Examples~n',[]),
    (initialize_build_ambig_examples(Options,Args,InterpretedArgs) ->
        (build_ambig_examples(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_build_ambig_examples(+Options, +Args, -InterpretedArgs)

initialize_build_ambig_examples/3 interprets command line options and arguments
(opening files as necessary), and sets and displays the Build Ambiguity Examples
control options discovered.  It returns InterpretedArgs for later use
(e.g., the stream associated with a file).  */

initialize_build_ambig_examples(Options,Args,InterpretedArgs) :-
    get_control_options_for_modules([build_ambig_examples],AllOptions),
    interpret_options(Options,AllOptions,build_ambig_examples,IOptions),
    \+memberchk(iopt(help,_),IOptions),
    ArgSpec=[aspec(infile,mandatory,file,read,no_default,
                          'A sorted prefix of supp.ambiguity_cases.')
            ],
    interpret_args(IOptions,ArgSpec,Args,InterpretedArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,InterpretedArgs),
    default_full_year(FullYear),
    display_current_control_options(build_ambig_examples, FullYear),
    !.


/* usage

usage/0 displays build_ambig_examples usage.  */

usage :-
    format('~nUsage: build_ambig_examples [<options>] <infile>~n~n',[]),
    format('  <infile> is a sorted prefix of supp.ambiguity_cases.~n~n',[]),
    display_control_options_for_modules(build_ambig_examples,[]).


/* build_ambig_examples(+InterpretedArgs)

build_ambig_examples/1 controls all build_ambig_examples processing.  */

build_ambig_examples(InterpretedArgs) :-
    get_from_iargs(infile,name,InterpretedArgs,InputFile),
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    format('Processing ~a.~n',[InputFile]),
    process_input(InputStream),
    close(InputStream),
    format('~nFinished.~n~n',[]),
    !.


/* process_input(+InputStream)

process_input/1 reads lines from InputStream and writes <CUI>|<SUI> lines
representing cases of ambiguity to files with names like
exnnn.<short name>.0. */

process_input(InputStream) :-
    retractall(casenum(_)),
    assert(casenum(1)),
    erase_all_facts(saved,info),
    fget_non_null_line(InputStream,Line0),
    parse_line(Line0,ID0,CUI0,_LUI0,SUI0,Name0),
    put_fact(saved,info,[ID0,[[CUI0,SUI0,Name0]]]),
    repeat,
    maybe_atom_gc(_,_),
    (erase_fact(saved,info,[ID,CSNs]) ->
        process_case(InputStream,ID,CSNs),
        fail
    ;   true
    ),
    !.


/* parse_line(+Line, -ID, -CUI, -LUI, -SUI, -Name)

parse_line/6 extracts ID, CUI, LUI, SUI and Name from Line. */

parse_line(Line,ID,CUI,LUI,SUI,Name) :-
    split_string_completely(Line,"|",[ID,_Degree,CUI,LUI,SUI,Name]),
    !.
parse_line(Line,_,_,_,_,_) :-
    format('~NFatal error: Bad input ~s~n',[Line]),
    halt.


/* process_case(+InputStream, +ID, +CSNs)
   
process_case/3 accumulates CSNs (lists consisting of a CUI, SUI and Name) with
the same ID (supp.ambiguity case number) by reading Line from InputStream.
When a new ID is encountered, the accumulated information is written to a
file with name of the form exnnn.<short name>.0. */

process_case(InputStream,ID0,CSNs0) :-
    repeat,
    (fget_non_null_line(InputStream,Line) ->
        parse_line(Line,ID,CUI,_LUI,SUI,Name),
        (ID==ID0 ->
            process_case(InputStream,ID,[[CUI,SUI,Name]|CSNs0])
        ;   write_case(CSNs0,ID0),
            put_fact(saved,info,[ID,[[CUI,SUI,Name]]])
        )
    ;   write_case(CSNs0,ID0)
    ),
    !.


/* write_case(+CSNs, +ID)

write_case/2 writes <CUI>:<SUI> lines to a file with name exnnn.<short name>.0.
*/

write_case(CSNs,_ID) :-
    get_casenum_string(NString),
    CSNs=[[_,_,Name]|_],
    compute_short_name(Name,ShortName),
    append(["ex",NString,".",ShortName,".0"],FileNameString),
    atom_codes(FileName,FileNameString),
    open(FileName,write,OutputStream),
    write_lines(CSNs,OutputStream),
    close(OutputStream),
    !.

get_casenum_string(NString) :-
    retract(casenum(N)),
    M is N + 1,
    assert(casenum(M)),
    number_codes(N,NString0),
    append("000",NString0,NString1),
    append(_,NString,NString1),
    length(NString,3),
    !.

compute_short_name(Name,ShortName) :-
    split_string_completely(Name," ",[ShortName0|_]),
    lower(ShortName0,ShortName1),
    filter_to_alpha(ShortName1,ShortName).

filter_to_alpha([],[]) :-
    !.
filter_to_alpha([First|Rest],[First|FilteredRest]) :-
    is_alpha(First),
    !,
    filter_to_alpha(Rest,FilteredRest).
filter_to_alpha([_First|Rest],FilteredRest) :-
    !,
    filter_to_alpha(Rest,FilteredRest).

write_lines([],_) :-
    !.
write_lines([[CUI,SUI,_Name]|Rest],OutputStream) :-
    format(OutputStream,'~s:~s~n',[CUI,SUI]),
    write_lines(Rest,OutputStream).

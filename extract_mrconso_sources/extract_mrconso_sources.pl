% File:     extract_mrconso_sources.pl
% Module:   Extract Mrconso Sources
% Author:   Lan
% Purpose:  Extracts source information from mrconso.eng producing records of
%           the form
%             <CUI>|<SUI>|<I>|<STR>|<SAB>|<TTY>
%           where <I> is the ith record for <CUI>.
%           -f (--first_of_each_source_only) is a default option.


:- module(extract_mrconso_sources,[
	go/0
    ]).

:- use_module(mm_tools_lib(factbase),[
	erase_all_facts/2,
	erase_fact/3,
	put_fact/3
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	compute_unique_filename/3,
	fget_non_null_line/2,
	parse_record/3,
	normalize_meta_string/3
    ]).

:- use_module(skr_lib(addportray),[
	add_portray/1
    ]).

:- use_module(skr_db(db_access),[
        default_release/1
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_strings),[
	portray_strings_double_quoted/1
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

:- use_module(library(lists),[
	append/2,
	last/2,
	rev/2
    ]).



/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for extract_mrconso_sources.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    reset_control_options(extract_mrconso_sources),
    format('~nExtract mrconso Sources~n',[]),
    (initialize_extract_mrconso_sources(Options,Args,InterpretedArgs) ->
        (extract_mrconso_sources(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_extract_mrconso_sources(+Options, +Args, -InterpretedArgs)

initialize_extract_mrconso_sources/3 interprets command line options and
arguments (opening files as necessary), and sets and displays the Extract
Mrconso Sources control options discovered.  It returns InterpretedArgs for
later use (e.g., the stream associated with a file).  */

initialize_extract_mrconso_sources(Options,Args,InterpretedArgs) :-
    get_control_options_for_modules([extract_mrconso_sources],AllOptions),
    interpret_options(Options,AllOptions,extract_mrconso_sources,IOptions),
    \+memberchk(iopt(help,_),IOptions),
    ArgSpec=[aspec(infile,mandatory,file,read,no_default,
                          'Input file similar to mrconso.eng.'),
             aspec(outfile,mandatory,file,write,no_default,
                           'Output file')
            ],
    interpret_args(IOptions,ArgSpec,Args,InterpretedArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,InterpretedArgs),
    default_release(Release),
    display_current_control_options(extract_mrconso_sources, Release),
    !.


/* usage

usage/0 displays extract_mrconso_sources usage.  */

usage :-
    format('~nUsage: extract_mrconso_sources [<options>] <infile> <outfile>~n~n',[]),
    format('  <infile> should normally be mrconso.eng or the like, and~n',[]),
    format('  <outfile> consists of records of the form~n',[]),
    format('            CUI|SUI|I|STR|SAB|TTY.~n~n',[]),
    display_control_options_for_modules(extract_mrconso_sources,[]).

/* extract_mrconso_sources(+InterpretedArgs)

extract_mrconso_sources/1 controls all extract_mrconso_sources processing.  */

extract_mrconso_sources(InterpretedArgs) :-
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

process_input/2 reads lines from InputStream and writes source information to
OutputStream.  */

process_input(InputStream,OutputStream) :-
    erase_all_facts(saved,info),
    fget_non_null_line(InputStream,Line0),
    parse_line(Line0,CUI0,SUI0,STR0,SAB0,TTY0),
    put_fact(saved,info,[CUI0,[cuiinfo(CUI0,SUI0,Line0,STR0,SAB0,TTY0)]]),
    repeat,
    maybe_atom_gc(_,_),
    (erase_fact(saved,info,[CUI,CUIInfoLines0]) ->
        process_cui(InputStream,OutputStream,CUI,CUIInfoLines0),
        fail
    ;   true
    ),
    !.


/* parse_line(+Line, -CUI, -SUI, -STR, -SAB, -TTY)

parse_line/6 extracts CUI, ... from Line.  */

parse_line(Line,CUI,SUI,STR,SAB,TTY) :-
    parse_record(Line,"|",[CLS,_N,_TS,_STT,STR,SAB,TTY,_SCD]),
    parse_record(CLS,":",[CUI,_LUI,SUI]),
    !.
parse_line(Line,_,_,_,_,_) :-
    format('~NFatal error: Bad input ~s~n',[Line]),
    halt.


/* process_cui(+InputStream, +OutputStream, +CUI, +CUIInfoLines)
   
process_cui/4 accumulates CUIInfoLines (cuiinfo/6 terms) by
reading Line from InputStream, extracting some fields with the same
CUI as the input.  When a new CUI is encountered, the accumulated
information is written to OutputStream. The information from
the current line is saved for further processing.  */

process_cui(InputStream,OutputStream,CUI0,CUIInfoLines0) :-
    repeat,
    (fget_non_null_line(InputStream,Line) ->
        parse_line(Line,CUI,SUI,STR,SAB,TTY),
        (CUI==CUI0 ->
            process_cui(InputStream,OutputStream,CUI0,
                            [cuiinfo(CUI,SUI,Line,STR,SAB,TTY)|CUIInfoLines0])
        ;   write_cuiinfo(OutputStream,CUIInfoLines0),
            put_fact(saved,info,[CUI,[cuiinfo(CUI,SUI,Line,STR,SAB,TTY)]])
        )
    ;   write_cuiinfo(OutputStream,CUIInfoLines0)
    ),
    !.


/* write_cuiinfo(+OutputStream, +CUIInfoLines)
   write_cuiinfo(+CUIInfoLines, +I, +OutputStream)
   write_cuiinfo(+CUIInfoLines, +I, +Sources, +OutputStream)

write_cuiinfo/2 writes the information in the cuiinfo/6 terms in CUIInfoLines.
write_cuiinfo/3 and write_cuiinfo/4 are auxiliaries which keep track of the
ith record for a given CUI and, in the case of /4, the sources that have already
been written. */

write_cuiinfo(OutputStream,CUIInfoLines0) :-
    rev(CUIInfoLines0,CUIInfoLines),
    (control_option(first_of_each_source_only) ->
	write_cuiinfo(CUIInfoLines,1,[],OutputStream)
    ;   write_cuiinfo(CUIInfoLines,1,OutputStream)
    ),
    !.

write_cuiinfo([],_,_) :-
    !.
write_cuiinfo([cuiinfo(CUI,SUI,_Line,STR,SAB,TTY)|Rest],I,OutputStream) :-
    format(OutputStream,'~s|~s|~d|~s|~s|~s~n',[CUI,SUI,I,STR,SAB,TTY]),
    NewI is I + 1,
    write_cuiinfo(Rest,NewI,OutputStream).

write_cuiinfo([],_,_,_) :-
    !.
write_cuiinfo([cuiinfo(_CUI,_SUI,_Line,_STR,SAB,_TTY)|Rest],I,Sources,
	      OutputStream) :-
    memberchk(SAB,Sources),
    !,
    write_cuiinfo(Rest,I,Sources,OutputStream).
write_cuiinfo([cuiinfo(CUI,SUI,_Line,STR,SAB,TTY)|Rest],I,Sources,
	      OutputStream) :-
    !,
    format(OutputStream,'~s|~s|~d|~s|~s|~s~n',[CUI,SUI,I,STR,SAB,TTY]),
    NewI is I + 1,
    write_cuiinfo(Rest,NewI,[SAB|Sources],OutputStream).

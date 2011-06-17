% File:     glean_ambig.pl
% Module:   Glean Ambiguity
% Author:   Lan
% Purpose:  Glean the ambiguity information from mrcon.eng.1, a version of
%           mrcon.eng.0 with a lowercased string appended to the end of each
%           record. Two versions of ambiguity case files are created, one
%           with suppressibles and one without.


:- module(glean_ambig,[
	go/0
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	fget_non_null_line/2,
	compute_unique_filename/3
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
	portray_strings_double_quoted/1,
	split_string_completely/3
    ]).

:- use_module(skr_lib(nls_system),[
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
        ttyflush/0
    ]).

:- use_module(library(lists),[
	rev/2
    ]).

/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for glean_ambig.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    reset_control_options(glean_ambig),
    format('~nGlean Ambiguity~n',[]),
    (initialize_glean_ambig(Options,Args,InterpretedArgs) ->
        (glean_ambig(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_glean_ambig(+Options, +Args, -InterpretedArgs)

initialize_glean_ambig/3 interprets command line options and arguments
(opening files as necessary), and sets and displays the Glean Ambiguity
control options discovered.  It returns InterpretedArgs for later use
(e.g., the stream associated with a file).  */

initialize_glean_ambig(Options,Args,InterpretedArgs) :-
    get_control_options_for_modules([glean_ambig],AllOptions),
    interpret_options(Options,AllOptions,glean_ambig,IOptions),
    \+memberchk(iopt(help,_),IOptions),
    ArgSpec=[aspec(infile,mandatory,file,read,no_default,
                          'Input file, similar to mrcon.eng.0 but with lc strings.'),
             aspec(outfile,mandatory,file,write,no_default,
                           'Output file of ambiguity cases.'),
             aspec(suppoutfile,mandatory,file,write,no_default,
                           'Output file of ambiguity cases without suppressibles.')
            ],
    interpret_args(IOptions,ArgSpec,Args,InterpretedArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,InterpretedArgs),
    default_release(Release),
    display_current_control_options(glean_ambig, Release),
    !.


/* usage

usage/0 displays glean_ambig usage.  */

usage :-
    format('~nUsage: glean_ambig [<options>] <infile> <outfile> <suppoutfile>~n~n',[]),
    format('  <infile> is similar to mrcon.eng.0 but with lc strings appended,~n',[]),
    format('  <outfile> will contain ambiguity cases, and~n',[]),
    format('  <outfile> will contsin ambiguity_cases without suppressibles.~n~n',[]),
    display_control_options_for_modules(glean_ambig,[]).


/* glean_ambig(+InterpretedArgs)

glean_ambig/1 controls all glean_ambig processing.  */

glean_ambig(InterpretedArgs) :-
    get_from_iargs(infile,name,InterpretedArgs,InputFile),
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    get_from_iargs(outfile,name,InterpretedArgs,OutputFile),
    get_from_iargs(outfile,stream,InterpretedArgs,OutputStream),
    get_from_iargs(suppoutfile,name,InterpretedArgs,SuppOutputFile),
    get_from_iargs(suppoutfile,stream,InterpretedArgs,SuppOutputStream),
    format('Processing ~a --> ~a and ~a.~n',
	   [InputFile,OutputFile,SuppOutputFile]),
    process_input(InputStream,OutputStream,SuppOutputStream),
    close(SuppOutputStream),
    close(OutputStream),
    close(InputStream),
    format('~nFinished.~n~n',[]),
    !.


/* process_input(+InputStream, +OutputStream, +SuppOutputStream)

process_input/3 reads lines from InputStream and writes to OutputStream and
SuppOutputStream. */

process_input(InputStream, OutputStream, SuppOutputStream) :-
	fget_non_null_line(InputStream, Line0),
	parse_line(Line0, CUI0, TS0, LUI0, SUI0, LCString0),
	maybe_atom_gc(_,_),
	SuppCaseNum is 0,
	UnSuppCaseNum is 0,
	process_case(InputStream, OutputStream, SuppOutputStream,
		     SuppCaseNum, UnSuppCaseNum,
		     LCString0, [CUI0-TS0-LUI0-SUI0]).


/* parse_line(+Line, -CUI, -TS, -LUI, -SUI, -LCString)

parse_line/6 extracts CUI, TS, LUI, SUI and LCString from Line. */

parse_line(Line, CUIAtom, TSAtom, LUIAtom, SUIAtom, LCAtom) :-
	( split_string_completely(Line, "|",
				  [CUIString,_,TSString,LUIString,_,SUIString,_,_,LCString]) ->
	  atom_codes(CUIAtom, CUIString),
	  atom_codes(TSAtom,  TSString),
	  atom_codes(LUIAtom, LUIString),
	  atom_codes(SUIAtom, SUIString),
	  atom_codes(LCAtom,  LCString)
	; format(user_output, '~NFatal error: Bad input ~s~n', [Line]),
	  ttyflush,
	  halt
	).

/* process_case(+InputStream, +OutputStream, +SuppOutputStream,
   	        +SuppCaseNum, +UnSuppCaseNum,
		+LCString, +CTLSList)
   
process_case/5 accumulates CTLSList (a list of terms of the form CUI-TS-LUI-SUI)
with the same LCString by reading Line from InputStream.
When a new LCString is encountered, the accumulated information is written
to OutputStream and SuppOutputStream. */

process_case(InputStream, OutputStream, SuppOutputStream,
	     SuppCaseNumIn, UnSuppCaseNumIn, LCString0, CTLSList) :-
	maybe_atom_gc(_,_),
	( fget_non_null_line(InputStream, Line) ->
	  parse_line(Line, CUI, TS, LUI, SUI, LCString),
	  maybe_write_results(LCString, LCString0,
			      InputStream, OutputStream, SuppOutputStream,
			      SuppCaseNumIn, UnSuppCaseNumIn,
			      CUI, TS, LUI, SUI, CTLSList)
	  % finish up the last batch
	; write_results(CTLSList, LCString0,
			OutputStream,    SuppOutputStream,
			SuppCaseNumIn,   UnSuppCaseNumIn,
			_SuppCaseNumOut, _UnSuppCaseNumOut)
	).

maybe_write_results(LCString, LCString0,
		    InputStream, OutputStream, SuppOutputStream,
		    SuppCaseNumIn, UnSuppCaseNumIn,
		    CUI, TS, LUI, SUI, CTLSList) :-
	  ( LCString == LCString0 ->
	    process_case(InputStream, OutputStream, SuppOutputStream,
			 SuppCaseNumIn, UnSuppCaseNumIn,
			 LCString, [CUI-TS-LUI-SUI|CTLSList])
	  ; write_results(CTLSList, LCString0,
			  OutputStream, SuppOutputStream,
			  SuppCaseNumIn, UnSuppCaseNumIn,
			  SuppCaseNumOut, UnSuppCaseNumOut),
	    process_case(InputStream, OutputStream, SuppOutputStream,
			 SuppCaseNumOut, UnSuppCaseNumOut,
			 LCString, [CUI-TS-LUI-SUI])
	  ).

/* write_results(+CTLS, +LCString,
   		 +OutputStream, +SuppOutputStream,
   		 +SuppCaseNumIn, +UnSuppCaseNumIn,
   		 -SuppCaseNumOut, -UnSuppCaseNumOut)


write_results/4 writes ambiguity cases defined by LCString and CTLSs
(a list of quadruples CUI-TS-LUI-SUI) to OutputStream and SuppOutputStream.
Cases including suppressibles are written to OutputStream, and those without
suppressibles are written to SuppOutputStream. */

write_results(CTLSList0, LCString, OutputStream, SuppOutputStream,
	      SuppCaseNumIn,  UnSuppCaseNumIn,
	      SuppCaseNumOut, UnSuppCaseNumOut) :-
	rev(CTLSList0, CTLSList),
	maybe_write_case(CTLSList, LCString, OutputStream, UnSuppCaseNumIn, UnSuppCaseNumOut), 
	filter_ctlss(CTLSList, SuppCTLSList),
	maybe_write_case(SuppCTLSList, LCString, SuppOutputStream, SuppCaseNumIn, SuppCaseNumOut).

maybe_write_case(CTLSList, LCString, OutputStream, UnSuppCaseNumIn, UnSuppCaseNumOut) :-
	compute_degree_of_ambiguity(CTLSList, N),
	( N > 1 ->
	  UnSuppCaseNumOut is UnSuppCaseNumIn + 1,
	  write_case(CTLSList, UnSuppCaseNumOut, N, LCString, OutputStream)
	; UnSuppCaseNumOut is UnSuppCaseNumIn
	).

compute_degree_of_ambiguity(CTLSs, N) :-
	extract_cuis(CTLSs, CUIs0),
	sort(CUIs0, CUIs),
	length(CUIs, N).

extract_cuis([], []).
extract_cuis([CUI-_TS-_LUI-_SUI|Rest], [CUI|ExtractedRest]) :-
	extract_cuis(Rest, ExtractedRest).

filter_ctlss([], []).
filter_ctlss([First|Rest], Filtered) :-
	First = _CUI-TS-_LUI-_SUI,
	( suppressible_term_status(TS) ->
	  Filtered = FilteredRest,
	  filter_ctlss(Rest, FilteredRest)
	; Filtered = [First|FilteredRest],
	  filter_ctlss(Rest, FilteredRest)
	).

suppressible_term_status(p).
suppressible_term_status(s).

write_case([], _, _, _, _).
write_case([CTLS|Rest], CN, N, LCString, Stream) :-
	CTLS = CUI-_TS-LUI-SUI,
	format(Stream, '~d|~d|~a|~a|~a|~a~n', [CN,N,CUI,LUI,SUI,LCString]),
	write_case(Rest, CN, N, LCString, Stream).

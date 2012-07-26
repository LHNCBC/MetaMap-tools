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
	announce_lines/4,
	fget_non_null_line/2,
	get_progress_bar_interval/1,
	get_total_lines/1,
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
	atom_codes_list/2,
	portray_strings_double_quoted/1,
	split_string_completely/3
    ]).

:- use_module(skr_lib(nls_system),[
	control_option/1,
	control_value/2,
	display_control_options_for_modules/2,
	display_current_control_options/2,
	get_control_options_for_modules/2,
	get_from_iargs/4,
	interpret_args/4,
	interpret_options/4,
	parse_command_line/1,
	reset_control_options/1,
	set_control_values/2,
	toggle_control_options/1
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

go :- go(halt).

go(HaltOption) :-
	parse_command_line(CLTerm),
	go(HaltOption, CLTerm).

go(HaltOption, command_line(Options,Args)) :-
	add_portray(portray_strings_double_quoted),
	reset_control_options(glean_ambig),
	format('~nGlean Ambiguity~n',[]),
	( initialize_glean_ambig(Options,Args,InterpretedArgs) ->
	  glean_ambig(InterpretedArgs)
	; usage
	),
	( HaltOption == halt ->
	  halt
	; true
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
    compile_mrrank_file,
    default_release(Release),
    display_current_control_options(glean_ambig, Release),
    !.

compile_mrrank_file :-
	( control_value(mrrank_file, FileName) ->
	  compile(FileName)
	; format(user_output, 'Must specify mrrank file with -R option.\n', []),
	  flush_output(user_output),
	  abort
	).

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
	get_from_iargs(infile,      name,   InterpretedArgs, InputFile),
	get_from_iargs(infile,      stream, InterpretedArgs, InputStream),
	get_from_iargs(outfile,     name,   InterpretedArgs, OutputFile),
	get_from_iargs(outfile,     stream, InterpretedArgs, OutputStream),
	get_from_iargs(suppoutfile, name,   InterpretedArgs, SuppOutputFile),
	get_from_iargs(suppoutfile, stream, InterpretedArgs, SuppOutputStream),
	get_progress_bar_interval(Interval),
	get_total_lines(TotalLines),
	format('Processing ~a --> ~a and ~a.~n', [InputFile,OutputFile,SuppOutputFile]),
	process_input(InputStream, OutputStream, SuppOutputStream, InputFile, Interval, TotalLines),
	close(SuppOutputStream),
	close(OutputStream),
	close(InputStream),
	format('~nFinished.~n~n',[]),
	!.


/* process_input(InputStream, OutputStream, SuppOutputStream, InputFile, Interval, TotalLines),
   
process_input/6 reads lines from InputStream and writes to OutputStream and SuppOutputStream. */

process_input(InputStream, OutputStream, SuppOutputStream, InputFile, Interval, TotalLines) :-
	fget_non_null_line(InputStream, Line0),
	parse_line(Line0, CUI0, TS0, LUI0, SUI0, SAB0, TTY0, SUPPRESS0, LCString0),
	maybe_atom_gc(_,_),
	SuppCaseNum is 0,
	UnSuppCaseNum is 0,
	NumLines is 1,
	process_case(InputStream, OutputStream, SuppOutputStream,
		     InputFile, NumLines, Interval, TotalLines,
		     SuppCaseNum, UnSuppCaseNum,
		     LCString0, [CUI0-TS0-LUI0-SUI0-SAB0-TTY0-SUPPRESS0]).


/* parse_line(+Line, -CUI, -TS, -LUI, -SUI, -SABAtom, -TTYAtom, -SUPPRESS, -LCString)

parse_line/9 extracts CUI, TS, LUI, SUI, SUPPRESS and LCString from Line. */

parse_line(Line, CUIAtom, TSAtom, LUIAtom, SUIAtom, SABAtom, TTYAtom, SUPPRESSAtom, LCAtom) :-
	( split_string_completely(Line, "|",
				  % [CUIString,_,TSString,LUIString,_,SUIString,_,_,LCString]) ->
				  [CUIString,_LAT,TSString,LUIString,_STT,SUIString,
				   _ISPREF,_AUI,_SAUI,_SCUI,_SDUI,SABString,TTYString,_CODE,
				   _STR,_SRL,SUPPRESSString,_CVF,LCString]) ->
	  atom_codes_list([CUIAtom,TSAtom,LUIAtom,SUIAtom,
			   SABAtom, TTYAtom,SUPPRESSAtom,LCAtom],
			  [CUIString,TSString,LUIString,SUIString,
			   SABString,TTYString,SUPPRESSString,LCString])
	; format(user_output, '~NFatal error: Bad input ~s~n', [Line]),
	  ttyflush,
	  halt
	).

/* process_case(+InputStream, +OutputStream, +SuppOutputStream,
		+InputFile, +NumLines, +Interval, +TotalLines,
   	        +SuppCaseNum, +UnSuppCaseNum,
		+LCString, +FieldList)
   
process_case/5 accumulates FieldsList (a list of terms of the form CUI-TS-LUI-SUI)
with the same LCString by reading Line from InputStream.
When a new LCString is encountered, the accumulated information is written
to OutputStream and SuppOutputStream. */

process_case(InputStream, OutputStream, SuppOutputStream,
	     InputFile, NumLinesIn, Interval, TotalLines,
	     SuppCaseNumIn, UnSuppCaseNumIn, LCString0, FieldsList) :-
	maybe_atom_gc(_,_),
	( fget_non_null_line(InputStream, Line) ->
	  NumLinesNext is NumLinesIn + 1,
	  announce_lines(NumLinesNext, Interval, TotalLines, InputFile),
	  parse_line(Line, CUI, TS, LUI, SUI, SAB, TTY, SUPPRESS, LCString),
	  maybe_write_results(LCString, LCString0,
			      InputStream, OutputStream, SuppOutputStream,
			      InputFile, NumLinesNext, Interval, TotalLines,
			      SuppCaseNumIn, UnSuppCaseNumIn,
			      CUI, TS, LUI, SUI, SAB, TTY, SUPPRESS, FieldsList)
	  % finish up the last batch
	; write_results(FieldsList, LCString0,
			OutputStream,    SuppOutputStream,
			SuppCaseNumIn,   UnSuppCaseNumIn,
			_SuppCaseNumOut, _UnSuppCaseNumOut)
	).

maybe_write_results(LCString, LCString0,
		    InputStream, OutputStream, SuppOutputStream,
		    InputFile, NumLines, Interval, TotalLines,
		    SuppCaseNumIn, UnSuppCaseNumIn,
		    CUI, TS, LUI, SUI, SAB, TTY, SUPPRESS, FieldsList) :-
	  FieldTerm = CUI-TS-LUI-SUI-SAB-TTY-SUPPRESS,
	  ( LCString == LCString0 ->
	    process_case(InputStream, OutputStream, SuppOutputStream,
			 InputFile, NumLines, Interval, TotalLines,
			 SuppCaseNumIn, UnSuppCaseNumIn,
			 LCString, [FieldTerm|FieldsList])
	  ; write_results(FieldsList, LCString0,
			  OutputStream, SuppOutputStream,
			  SuppCaseNumIn, UnSuppCaseNumIn,
			  SuppCaseNumOut, UnSuppCaseNumOut),
	    process_case(InputStream, OutputStream, SuppOutputStream,
			 InputFile, NumLines, Interval, TotalLines,
			 SuppCaseNumOut, UnSuppCaseNumOut, 
			 LCString, [FieldTerm])
	  ).

/* write_results(+Fields, +LCString,
   		 +OutputStream, +SuppOutputStream,
   		 +SuppCaseNumIn, +UnSuppCaseNumIn,
   		 -SuppCaseNumOut, -UnSuppCaseNumOut)


write_results/4 writes ambiguity cases defined by LCString and Fieldss
(a list of quadruples CUI-TS-LUI-SUI) to OutputStream and SuppOutputStream.
Cases including suppressibles are written to OutputStream, and those without
suppressibles are written to SuppOutputStream. */

write_results(FieldsList0, LCString, OutputStream, SuppOutputStream,
	      SuppCaseNumIn,  UnSuppCaseNumIn,
	      SuppCaseNumOut, UnSuppCaseNumOut) :-
	rev(FieldsList0, FieldsList),
	maybe_write_case(FieldsList, LCString, OutputStream, UnSuppCaseNumIn, UnSuppCaseNumOut),
	filter_fields(FieldsList, SuppFieldsList),
	maybe_write_case(SuppFieldsList, LCString, SuppOutputStream, SuppCaseNumIn, SuppCaseNumOut).

maybe_write_case(FieldsList, LCString, OutputStream, CaseNumIn, CaseNumOut) :-
	compute_degree_of_ambiguity(FieldsList, DegreeOfAmbiguity),
	( DegreeOfAmbiguity > 1 ->
	  CaseNumOut is CaseNumIn + 1,
	  write_case(FieldsList, CaseNumOut, DegreeOfAmbiguity, LCString, OutputStream)
	; CaseNumOut is CaseNumIn
	).

compute_degree_of_ambiguity(FieldsList, DegreeOfAmbiguity) :-
	extract_cuis(FieldsList, CUIs0),
	sort(CUIs0, CUIs),
	length(CUIs, DegreeOfAmbiguity).

extract_cuis([], []).
extract_cuis([CUI-_TS-_LUI-_SUI-_SAB-_TTY-_SUPPRESS|Rest], [CUI|ExtractedRest]) :-
	extract_cuis(Rest, ExtractedRest).

filter_fields([], []).
filter_fields([First|Rest], Filtered) :-
	First = _CUI-_TS-_LUI-_SUI-SAB-TTY-SUPPRESS,
	( suppressible_atom(SUPPRESS, SAB, TTY) ->
	  Filtered = FilteredRest
	; Filtered = [First|FilteredRest]
	),
	filter_fields(Rest, FilteredRest).

suppressible_atom('Y', _SAB, _TTY).
suppressible_atom('E', _SAB, _TTY).
suppressible_atom('O', _SAB, _TTY).
suppressible_atom('N',  SAB,  TTY) :-
	mrrank(SAB, TTY, _MRRANK, 'Y').

write_case([], _, _, _, Stream) :- flush_output(Stream).
write_case([Fields|Rest], CaseNum, DegreeOfAmbiguity, LCString, Stream) :-
	Fields = CUI-_TS-LUI-SUI-_SAB-_TTY-_SUPPRESS,
	format(Stream, '~d|~d|~a|~a|~a|~a~n', [CaseNum,DegreeOfAmbiguity,CUI,LUI,SUI,LCString]),
	write_case(Rest, CaseNum, DegreeOfAmbiguity, LCString, Stream).

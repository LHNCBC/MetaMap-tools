
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

/* 
    This code is based on lcheckp.pl
*/

:- module(conv_lex, [
        go/0
    ]).

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/4
   ]).

:- use_module(skr_db(db_access), [
	default_release/1
   ]).

:- use_module(skr_lib(ctypes), [
	is_endfile/1,
	is_newline/1
   ]).

:- use_module(skr_lib(sicstus_utils), [
	interleave_string/3
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	display_current_control_options/2,
	get_control_options_for_modules/2,
	get_from_iargs/4,
	interpret_args/4,
	interpret_options/4,
	parse_command_line/1,
	pwd/1,
	set_control_values/2,
	toggle_control_options/1,
	update_command_line/5
   ]).

:- use_module(library(codesio), [
	write_to_codes/2
    ]).

:- use_module(library(lists), [
	append/2
    ]).

:- use_module(library(lists3), [
	substitute/4
    ]).


usage :- format(user_output, 'conv_lex ASCIILexicon PrologFile~n', []).

go :- go(halt).

go(HaltOption) :-
	parse_command_line(CLTerm),
	go(HaltOption, CLTerm).

go(HaltOption, command_line(Options,Args)) :-
	( initialize_conv_lex(Options, Args, InterpretedArgs) ->
	    ( conv_lex(InterpretedArgs)
	    ; true
	    )
	; usage
	),
	( HaltOption == halt ->
	  halt
	; true
	).


/* initialize_conv_lex(+Options, +Args, -InterpretedArgs)

initialize_conv_lex/3 interprets command line options and arguments (opening
files as necessary) and sets and displays the MM Variants control options
discovered.  */

initialize_conv_lex(Options, Args, IArgs) :-
	get_control_options_for_modules([conv_lex], AllOptions),
	interpret_options(Options, AllOptions, conv_lex, IOptions),
	\+ member(iopt(help,_), IOptions),
	ArgSpecs = [aspec(infile,mandatory,file,read,
			  no_default,
			  'Input file containing lexical entries'),
		    aspec(outfile,mandatory,file,write,
			  ['<infile>','.','out'],
			  'Output file')
		    ],
	interpret_args(IOptions, ArgSpecs, Args, IArgs),
	toggle_control_options(IOptions),
	set_control_values(IOptions,IArgs),
	default_release(Release),
	display_current_control_options(conv_lex, Release),
	!.


conv_lex(InterpretedArgs) :-
	get_from_iargs(infile, name, InterpretedArgs, InputFile),
	get_from_iargs(infile, stream, InterpretedArgs, InputStream),
	get_from_iargs(outfile, name, InterpretedArgs, OutputFile),
	get_from_iargs(outfile, stream, InterpretedArgs, OutputStream),
	format('~n~nBeginning to process ~a sending output to ~a.~n~n',
	       [InputFile,OutputFile]),
	current_input(SavedCurrentInput),
	set_input(InputStream),
	current_output(SavedCurrentOutput),
	set_output(OutputStream),
	pwd(PWD),
	process_all(0, PWD, InputFile, InputStream, OutputStream),
	( control_option(end_of_processing) ->
	  format(OutputStream,'<<< EOT >>>~n',[])
	; true
	),
	set_input(SavedCurrentInput),
	set_output(SavedCurrentOutput),
	close(OutputStream),
	close(InputStream),
	format('~nBatch processing is finished.~n',[]),
	!.
conv_lex(_InterpretedArgs).

process_all(NumLines, PWD, InputFile, InputStream, OutputStream) :-
	peek_code(InputStream, Code),
	( is_endfile(Code) ->
	  true
	; read_and_process_lexrec(InputStream, OutputStream),
	  NumLines1 is NumLines + 1,
	  maybe_announce_progress(NumLines1, PWD, InputFile),
	  process_all(NumLines1, PWD, InputFile, InputStream, OutputStream)
	).
	  
read_and_process_lexrec(InputStream, OutputStream) :-
	read_lex_record_lines(InputStream, RawLexicalRecordLines0),
	interleave_string(RawLexicalRecordLines0, [10], RawLexicalRecordLines),
	append(RawLexicalRecordLines, RawLexicalRecord),
	( fm_lexical_record(ParsedLexicalRecord, EUI, RawLexicalRecord, []) ->
	  % The string will be conveerted to a Prolog term via read_from_codes/2
	  % in get_lex_rec_for_EUI/3.
	  % format(user_output, '~w|~q.~n', [EUI,ParsedLexicalRecord]),
	  format(OutputStream, '~w|~q.~n', [EUI,ParsedLexicalRecord])
	; report_fm_failure(RawLexicalRecord),
	  abort
	).

maybe_announce_progress(NumLines, PWD, InputFile) :-
        ( NumLines rem 1000 =:= 0 ->
          format(user_output, 'Processed ~d records of ~w/~w.~n', [NumLines,PWD,InputFile]),
          flush_output(user_output)
        ; true
        ).

% The simple and obvious method, which is to simply read chars until a '}' is found,
% does not work because the lexical entry for "MAHMA-NONOate" contains a '}'!!
% {base=MAHMA-NONOate
% entry=E0505119
% 	cat=noun
% 	variants=uncount
% 	acronym_of=(Z)-1-{N-methyl-N-[6(N-methylammoniohexyl)amino]}

read_lex_record_lines(InputStream, [Line|RestLines]) :-
	read_line(InputStream, Line),
	( Line = [C],
	  is_eor(C) ->
 	  % The DCG used by fm_lexical_record assumes a trailing CR,
	  % so add an empty list so the last interleaving char (10) will be the trailing CR.
	  RestLines = [[]]
	; read_lex_record_lines(InputStream, RestLines)
	).

%%% read_lex_record(LexicalRecord) :-
%%% 	get_code(Code),
%%% 	( is_endfile(Code) ->
%%% 	  LexicalRecord = []
%%% 	; is_eor(Code) ->
%%% 	  % The DCG used by fm_lexical_record assumes a trailing CR 
%%% 	  get_code(CR),
%%% 	  LexicalRecord = [Code,CR]
%%% 	; LexicalRecord = [Code|RestCodes],
%%% 	  read_lex_record(RestCodes)
%%% 	).

is_eor(0'}).

get_EUI(RawLexicalRecord, EUI) :-
	append([_Prefix, "entries:[entry:[num:['", EUI0], RawLexicalRecord),
	append([EUI, "'", _Rest], EUI0).
	      

%%% reports a failure to convert the record
report_fm_failure(Chars) :-
	look_for_base(Chars, Base),
	!,
	format(user_error, 'ERROR: Cannot convert record for "~w":~n~s', [Base,Chars]).

%%% looks for base
look_for_base(Chars, Base) :-
	append("{base=", List, Chars),
	chars_to_newline(BL, List, _),
	!,
	atom_codes(Base, BL).

%%% chars_to_newline - returns all characters till a newline
chars_to_newline([C|R]) -->
	[C],
	{ \+ is_newline(C) },
	chars_to_newline(R).
chars_to_newline([]) --> [].

% File:     glean_mrcon.pl
% Module:   Glean Mrcon
% Author:   Lan
% Purpose:  Create the files words.gleaned, strings.gleaned and concepts.gleaned
%           files which are used to create the DB files for the Meta Word Index facility.

:- module(glean_mrconso, [
	go/0
    ]).

:- use_module(mm_tools_lib(mwi_utilities), [
	compute_unique_filename/3,
	parse_record/2,
	normalize_meta_string/3
    ]).

:- use_module(metamap(metamap_tokenization), [
	tokenize_text_mm/2
    ]).

:- use_module(skr_db(db_access), [
	default_release/1
    ]).

:- use_module(skr_lib(addportray), [
	add_portray/1
    ]).

:- use_module(skr_lib(nls_system), [
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

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	portray_strings_double_quoted/1,
	split_string_completely/3
    ]).

:- use_module(skr_lib(efficiency), [
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_io), [
	fget_line/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
    ]).

:- use_module(library(file_systems), [
	file_exists/2
    ]).

:- use_module(library(lists), [
	append/2,
	last/2
    ]).

:- use_module(library(system), [
	now/1
    ]).

% :- dynamic filter_info/2.
% :- dynamic current_cui/1.

/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for glean_mrconso.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    reset_control_options(glean_mrconso),
    add_portray(portray_strings_double_quoted),
    format('~nGlean mrcon~n', []),
    (initialize_glean_mrconso(Options,Args,InterpretedArgs) ->
        (glean_mrconso(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_glean_mrconso(+Options, +Args, -InterpretedArgs)

initialize_glean_mrconso/3 interprets command line options and arguments
(opening files as necessary), and sets and displays the Glean Mrcon
control options discovered.  It returns InterpretedArgs for later use
(e.g., the stream associated with a file).  */

initialize_glean_mrconso(Options, Args, InterpretedArgs) :-
	get_control_options_for_modules([glean_mrconso], AllOptions),
	interpret_options(Options, AllOptions, glean_mrconso, IOptions),
	\+ member(iopt(help,_), IOptions),
	ArgSpec=[aspec(infile,mandatory,file,read,no_default,
		       'Input file similar to mrcon.filtered'),
		 aspec(filterfile,mandatory,file,read,no_default,
		       'Filter file'),
		 aspec(wordoutfile,mandatory,file,write,no_default,
		       'Word output file'),
		 aspec(suioutfile,mandatory,file,write,no_default,
		       'SUI output file'),
		 aspec(cuioutfile,mandatory,file,write,no_default,
		       'CUI output file')

		],
	interpret_args(IOptions, ArgSpec, Args, InterpretedArgs),
	toggle_control_options(IOptions),
	set_control_values(IOptions, InterpretedArgs),
	default_release(Release),
	display_current_control_options(glean_mrconso, Release),
	!.


/* usage

usage/0 displays glean_mrconso usage.  */

usage :-
    format('~nUsage: glean_mrconso [<options>] <infile> <filterfile> <wordoutfile> <suioutfile> <cuioutfile>~n~n', []),
    format('  <infile> should normally be mrcon.filtered,~n', []),
    format('  <filterfile> contains <word1>|<word2> pairs corresponding to~n', []),
    format('    the first and last words of strings whose words will be~n', []),
    format('    filtered out of <wordoutfile>. WARNING: It has no effect~n', []),
    format('    on the other output files.~n', []),
    format('  <wordoutfile> consists of I|N|CanonicalWord|SUI|CUI records,~n', []),
    format('  <suioutfile> consists of SUI|String records,~n', []),
    format('  and <cuioutfile> consists of CUI|Concept records.~n', []),
    display_control_options_for_modules(glean_mrconso, []).




/* glean_mrconso(+InterpretedArgs)

glean_mrconso/1 controls all glean_mrconso processing.  */

glean_mrconso(InterpretedArgs) :-
	get_from_iargs(infile,      name,   InterpretedArgs, InputFile),
	get_from_iargs(infile,      stream, InterpretedArgs, InputStream),
	get_from_iargs(filterfile,  name,   InterpretedArgs, FilterFile),
	get_from_iargs(filterfile,  stream, InterpretedArgs, FilterStream),
	get_from_iargs(wordoutfile, name,   InterpretedArgs, WordOutputFile),
	get_from_iargs(wordoutfile, stream, InterpretedArgs, WordOutputStream),
	get_from_iargs(suioutfile,  name,   InterpretedArgs, SUIOutputFile),
	get_from_iargs(suioutfile,  stream, InterpretedArgs, SUIOutputStream),
	get_from_iargs(cuioutfile,  name,   InterpretedArgs, CUIOutputFile),
	get_from_iargs(cuioutfile,  stream, InterpretedArgs, CUIOutputStream),
	format('Processing ~a and ~a --> ~a, ~a, and ~a.~n',
	       [InputFile,FilterFile,WordOutputFile,SUIOutputFile,CUIOutputFile]),
	process_input(InputStream, FilterStream,
		      WordOutputStream, SUIOutputStream, CUIOutputStream),
	close(CUIOutputStream),
	close(SUIOutputStream),
	close(WordOutputStream),
	% FilterStream is closed in process_input/5
        % close(FilterStream),
	close(InputStream),
	format('Finished.~n~n', []).


/* process_input(+InputStream, +FilterStream,
                 +WordOutputStream, +SUIOutputStream, +CUIOutputStream)

process_input/4 reads lines from InputStream and writes gleaned information to
WordOutputStream, SUIOutputStream, and CUIOutputStream. Lines from FilterStream
are of the form <word1>|<word2> and are used to filter out the words of all
strings beginning with <word1> and ending with <word2>. */

process_input(InputStream, FilterStream,
	      WordOutputStream, SUIOutputStream, CUIOutputStream) :-
	read_filter_info(FilterStream, WordPairs),
	close(FilterStream),
	write_filter_info(WordPairs),
	format(CUIOutputStream,'C.......|X~n', []),  % dummy entry for optimization
	% retractall(current_cui(_)),
	get_processing_parameters(FirstTermIsConcept, GenerateCUIs, GenerateStrings, GenerateWords),
	now(Now),
	process_input_1('C.......', FirstTermIsConcept, Now,
			GenerateCUIs, GenerateStrings, GenerateWords,
			1, InputStream, WordOutputStream, SUIOutputStream, CUIOutputStream).

process_input_1(CurrentCUI, FirstTermIsConcept, StartTime,
		GenerateCUIs, GenerateStrings, GenerateWords,
		NumLinesProcessed,
		InputStream, WordOutputStream, SUIOutputStream, CUIOutputStream) :-
	% ( at_end_of_stream(InputStream) ->
	( peek_code(InputStream, Code),
	  Code =:= -1 ->
	  true
	; fget_line(InputStream, Line) ->
	  do_housekeeping(1000, StartTime, NumLinesProcessed, Line),
	  % atom_codes(LineAtom, Line),
	  % format(user_output, 'LINE ~d: ~w~n', [NumLinesProcessed, LineAtom]),
          split_string_completely(Line, "|",
				  % [CUI0,_Language,TermStatus,_LUI,StringType,SUI,String|_]),
				  [CUI0,_LAT,TS,_LUI,STT,SUI,_ISPREF,_AUI,_SAUI,_SCUI,_SDUI,
				   _SAB,_TTY,_CODE,String,_SRL,_SUPPRESS,_CVF|_]),
          normalize_meta_string(String, NormalizedString, _NMTypes),

	  % SUIOutputStream writes to strings.gleaned
	  % strings.gleaned has the same number of lines as mrcon.filtered.
	  generate_string_output(GenerateStrings, SUIOutputStream, SUI, NormalizedString, String),
          % first_term_is_concept is a default option for glean_mrconso

	  handle_CUIs(FirstTermIsConcept, GenerateCUIs, CUIOutputStream,
		      CUI0, CurrentCUI, TS, String, STT, CUI),

          tokenize_text_mm(NormalizedString, WordStrings),
          ( is_filtered_out(WordStrings) ->
	    % atom_codes_list(WordAtoms, WordStrings),
	    % format(user_output, 'FILTERED OUT: ~w~n', [WordAtoms])
	    true
          ; length(WordStrings,NWordStrings),
	    % WordOutputStream is words.gleaned
            write_words(GenerateWords, WordStrings,1,NWordStrings,SUI,CUI,WordOutputStream)
          ),
	  NextNum is NumLinesProcessed + 1,
	  process_input_1(CUI0, FirstTermIsConcept, StartTime,
			  GenerateCUIs, GenerateStrings, GenerateWords,
			  NextNum,
			  InputStream, WordOutputStream, SUIOutputStream, CUIOutputStream)
	; format(user_output, 'ERROR: could not get line.~n', []),
	  abort
	).


/* read_filter_info(+FilterStream)

read_filter_info/1 reads lines of the form <word1>|<word2> from FilterStream
and stores them in filter_info/2, a factual predicate. */

read_filter_info(FilterStream, WordPairs) :-
	% ( at_end_of_stream(InputStream) ->
	( peek_code(FilterStream, Code),
	  Code =:= -1 ->
	  WordPairs = []
	; fget_line(FilterStream, Line) ->
	  split_string_completely(Line, "|", SplitLine),
	  ( SplitLine = [Word1String,Word2String] ->
	    atom_codes(Word1Atom, Word1String),
	    atom_codes(Word2Atom, Word2String),
	    WordPairs = [Word1Atom-Word2Atom|RestWordPairs],
	    read_filter_info(FilterStream,  RestWordPairs)
            % assert(filter_info(Word1,Word2))
          ; format('~NERROR: Illegal filter information: ~p~n', [Line]),
            halt
	  )
	; format('~NERROR: fget_line/2 failed.', []),
	  halt
	).

filter_file('filter_pairs.pl').

write_filter_info(WordPairs) :-
	filter_file(FilterFile),
	( file_exists(FilterFile, read) ->
	  true
	; open(FilterFile, write, FilterStream),
	  write_all_pairs(WordPairs, FilterStream),
	  close(FilterStream)
	),
	format(user_output, 'Compiling file ~w~n', [FilterFile]),
	maybe_abolish_filter_pair(WordPairs),
	compile(FilterFile).	

% This is ugly, but it works.
% The predicate filter_pair/2 is a static predicate defined via a stub
% at the very end of this file. The filter file is either
% * the empty (i.e., zero-length) file 0filter.0 or
% * the non-empty file 0filter.1, which contains lines like
% 2|acid
% arabidopsis|protein
% c|proteinf
% drosophila|protein
% e|protein
% human|1
% human|protein

% If the filter file is empty, we keep the static stub definition of filter_pair/2
% at the end of this file. However, if the filter file contains data, we must
%  * abolish that stub definition of filter_pair/2,
%  * create a new definition of filter_pair/2, e.g.,
% filter_pair('2',acid).
% filter_pair(arabidopsis,protein).
% filter_pair(c,protein).
% filter_pair(drosophila,protein).
% filter_pair(e,protein).
% filter_pair(human,'1').
% filter_pair(human,protein).
%  * and compile that file.


maybe_abolish_filter_pair([]).
maybe_abolish_filter_pair([_|_])  :- abolish(filter_pair/2, [force(true)]).

write_all_pairs([], _FilterStream).
write_all_pairs([Word1-Word2|RestWords], FilterStream) :-
	format(FilterStream, '~q.~n', [filter_pair(Word1, Word2)]),
	write_all_pairs(RestWords, FilterStream).

/* is_filtered_out(+WordList)

is_filtered_out/1 succeeds if WordList begins with Word1, ends with Word2,
and Word-Word2 is a member of WordPairs is true. */

is_filtered_out(WordList) :-
	WordList = [Word1String|_],
	last(WordList, Word2String),
	atom_codes(Word1Atom, Word1String),
	atom_codes(Word2Atom, Word2String),
	filter_pair(Word1Atom, Word2Atom).

/* write_words(+GenerateWords, +Words, +I, +N, +SUI, +CUI, +WordOutputStream)

write_words/7 write records of the form Word|I|N|SUI|CUI to WordOutputStream.
N is the total number of Words to be written, and I is the current one.  */

write_words(GenerateWords, WordStrings,1,NWordStrings,SUI,CUI,WordOutputStream) :-
	( GenerateWords =:= 1 ->
	  write_words_aux(WordStrings,1,NWordStrings,SUI,CUI,WordOutputStream)
	; true
	).
	  

write_words_aux([], _, _, _, _, _).
write_words_aux([Word|Rest], I, N, SUI, CUI, WordOutputStream) :-
	format(WordOutputStream, '~d|~d|~s|~s|~s~n', [I,N,Word,SUI,CUI]),
	J is I + 1,
	write_words_aux(Rest, J, N, SUI, CUI, WordOutputStream).


% do_housekeeping(+N, +StartTime, +NumLinesProcessed, +Line)
% call maybe_atom_gc and announce partial result IFF NumLinesProcessed mod N == 0
do_housekeeping(N, StartTime, NumLinesProcessed, Line) :-
	( 0 is NumLinesProcessed mod N ->
	  % format(user_output, 'maybe_atom_gc(~d)~n', [NumLinesProcessed]),
	  maybe_atom_gc(_,_),
	  now(Now),
	  TimeDiff is Now - StartTime,
	  format(user_output, '~N~d...~d...~s~n', [NumLinesProcessed, TimeDiff, Line]),
	  ttyflush
	; true
	).

get_processing_parameters(FirstTermIsConcept, GenerateCUIs, GenerateStrings, GenerateWords) :-
	( control_option(first_term_is_concept) ->
	  FirstTermIsConcept is 1
	; FirstTermIsConcept is 0
	),
	( control_option(generate_CUIs) ->
	  GenerateCUIs is 1
	; GenerateCUIs is 0
	),
	( control_option(generate_strings) ->
	  GenerateStrings is 1
	; GenerateStrings is 0
	),
	( control_option(generate_words) ->
	  GenerateWords is 1
	; GenerateWords is 0
	),
	announce_processing_parameters(GenerateCUIs, GenerateStrings, GenerateWords).

announce_processing_parameters(GenerateCUIs, GenerateStrings, GenerateWords) :-
	format(user_output, '~n~n~*c~n~*c~n~n', [80,35,80,35]),
	announce_parameter(GenerateCUIs,    'Concepts: '),
	announce_parameter(GenerateStrings, 'Strings:  '),
	announce_parameter(GenerateWords,   'Words:    '),
	format(user_output, '~n~*c~n~*c~n~n', [80,35,80,35]).

announce_parameter(OneOrZero, Entity) :-
	map_one_zero_to_yes_no(OneOrZero, YesOrNo),
	format(user_output, 'Generating ~w ~w~n', [Entity,YesOrNo]).


map_one_zero_to_yes_no(1, 'YES').
map_one_zero_to_yes_no(0, ' NO').
	

generate_CUI(GenerateCUI, CUIOutputStream, CUI0, String) :-
	( GenerateCUI =:= 1 ->
	  format(CUIOutputStream,'~s|~s~n', [CUI0,String])
	; true
	).

handle_CUIs(1, GenerateCUIs, CUIOutputStream, CUI0,
	    CurrentCUI, _TermStatus, String, _StringType, CUI) :-
	( CUI0 == CurrentCUI ->
          CUI = CUI0
	  % CUIOutputStream writes to concepts.gleaned
	  % If this is the first line in mrcon.filtered for the given CUI,
	  % then write the line to concepts.gleaned
        ; generate_CUI(GenerateCUIs, CUIOutputStream, CUI0, String),
	  CUI = "C......."
	  % retract(current_cui(_)),
	  % assert(current_cui(CUI0))
        ).
% This doesn't look like it ever gets called,
% because first_term_is_concept is a default option that is not overridden
handle_CUIs(0, GenerateCUIs, CUIOutputStream, CUI0,
	    _CurrentCUI, TermStatus, String, StringType, CUI) :-
	% TermStatus P:  the LUI is the preferred LUI of the CUI.
	% StringType PF: Preferred form of term
	% if TermStatus is P and STT is PF,
	% then the string represents the concept
	% CUIOutputStream writes to concepts.gleaned
        ( ( TermStatus == "P",
	    StringType == "PF"
	  ) ->
	  generate_CUI(GenerateCUIs, CUIOutputStream, CUI0, String),
	  CUI = "C......."
	; CUI = CUI0
	).

% SUIOutputStream writes to strings.gleaned
% strings.gleaned has the same number of lines as mrcon.filtered.
generate_string_output(GenerateStrings, SUIOutputStream, SUI, NormalizedString, String) :-
	( GenerateStrings is 1 ->
	  format(SUIOutputStream,'~s|~s|~s~n', [SUI,NormalizedString,String])
	; true
	).

% This stub definition is just to fake out the compiler,
% and will be overridden when filter_pairs.pl is compiled.
filter_pair('', '').

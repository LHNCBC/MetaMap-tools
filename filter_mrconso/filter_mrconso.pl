% File:     filter_mrconso.pl
% Module:   Filter Mrconso
% Author:   Lan
% Purpose:  Create less redundant versions of (English) mrcon by filtering
%           mrconso (essentially mrcon with source information).
%
%           Basic (lexical) filtering consists of removing strings for a concept
%           which are effectively the same as another string for the concept.
%           The normalization process consists of the following steps:
%             1. removal of (left []) parentheticals;
%             2. removal of multiple meaning designators (<n>);
%             3. NOS normalization;
%             4. syntactic uninversion;
%             5. conversion to lowercase;
%             6. replacement of hyphens with spaces; and
%             7. stripping of possessives.
%           Some right parentheticals used to be stripped, but no longer are.
%           Lexical Filtering Examples:
%           The concept "Abdomen" has strings "ABDOMEN" and "Abdomen, NOS".
%           Similarly, the concept "Lung Cancer" has string "Cancer, Lung".
%           And the concept "1,4-alpha-Glucan Branching Enzyme" has a string
%           "1,4 alpha Glucan Branching Enzyme".
%
%           Term-Status filtering filters out all lines whose Term Status
%           is either "s" (suppressible synonym) or "p" (suppressible preferred name).
%           Concepts with Term Status of "s" are filtered out regardless of whether
%           the "s" was originally assigned by the Metathesaurus developers or the
%           suppressing Java code.
%           This filtering should be done for both the strict and relaxed models,
%           and is invoked with the "S" flag.
%
%           Strict filtering additionally involves filtering out terms by syntax:
%           Concepts which parse into more than one MSU (minimal syntactic unit), i.e.,
%           more than one phrase (with certain exceptions involving prepositional
%           phrases -- see is_syntactically_simple/2).
%           Alternative criteria may be considered.
%           These concepts are filtered out because they would never be identified by MetaMap.
%
%           Term-Type filtering is no longer done.

:- module(filter_mrconso,[
	go/0,
	stop_filter_mrconso/0
    ]).

:- use_module(skr_lib(addportray),[
	add_portray/1
    ]).

:- use_module(skr_db(db_access),[
	default_full_year/1
    ]).

:- use_module(lexicon(lex_access),[
	initialize_lexicon/2
    ]).

:- use_module(tagger(tagger_access),[
	tag_text/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	get_phrase_item_name/2,
	get_phrase_item_feature/3
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	compute_unique_filename/3,
	fget_non_null_line/2,
	generate_syntactic_analysis/3,
	normalize_meta_string/3,
	parse_record/3
    ]).

:- use_module(lexicon(qp_lexicon),[
	use_multi_word_lexicon/0
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

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).


:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	portray_strings_double_quoted/1
    ]).

:- use_module(skr_lib(sicstus_utils),[
        ttyflush/0
    ]).

:- use_module(library(between),[
	between/3
   ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

:- use_module(library(lists),[
	append/2,
	last/2,
	rev/2,
	selectchk/3
    ]).

/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for filter_mrconso.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
	go(halt).

go(HaltOption) :-
	parse_command_line(CLTerm),
	go(HaltOption, CLTerm).

go(HaltOption, command_line(Options,Args)) :-
	add_portray(portray_strings_double_quoted),
	reset_control_options(filter_mrconso),
	( initialize_filter_mrconso(Options,Args,InterpretedArgs) ->
	       ( filter_mrconso(InterpretedArgs) ->
	         true
	       ; true
	       )
	; usage
	),
	stop_filter_mrconso,
	( HaltOption == halt ->
	  halt
	; true
	).


/* initialize_filter_mrconso(+Options, +Args, -InterpretedArgs)

initialize_filter_mrconso/3 interprets command line options and arguments
(opening files as necessary), and sets and displays the Filter Mrconso
control options discovered.  It returns InterpretedArgs for later use
(e.g., the stream associated with a file).  */

initialize_filter_mrconso(Options, Args, InterpretedArgs) :-
	get_control_options_for_modules([filter_mrconso], AllOptions),
	interpret_options(Options, AllOptions, filter_mrconso, IOptions),
	\+ memberchk(iopt(help,_), IOptions),
	ArgSpec = [aspec(infile,mandatory,file,read,no_default,
			 'Input file similar to mrconso.'),
		   aspec(outfile,mandatory,file,write,no_default,
			 'Output file')
		  ],
	interpret_args(IOptions, ArgSpec, Args, InterpretedArgs),
	toggle_control_options(IOptions),
	set_control_values(IOptions, InterpretedArgs),
	default_full_year(FullYear),
	use_multi_word_lexicon,
	display_current_control_options(filter_mrconso, FullYear),
	initialize_filter_mrconso,
	!.

initialize_filter_mrconso :-
	(( control_option(strict_filtering)
	 ; control_option(dump_syntax_only)
	 ) ->
	   initialize_lexicon(_,_)
	 ; true
	),
	!.
initialize_filter_mrconso :-
	format('~NERROR: initialize_filter_mrconso/0 failed.~n', []),
	!,
	stop_filter_mrconso,
	fail.

stop_filter_mrconso :- close_all_streams.


/* usage

usage/0 displays filter_mrconso usage.  */

usage :-
	format('~nUsage: filter_mrconso [<options>] <infile> <outfile>~n~n', []),
	format('  <infile> should normally be mrconso.eng.0 or the like, and~n', []),
	format('  <outfile> is <infile> filtered.~n~n', []),
	display_control_options_for_modules(filter_mrconso, []).

/* filter_mrconso(+InterpretedArgs)

filter_mrconso/1 controls all filter_mrconso processing.  */

filter_mrconso(InterpretedArgs) :-
	get_from_iargs(infile, name, InterpretedArgs, InputFile),
	get_from_iargs(infile, stream, InterpretedArgs, InputStream),
	get_from_iargs(outfile, name, InterpretedArgs, OutputFile),
	get_from_iargs(outfile, stream, InterpretedArgs, OutputStream),
	get_progress_bar_interval(Interval),
	get_total_lines(TotalLines),
	format('Processing ~a --> ~a.~n', [InputFile,OutputFile]),
	process_input(InputStream, InputFile, OutputStream, Interval, TotalLines,
		      NormCounts, TSCounts, SyntaxCounts),
	write_normalization_counts(NormCounts, OutputStream),
	write_term_status_counts(TSCounts, OutputStream),
	write_syntax_counts(SyntaxCounts, OutputStream),
	( control_option(end_of_processing) ->
	  format(OutputStream, '<<< EOT >>>~n', [])
	; true
	),
	close(OutputStream),
	close(InputStream),
	!.

get_progress_bar_interval(Interval) :-
	( control_value(progress_bar_interval, Interval) ->
	  true
	; Interval = 1000
	).

get_total_lines(TotalLines) :-
	( control_value(total_lines, TotalLines) ->
	  true
	; TotalLines = -9999
	).


/* process_input(+InputStream, +OutputStream)

process_input/2 reads lines from InputStream and writes filtered lines to
OutputStream.  */

process_input(InputStream, InputFile, OutputStream, Interval, TotalLines,
	      NormCounts, TSCounts, SyntaxCounts) :-
	fget_non_null_line(InputStream, Line0),
	parse_line(Line0, LineData, CUI0, LUI0, TS0, STT0, TTY0, STR0, SAB0, SCD0),
	atom_codes(STR0, STR0String),
	normalize_meta_string(STR0String, NMSTRString, NMTypes0),
	atom_codes(NMSTR0, NMSTRString),
	Line = clinfo(CUI0,LineData,TS0,STT0,TTY0,STR0,SAB0,SCD0,NMSTR0,NMTypes0),
	NumLines is 1,
	process_cui_lui(InputStream, InputFile, OutputStream,
			NumLines, Interval, TotalLines,
			CUI0, LUI0, [Line],
			[], NormCounts,
			[p-0,s-0], TSCounts,
			[pref-0,synt-0], SyntaxCounts).

/* parse_line(+Line, -CUI, -LUI, -TS, -STT, -TTY, -STR, -SAB, -SCD)

parse_line/9 extracts CUI, ... from Line.  */

parse_line(LineString, LineAtom, CUIAtom, LUIAtom, TSAtom,
	   STTAtom, TTYAtom, STRAtom, SABAtom, SCDAtom) :-
	parse_record(LineString, "|",
		     [CLSString,_N,TSString,STTString,STRString,SABString,TTYString,SCDString]),
	atom_codes_list([LineAtom,TSAtom,STTAtom,STRAtom,SABAtom,TTYAtom,SCDAtom],
			[LineString,TSString,STTString,STRString,SABString,TTYString,SCDString]),
	parse_record(CLSString, ":", [CUIString,LUIString,_SUI]),
	atom_codes_list([CUIAtom,LUIAtom],[CUIString,LUIString]),
	!.
parse_line(Line, _, _, _, _, _, _, _, _, _) :-
	format('~NFatal error: Bad input ~s~n', [Line]),
	stop_filter_mrconso,
	halt.

/* process_cui_lui(+InputStream, +OutputStream, +CUI, +LUI, +CLInfoLines)

   
   ***     WARNING     WARNING     WARNING     WARNING     WARNING     ***
   
      The current implementation processes according to CUI only.
      This should have the effect of filtering out more strings at the expense
      of not necessarily having a representative for each LUI.
   
   ***     WARNING     WARNING     WARNING     WARNING     WARNING     ***

   
   process_cui_lui/6 accumulates CLInfoLines (clinfo/7 terms) by
   reading Line from InputStream, extracting some fields, forming the
   normalized Meta string NMSTR) with the same concept id (CUI) and
   term id (LUI) as the input.  When a new CUI,LUI pair is encountered, the
   accumulated information is filtered "removing" duplicate NMSTRs and written to
   OutputStream (prefixed by y to indicate survival from filtering).
   Duplicates are written to OutputStream with a n prefix.
   The information from the current line is saved for further processing.
*/

process_cui_lui(InputStream, InputFile, OutputStream,
		NumLinesIn, Interval, TotalLines,
		CUI0, LUI0, CLInfoLines0,
		NormCountsIn, NormCountsOut,
		TSCountsIn, TSCountsOut,
		SyntaxCountsIn, SyntaxCountsOut) :-
	( fget_non_null_line(InputStream, Line) ->
	  NumLinesNext is NumLinesIn + 1,
	  % format(user_output, '~d:~a~n', [NumLinesNext,Line]),
	  announce_lines(NumLinesNext, Interval, TotalLines, InputFile),
	  parse_line(Line, LineData, CUI, LUI, TS, STT, TTY, STR, SAB, SCD),
	  % LAT="ENG",   % limit to English; no need with mrconso.eng
	  atom_codes(STR, STRString),
	  normalize_meta_string(STRString, NMSTRString, NMTypes),
	  atom_codes(NMSTR, NMSTRString),
	  NextLine = clinfo(CUI,LineData,TS,STT,TTY,STR,SAB,SCD,NMSTR,NMTypes),
	  % format(user_output, '~n~w~n', [NextLine]),
	  ( CUI == CUI0 ->
	    process_cui_lui(InputStream, InputFile, OutputStream,
			    NumLinesNext, Interval, TotalLines,
			    CUI0, LUI0, [NextLine|CLInfoLines0],
			    NormCountsIn, NormCountsOut,
			    TSCountsIn, TSCountsOut,
			    SyntaxCountsIn, SyntaxCountsOut)
	  ; filter_and_write(OutputStream, CLInfoLines0,
			     NormCountsIn, NormCountsNext,
			     TSCountsIn, TSCountsNext,
			     SyntaxCountsIn, SyntaxCountsNext),			     
	    process_cui_lui(InputStream, InputFile, OutputStream,
			    NumLinesNext, Interval, TotalLines,
			    CUI, LUI, [NextLine],
			    NormCountsNext, NormCountsOut,
			    TSCountsNext, TSCountsOut,
			    SyntaxCountsNext, SyntaxCountsOut)
	  )
	  % If there are no more lines to read, simply write out what's been accumulated.
	; filter_and_write(OutputStream, CLInfoLines0,
			   NormCountsIn, NormCountsOut,
			   TSCountsIn, TSCountsOut,
			   SyntaxCountsIn, SyntaxCountsOut)
	).

% Announce how many lines have been processed every Interval lines
% and at the very last line.
announce_lines(NumLines, Interval, TotalLines, InputFile) :-
	( TotalLines > 0 ->
	  announce_lines_with_total(NumLines, Interval, TotalLines, InputFile)
	; announce_lines_without_total(NumLines, Interval, InputFile)
	).

announce_lines_without_total(NumLines, Interval, InputFile) :-
	( 0 is NumLines mod Interval ->
	  maybe_atom_gc(_, _),
	  format(user_output,
		 '~NProcessed ~d lines of file ~w~n',
		 [NumLines,InputFile])
	; true
	).

announce_lines_with_total(NumLines, Interval, TotalLines, InputFile) :-
	( NumLines is TotalLines ->
	  format(user_output,
		 '~NCOMPLETED ~d of ~w lines of file ~w~n',
		 [NumLines,TotalLines,InputFile])
	; 0 is NumLines mod Interval ->
	  maybe_atom_gc(_, _),
	  format(user_output,
		 '~NProcessed ~d of ~w lines of file ~w~n',
		 [NumLines,TotalLines,InputFile])
	; true
	).



/* filter_and_write(+OutputStream, +CLInfoLines)

filter_and_write/3 removes entries (clinfo/7 terms) from CLInfoLines
according to several criteria:
  strings with the same normalized Meta string (NMSTR),
  strings with particular term types (unless 'X' is specified;
  Note: this option is now obsolete, because we no longer do term-type filtering)
  strings with particular term status, and
  strings with complex syntax (several phrases rather than one)
Every entry is written to OutputStream with an initial y or n
indicating if it survived filtering. */

filter_and_write(OutputStream,   CLInfoLines0,
		 NormCountsIn,   NormCountsOut,
		 TSCountsIn,     TSCountsOut,
		 SyntaxCountsIn, SyntaxCountsOut) :-
	% First, find the preferred concept name.
	find_preferred(CLInfoLines0, Preferred),
	% Preferred = clinfo(CUI0,_Line0,_TS0,_STT0,_TTY0,STR0,_SAB0,_SCD0,_NMSTR0,_NMTypes0),
	% atom_codes(StrAtom0, STR0),
	% atom_codes(CUIAtom0, CUI0),
	% format(user_output, 'PREFERRED: ~w|~w~n', [CUIAtom0,StrAtom0]),
	% First, do term-status filtering in the strict model only
	% ntss
	( control_option(strict_filtering) ->
	  filter_by_term_status(CLInfoLines0,  OutputStream,
				TSCountsIn,    TSCountsNext,
				TSExclusions0, CLInfoLines1)
	; CLInfoLines1 = CLInfoLines0,
	  TSCountsOut = TSCountsIn,
	  TSExclusions0 = []
	),
	% then do lexical filtering
	% (type filtering is done first to prevent the filtering out of a good
	% lexical representative with a bad type)
	% nnorm
	filter_nmstr_dups(CLInfoLines1, OutputStream,
			  NormCountsIn, NormCountsNext, NormExclusions0, CLInfoLines2),
	( ( control_option(strict_filtering)
	  ;  control_option(dump_syntax_only)
	  ) ->
	  % finally do syntactic filtering
	  % nsynt
	  filter_syntactically(CLInfoLines2, OutputStream,
			       SyntaxCountsIn, SyntaxCountsNext, SyntaxExclusions0, CLInfoLines3)
	; CLInfoLines3 = CLInfoLines2,
	  SyntaxCountsNext = SyntaxCountsIn,
	  SyntaxExclusions0 = []
	),
	% further filtering?
	( control_option(dump_syntax_only) ->
	  SyntaxCountsOut = SyntaxCountsNext
	; restore_pref_concept(CLInfoLines3, Preferred, CLInfoLines4,
			       TSCountsNext,     TSCountsOut,
			       NormCountsNext,   NormCountsOut,
			       SyntaxCountsNext, SyntaxCountsOut,
			       TSExclusions0, NormExclusions0, SyntaxExclusions0,
			       TSExclusions1, NormExclusions1, SyntaxExclusions1),

	  write_exclusions(OutputStream, TSExclusions1, NormExclusions1, SyntaxExclusions1),
	  write_clinfo_lines(CLInfoLines4, OutputStream)
	),
	!.

write_exclusions(OutputStream, TSExclusions, NormExclusions, SyntaxExclusions) :-
	  write_term_status_exclusions(TSExclusions, OutputStream),
	  write_normalization_exclusions(NormExclusions, OutputStream),
	  write_syntax_exclusions(SyntaxExclusions, OutputStream).

write_term_status_exclusions([], _OutputStream).
write_term_status_exclusions([CLInfoLine|Rest], OutputStream) :-
	CLInfoLine = clinfo(_CUI,Line,_TS,_STT,_TTY,_STR,_SAB,_SCD,NMSTR,NMTypes),
        format(OutputStream,'ntss|~a|~p|~a~n', [Line,NMTypes,NMSTR]),
	write_term_status_exclusions(Rest, OutputStream).

write_normalization_exclusions([], _OutputStream).
write_normalization_exclusions([CLInfoLine|Rest], OutputStream) :-
	CLInfoLine = clinfo(_CUI,Line,_TS,_STT,_TTY,_STR,_SAB,_SCD,NMSTR,NMTypes),
	format(OutputStream,'nnorm|~a|~p|~a~n', [Line,NMTypes,NMSTR]),
	write_normalization_exclusions(Rest, OutputStream).

write_syntax_exclusions([], _OutputStream).
write_syntax_exclusions([CLInfoLine|Rest], OutputStream) :-
	CLInfoLine = clinfo(_CUI,Line,_TS,_STT,_TTY,_STR,_SAB,_SCD,NMSTR,NMTypes),
	format(OutputStream, 'nsynt|~a|~p|~a~n', [Line,NMTypes,NMSTR]),          
	write_syntax_exclusions(Rest, OutputStream).

% CLInfoLines is the list of remaining concepts after term-status filtering,
% lexical (normalization) filtering, and syntactic filtering.
% If at least one concept remains, i.e., CLInfoLines \== [],
% but the preferred concept was excluded i.e., \+ memberchk(Preferred, CLInfoLines3),
% then restore the preferred concept.
% Also delete it from whichever exclusion list it was in,
% and update the counts accordingly.
restore_pref_concept(CLInfoLinesIn, Preferred, CLInfoLinesOut,
		     TSCountsIn,     TSCountsOut,
		     NormCountsIn,   NormCountsOut,
		     SyntaxCountsIn, SyntaxCountsOut,
		     TSExclusionsIn, NormExclusionsIn, SyntaxExclusionsIn,
		     TSExclusionsOut, NormExclusionsOut, SyntaxExclusionsOut) :-
	( CLInfoLinesIn \== [],
	  \+ memberchk(Preferred, CLInfoLinesIn),
	  Preferred \== none ->
	  Preferred = clinfo(CUI,_Line,_TS,_STT,_TTY,STR,_SAB,_SCD,_NMSTR,_NMTypes),
	  update_syntax_count(pref, 1, SyntaxCountsIn, SyntaxCountsInOut),
	  % put the preferred concept back into the CLInfoLines
	  CLInfoLinesOut = [Preferred|CLInfoLinesIn],
	  delete_pref(Preferred,
		      TSExclusionsIn, NormExclusionsIn, SyntaxExclusionsIn,
		      TSCountsIn,      TSCountsOut,
		      NormCountsIn,    NormCountsOut,
		      SyntaxCountsInOut, SyntaxCountsOut,
		      TSExclusionsOut, NormExclusionsOut, SyntaxExclusionsOut,
		      Reason),
	  format(user_output,
		 '~NRestored concept ~w|~w previously deleted because of ~w~n',
		 [CUI,STR,Reason])
	; TSCountsOut         = TSCountsIn,
	  NormCountsOut       = NormCountsIn,
	  SyntaxCountsOut     = SyntaxCountsIn,
	  TSExclusionsOut     = TSExclusionsIn,
	  NormExclusionsOut   = NormExclusionsIn,
	  SyntaxExclusionsOut = SyntaxExclusionsIn,
	  CLInfoLinesOut      = CLInfoLinesIn
	).

delete_pref(Preferred,
	    TSExclusionsIn,   NormExclusionsIn,  SyntaxExclusionsIn,
	    TSCountsIn,       TSCountsOut,
	    NormCountsIn,     NormCountsOut,
	    SyntaxCountsIn,   SyntaxCountsOut,
	    TSExclusionsOut,  NormExclusionsOut, SyntaxExclusionsOut,
	    Reason) :-
	Preferred = clinfo(_CUI,_Line,TS,_STT,_TTY,_STR,_SAB,_SCD,_NMSTR,NMTypes),
	  % Was the preferred concept excluded because of term status?
	( selectchk(Preferred, TSExclusionsIn,  TSExclusionsOut) ->
	  Reason = 'term status',
	  % Decrement the appropriate term status count,
	  update_term_status_count(TS, -1, TSCountsIn, TSCountsOut),
	  % leave the normalization counts unchanged
	  NormCountsOut       = NormCountsIn,
	  % leave the syntax counts unchanged
	  SyntaxCountsOut     = SyntaxCountsIn,
	  % leave the normalization exclusions unchanged
	  NormExclusionsOut   = NormExclusionsIn,
	  % leave the syntax exclusions unchanged
	  SyntaxExclusionsOut = SyntaxExclusionsIn
	  % Was the preferred concept excluded because of normalization?
	; selectchk(Preferred, NormExclusionsIn, NormExclusionsOut) ->
	  Reason = normalization,
	  % Decrement all the appropriate normalization counts
	  update_all_normalization_counts([NMTypes|NMTypes], -1, NormCountsIn, NormCountsOut),
	  % leave the term-status counts unchanged
	  TSCountsOut         = TSCountsIn,
	  % leave the syntax counts unchanged
	  SyntaxCountsOut     = SyntaxCountsIn,
	  % leave the term-status exclusions unchanged
	  TSExclusionsOut     = TSExclusionsIn,
	  % leave the syntax exclusions unchanged
	  SyntaxExclusionsOut = SyntaxExclusionsIn
	  % Was the preferred concept excluded because of syntax?
	; selectchk(Preferred, SyntaxExclusionsIn, SyntaxExclusionsOut) ->
	  Reason = syntax,
	  % Decrement the synt syntax count
	  update_syntax_count(synt, -1, SyntaxCountsIn, SyntaxCountsOut),
	  % leave the term-status counts unchanged
	  TSCountsOut         = TSCountsIn,
	  % leave the normalization counts unchanged
	  NormCountsOut       = NormCountsIn,
	  % leave the term-status exclusions unchanged
	  TSExclusionsOut     = TSExclusionsIn,
	  % leave the normalization exclusions unchanged
	  NormExclusionsOut   = NormExclusionsIn
	).

find_preferred(RevCLInfoLines, Preferred) :-
	rev(RevCLInfoLines,CLInfoLines),
	( member(InfoLine, CLInfoLines),
	  InfoLine = clinfo(_,_,TS,STT,_,_,_,_,_,_),
	  TS == 'P',
	  STT == 'PF' ->
	  Preferred = InfoLine
	; CLInfoLines = [FirstCLInfoLine|_],
	  FirstCLInfoLine = clinfo(CUI,_,_,_,_,_,_,_,_,_),
	  format(user_output, '~NNo preferred form found for CUI ~w~n', [CUI]),
	  Preferred = none
	).

/* filter_nmstr_dups(+CLInfoLines, +OutputStream,
   		     -NormCountsIn, -NormCountsOut,
		     -NormExclusions, -FilteredCLInfoLines)

filter_nmstr_dups/6.  See filter_and_write/1.  */

filter_nmstr_dups([], _, NormCounts, NormCounts, [], []).
filter_nmstr_dups([CLInfoLine|Rest], OutputStream, 
		  NormCountsIn, NormCountsOut,
		  [CLInfoLine|RestExcluded], FilteredRest) :-
	CLInfoLine = clinfo(_CUI,_Line,_TS,_STT,_TTY,_STR,_SAB,_SCD,NMSTR,NMTypes),
	nmstr_is_duplicate(NMSTR, Rest),
	!,
	% format(user_output, 'DUPLICATE: ~a ~a ~a ~a~n', [STR,NMSTR,SAB,TTY]),
	% We want to update the normalization counts for
	% (1) the entire list of NMTypes, e.g., [case,hyphen,poss] and
	% (2) each individual types e.g., case, hyphen, and poss.
	% Creating the list [NMTypes|NMTypes] enables a simple way of doing this.
	% Suppose as above that NMTypes = [case,hyphen,poss];
	% Then [NMTypes|NMTypes] = will be [[case,hyphen,poss],case,hypen,poss],
	% and we just have to update the normalization count for each element of that list!
	update_all_normalization_counts([NMTypes|NMTypes], 1, NormCountsIn, NormCountsNext),
	filter_nmstr_dups(Rest, OutputStream,
			  NormCountsNext, NormCountsOut, RestExcluded, FilteredRest).
filter_nmstr_dups([First|Rest], OutputStream,
		  NormCountsIn, NormCountsOut, Excluded, [First|FilteredRest]) :-
	filter_nmstr_dups(Rest, OutputStream,
			  NormCountsIn, NormCountsOut, Excluded, FilteredRest).


update_all_normalization_counts([], _Increment, NormCounts, NormCounts).
update_all_normalization_counts([H|T], Increment, NormCountsIn, NormCountsOut) :-
	update_one_normalization_count(NormCountsIn, H, Increment, NormCountsNext),
	update_all_normalization_counts(T, Increment, NormCountsNext, NormCountsOut).

% If we've reached the end of the counts list,
% then the normaliztion type we're looking at is not yet in the list,
% so add it with a count of 1.
update_one_normalization_count([], NormType, _Increment, [NormType-1]).
update_one_normalization_count([H|T], NormType, Increment, NormCountsNext) :-
	( H = NormType-Count ->
	  NextCount is Count + Increment,
	  NormCountsNext = [NormType-NextCount|T]
	; NormCountsNext = [H|UpdatedT],
	  update_one_normalization_count(T, NormType, Increment, UpdatedT)
	).

/* nmstr_is_duplicate(+NMSTR, +CLInfoLines)

nmstr_is_duplicate/2 succeeds if NMSTR occurs in one of the CLInfoLines.  */

% nmstr_is_duplicate(_NMSTR,[]) :-
%     !,
%     fail.
nmstr_is_duplicate(NMSTR,
		   [clinfo(_CUI,_Line,_TS,_STT,_TTY,_STR,_SAB,_SCD,NMSTR,_NMTypes)|_]) :- !.
nmstr_is_duplicate(NMSTR,
		   [_|Rest]) :-
	nmstr_is_duplicate(NMSTR, Rest).


% filter_by_term_status(+CLInfoLines, +OutputStream, -FilteredCLInfoLines)
% filters out and writes with "ntss" prefix lines whose Term Status is "s" or "p"

filter_by_term_status([], _OutputStream, TSCounts, TSCounts, [], []).
filter_by_term_status([CLInfoLine|RestCLInfoLines], OutputStream,
		      TSCountsIn, TSCountsOut,
		      [CLInfoLine|RestExcluded], FilteredRest) :-
	CLInfoLine = clinfo(_CUI,_Line,TS,_STT,_TTY,_STR,_SAB,_SCD,_NMSTR,_NMTypes),
	excluded_term_status(TS),
	!,
	update_term_status_count(TS, 1, TSCountsIn, TSCountsNext),
	filter_by_term_status(RestCLInfoLines, OutputStream,
			      TSCountsNext, TSCountsOut,
			      RestExcluded, FilteredRest).
filter_by_term_status([First|Rest], OutputStream,
		      TSCountsIn, TSCountsOut, Excluded, [First|FilteredRest]) :-
	filter_by_term_status(Rest, OutputStream,
			      TSCountsIn, TSCountsOut, Excluded, FilteredRest).

excluded_term_status(p).
excluded_term_status(s).

update_term_status_count(p, Increment,
			 [p-PCountIn,s-SCount],
			 [p-PCountNext,s-SCount]) :-
	PCountNext is PCountIn + Increment.
update_term_status_count(s, Increment,
			 [p-PCount,s-SCountIn],
			 [p-PCount,s-SCountNext]) :-
	SCountNext is SCountIn + Increment.

update_syntax_count(pref, Increment,
		    [pref-PrefCountIn,   synt-SyntCount],
		    [pref-PrefCountNext, synt-SyntCount]) :-
	PrefCountNext is PrefCountIn + Increment.
update_syntax_count(synt, Increment,
		    [pref-PrefCount, synt-SyntCountIn],
		    [pref-PrefCount, synt-SyntCountNext]) :-
	SyntCountNext is SyntCountIn + Increment.

/* filter_syntactically(+CLInfoLines, +OutputStream, -SyntaxExclusions, -FilteredCLInfoLines)

filter_syntactically/4.  See filter_and_write/1.  */

filter_syntactically([], _, SyntaxCounts, SyntaxCounts, [], []).
% The first recursive clause of filter_syntactically/4 handles complex phrases
% that are excluded, and therefore added to the third argument.
filter_syntactically([CLInfoLine|Rest], OutputStream,
		     SyntaxCountsIn, SyntaxCountsOut,
		     [CLInfoLine|RestExcluded], FilteredRest) :-
	CLInfoLine = clinfo(CUI,_Line,_TS,_STT,_TTY,_STR,SAB,SCD,NMSTR,_NMTypes),
	atom_codes(NMSTR, NMSTRString),
	parse_it(NMSTRString, minimal_syntax(Phrases)),
	length(Phrases, MSUCount), % number of minimal syntactic units (i.e., phrases)
	( control_option(dump_syntax_only) ->
	  simplify_all_phrases(Phrases, SUs0),
	  append(SUs0, SUs),
	  length(SUs, SyntaxCount), % number of syntactic items (e.g., shapes, mod, ...)
	  generate_dump_syntax_only_output(SyntaxCount, MSUCount, CUI, SCD, SAB, OutputStream)
	; \+ is_syntactically_simple(Phrases, MSUCount),   % do the filtering
	  update_syntax_count(synt, 1, SyntaxCountsIn, SyntaxCountsNext)
	),
	!,
	filter_syntactically(Rest, OutputStream,
			     SyntaxCountsNext, SyntaxCountsOut, RestExcluded, FilteredRest).
% The second recursive clause of filter_syntactically/4 handles simple phrases
% that are not excluded, and therefore added to the fourth argument.
filter_syntactically([First|Rest], OutputStream,
		     SyntaxCountsIn, SyntaxCountsOut, Excluded, [First|FilteredRest]) :-
	filter_syntactically(Rest, OutputStream,
			     SyntaxCountsIn, SyntaxCountsOut, Excluded, FilteredRest).

generate_dump_syntax_only_output(SyntaxCount, MSUCount, CUI, SCD, SAB, OutputStream) :-
	( SyntaxCount =:= 0 ->
	  format(OutputStream,
		 '1|1|~a|~a|~a|~a|~p~n',
		 [CUI,SAB,SCD,NMSTR,SUs0]),
	  flush_output(OutputStream)
	; format(OutputStream,
		 '~d|~d|~a|~a|~a|~a|~p~n',
		 [MSUCount,SyntaxCount,CUI,SAB,SCD,NMSTR,SUs0]),
	  flush_output(OutputStream)
	).

/* is_syntactically_simple/2(+Phrases, +MSUCount)

is_syntactically_simple/2 succeeds if either MSUCount (the number of minimal
syntactic units) is 1 or Phrases is of the form 'a <prep> b' or
'a <prep> b of c of ...'. */

is_syntactically_simple(Phrases, Length) :-
	( Length =:= 1 ->
	  true
	; Phrases = [_First,Second|Rest],
	  is_prep_phrase(Second),
	  are_of_phrases(Rest)
	).

is_prep_phrase([FirstItem,_NextItem|_]) :- % there must be something after the
	get_phrase_item_name(FirstItem, Name),  % the prep
	!,
	Name == prep.

are_of_phrases([]).
are_of_phrases([First|Rest]) :-
	is_of_phrase(First),
	are_of_phrases(Rest).

is_of_phrase([FirstItem,_NextItem|_]) :- % there must be something after 'of'
	get_phrase_item_name(FirstItem, Name),
	get_phrase_item_feature(FirstItem, inputmatch, InputMatch),
	% format('~p~n~p~n',[Name,InputMatch]),
	!,
	Name == prep,
	InputMatch == [of].

% Try tagging and parsing five times, and quit if still unsuccessful.
parse_it(NMSTR, SyntacticAnalysis) :-
	between(1, 5, _),
	   tag_text(NMSTR, TagList),
	   generate_syntactic_analysis(NMSTR, TagList, SyntacticAnalysis),
	!.
parse_it(NMSTR, minimal_syntax([[]])) :-
	format('~NError: Cannot parse ~a after 5 attempts~n', [NMSTR]),
	ttyflush,
	halt.

simplify_all_phrases([], []).
simplify_all_phrases([First|Rest], [SFirst|SRest]) :-
	simplify_one_phrase(First, SFirst),
	simplify_all_phrases(Rest, SRest).

simplify_one_phrase([], []).
simplify_one_phrase([First|Rest], [Cat|SRest]) :-
	functor(First, Cat, _),
	simplify_one_phrase(Rest, SRest).


/* write_clinfo_lines(+CLInfoLines, +OutputStream)

write_clinfo_lines/2.  See filter_and_write/2.  */

write_clinfo_lines([], _).
write_clinfo_lines([CLInfoLine|Rest], OutputStream) :-
	CLInfoLine = clinfo(_CUI,Line,_TS,_STT,_TTY,_STR,_SAB,_SCD,_NMSTR,_NMTypes),
	% y == "yes": the line survived filtering
	format(OutputStream, 'y|~a~n', [Line]),
	flush_output(OutputStream),
	write_clinfo_lines(Rest, OutputStream).

% NormCounts0 is a list of terms of the form NormTypes-Count,
% where NormTypes is either
% * a single NormCount, which is an atom, e.g., uninv, case, hyphen, or
% * a list or NormTypes, e.g., [uninv,case], [uninv,case,hyphen].

% A typical NormCounts0 is
% [[uninv,case]-10,uninv-17,case-61,[case]-28,[uninv,case,hyphen]-7,hyphen-23,[case,hyphen]-16]

write_normalization_counts(NormCounts0, OutputStream) :-
	% Sorting and then reversing NormCounts0 ensures that
	% the NormType lists (compound normalization counts)
	% appear in the list before
	% the NormType atoms (base normalization counts).
	sort(NormCounts0, SortedNormCounts),
	rev(SortedNormCounts, NormCounts),
	write_normalization_counts_1(NormCounts, OutputStream).

write_normalization_counts_1([], _OutputStream).
write_normalization_counts_1([H|T], OutputStream) :-
	H = NormalizationTypes-Count,
	get_normalization_count_indicator(NormalizationTypes, IndicatorLetter),
	format(OutputStream, '~Ni~wn|~d|~w~n', [IndicatorLetter,Count,NormalizationTypes]),
	flush_output(OutputStream),
	write_normalization_counts_1(T, OutputStream).

get_normalization_count_indicator(H, IndicatorLetter) :-
	( H = [_|_] ->
	  % Compound Normalization Count
	  IndicatorLetter = 'c'
	; H == [] ->
	  % Compound Normalization Count
	  IndicatorLetter = 'c'
	  % Base Normalization Count
	; IndicatorLetter = 'b'
	).

write_term_status_counts([p-PCount,s-SCount], OutputStream) :-
	format(OutputStream, '~Nit|~d|p~n', [PCount]),
	format(OutputStream, '~Nit|~d|s~n', [SCount]),
	flush_output(OutputStream).

write_syntax_counts([pref-PrefCount,synt-SyntCount], OutputStream) :-
	format(OutputStream, '~Nis|~d|pref~n', [PrefCount]),
	format(OutputStream, '~Nis|~d|synt~n', [SyntCount]),
	flush_output(OutputStream).


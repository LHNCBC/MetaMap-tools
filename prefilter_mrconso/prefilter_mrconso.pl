% File:     prefilter_mrconso.pl
% Module:   PreFilter Mrconso
% Author:   Lan
% Purpose:  Determine cases in which some mrconso strings should be marked
%           as suppressible. The original intent was to produce a new version
%           of the input file with additional records being marked as
%           suppressible ("s"). But the only active use of this program,
%           that of determining "brand name suppression", simply writes
%           those records that should be suppressed.
%           Originally,
%           filter out ambiguous mrconso strings by marking them suppressible.
%           The strings marked suppressible are proper substrings of other
%           strings within the same concept. Examples include:

/*

C1546380:L5530194:S6330328|2980770|P|PF|Other - Event Reason|MTH|PN|NOCODE|0|0|
C1546380:L0249050:S0324820|2980771|S|PF|Other|HL7V2.5|PT|O|0|0|
where Other is suppressed

C1546837:L5530153:S6330287|2981436|P|PF|Unknown - Special Program Code|MTH|PN|NOCODE|0|0|
C1546837:L0887186:S1053711|2981437|S|PF|Unknown|HL7V2.5|PT|U|0|0|
where Unknown is suppressed

C1550523:L5618043:S6441892|2985903|P|PF|SUGG - Retired Code|MTH|PN|NOCODE|0|0|
C1550523:L5527434:S6324062|2985904|S|PF|Retired Code|HL7V3.0|NPT|SUGG|0|0|
where Retired Code is suppressed

and

C0549070:L1030284:S1242901|1333609|P|PF|Assessment: Coping|MTH|PN|NOCODE|0|0|
C0549070:L1030284:S1242901|1333609|P|PF|Assessment: Coping|PCDS|HX|U000018|0|3|
C0549070:L1007288:S1209917|1333610|S|PF|Assessment|PCDS|HT|U000018|3|3|
where Assessment is suppressed.

Note that in the following case, the suppression is not as clearly desired:
C0034599:L1774205:S2070145|172657|P|PF|Radiology Specialty|MTH|PN|NOCODE|0|0|
C0034599:L1774205:S3503133|172658|P|VC|Radiology - specialty|SNOMEDCT|OF|393401009|4|4|
C0034599:L1774205:S3503133|172658|P|VC|Radiology - specialty|SNOMEDCT|OF|394351007|4|4|
C0034599:L1774205:S3503133|172658|P|VC|Radiology - specialty|SNOMEDCT|OP|393401009|4|4|
C0034599:L1774205:S3503133|172658|P|VC|Radiology - specialty|SNOMEDCT|OP|394351007|4|4|
C0034599:L1774205:S3503133|172658|P|VC|Radiology - specialty|SNOMEDCT|PT|394914008|4|4|
C0034599:L0034599:S0079689|172659|S|PF|Radiology|LCH|PT|U004032|0|0|
C0034599:L0034599:S0079689|172659|S|PF|Radiology|MSH|MH|D011871|0|0|
C0034599:L0034599:S0079689|172659|S|PF|Radiology|NCI|PT|C17059|0|0|
C0034599:L0034599:S0079689|172659|S|PF|Radiology|PSY|PT|42740|0|3|
C0034599:L0034599:S0079689|172659|S|PF|Radiology|SNOMEDCT|SY|394914008|0|4|
C0034599:L0034599:S0424858|172660|S|VC|radiology|CSP|PT|2558-7535|0|0|
C0034599:L0034599:S0424858|172660|S|VC|radiology|NCI|SY|C17059|0|0|
C0034599:L0846389:S1463977|172661|S|PF|Radiology, General|NCI|SY|C17059|0|0|
C0034599:L1117800:S3690717|172662|S|PF|RAD|NCI|SY|C17059|0|0|
C0034599:L1117824:S6285892|172663|S|PF|Radiology services|ALT|HT|DF|3|3|
C0034599:L1196441:S1433168|172664|S|PF|radiology (field)|AOD|DE|0000014224|0|0|
C0034599:L3125286:S3503132|172665|S|PF|Radiology - specialty (qualifier value)|SNOMEDCT|FN|394914008|4|4|
C0034599:L3126199:S3503131|172666|S|PF|Radiology - speciality|SNOMEDCT|PTGB|394914008|4|4|
where Radiology and radiology are suppressed. */


:- module(prefilter_mrconso,[
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
	parse_record/3
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

:- use_module(skr_lib(sicstus_utils),[
	lower/2
    ]).

:- use_module(library(lists),[
	append/2,
	rev/2
    ]).

:- dynamic suppression_count/2.

/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for prefilter_mrconso.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    add_portray(portray_strings_double_quoted),
    reset_control_options(prefilter_mrconso),
    (initialize_prefilter_mrconso(Options,Args,InterpretedArgs) ->
        (prefilter_mrconso(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_prefilter_mrconso(+Options, +Args, -InterpretedArgs)

initialize_prefilter_mrconso/3 interprets command line options and arguments
(opening files as necessary), and sets and displays the Filter Mrconso
control options discovered.  It returns InterpretedArgs for later use
(e.g., the stream associated with a file).  */

initialize_prefilter_mrconso(Options,Args,InterpretedArgs) :-
    get_control_options_for_modules([prefilter_mrconso],AllOptions),
    interpret_options(Options,AllOptions,prefilter_mrconso,IOptions),
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
    display_current_control_options(prefilter_mrconso, Release),
    retractall(suppression_count(_,_)),
    !.


/* usage

usage/0 displays prefilter_mrconso usage.  */

usage :-
    format('~nUsage: prefilter_mrconso [<options>] <infile> <outfile>~n~n',[]),
    format('  <infile> should normally be mrconso.eng.0 or the like, and~n',[]),
    format('  <outfile> is either a subset of <infile> to be suppressed',[]),
    format('  or it is <infile>  with some strings marked as~n',[]),
    format('  suppressible synonyms.~n~n',[]),
    display_control_options_for_modules(prefilter_mrconso,[]).


/* prefilter_mrconso(+InterpretedArgs)

prefilter_mrconso/1 controls all prefilter_mrconso processing.  */

prefilter_mrconso(InterpretedArgs) :-
    get_from_iargs(infile,name,InterpretedArgs,InputFile),
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    get_from_iargs(outfile,name,InterpretedArgs,OutputFile),
    get_from_iargs(outfile,stream,InterpretedArgs,OutputStream),
    format('Processing ~a --> ~a.~n',[InputFile,OutputFile]),
    process_input(InputStream,OutputStream),
    write_suppression_counts,
    close(OutputStream),
    close(InputStream),
    format('~nFinished.~n~n',[]),
    !.


/* process_input(+InputStream, +OutputStream)

process_input/2 reads lines from InputStream and writes filtered lines to
OutputStream.  */

process_input(InputStream, OutputStream) :-
	erase_all_facts(saved,info),
	fget_non_null_line(InputStream, Line0),
	parse_line(Line0,CUI0,LUI0,SUI0,TS0,STT0,STR0,SAB0,TTY0,CODE0,SRL0),
	lower(STR0,LCSTR0),
	put_fact(saved,info,[CUI0,LUI0,[clinfo(Line0,CUI0,LUI0,SUI0,TS0,STT0,
					       STR0,SAB0,TTY0,CODE0,SRL0,LCSTR0)]]),
	repeat,
	   maybe_atom_gc(_,_),
	   ( erase_fact(saved,info,[CUI,LUI,CLInfoLines0]) ->
	     process_cui_lui(InputStream,OutputStream,CUI,LUI,CLInfoLines0),
	     fail
	   ; true
	   ),
	   !.


/* parse_line(+Line, -CUI, -LUI, -SUI, -TS, -STT, -STR, -SAB, -TTY, -CODE, -SRL )

parse_line/13 extracts CUI, ... from Line.  */

% N and LRL?
parse_line(Line, CUI, LUI, SUI, TS, STT, STR, SAB, TTY, CODE, SRL) :-
	% parse_record(Line,"|",[N,LRL]),
	parse_record(Line,"|",[CUI,_LAT,TS,LUI,STT,SUI,_ISPREF,_AUI,_SAUI,
			       _SCUI,_SDUI,SAB,TTY,CODE,STR,SRL,_SUPPRESS,_CVF]),
	!.
parse_line(Line, _, _, _, _, _, _, _, _, _, _, _, _) :-
	format('~NFatal error: Bad input ~s~n', [Line]),
	halt.


/* process_cui_lui(+InputStream, +OutputStream, +CUI, +LUI, +CLInfoLines)

   
   ***     WARNING     WARNING     WARNING     WARNING     WARNING     ***
   
      The current implementation processes according to CUI only.  This
      should have the effect of filtering out more strings at the expense
      of not necessarily having a representative for each LUI.
   
   ***     WARNING     WARNING     WARNING     WARNING     WARNING     ***

   
process_cui_lui/5 accumulates CLInfoLines (clinfo/14 terms) by
reading Line from InputStream, extracting some fields, forming the
lowercased string LCSTR with the same concept id (CUI) and
term id (LUI) as the input.  When a new CUI,LUI pair is encountered, the
accumulated information is filtered by marking certain strings as suppressible.
The information from the current line is saved for further processing.  */

process_cui_lui(InputStream,OutputStream,CUI0,LUI0,CLInfoLines0) :-
    repeat,
    (fget_non_null_line(InputStream,Line) ->
        parse_line(Line,CUI,LUI,SUI,TS,STT,STR,SAB,TTY,CODE,SRL),
%        LAT="ENG",   % limit to English   no need with mrconso.eng
	lower(STR,LCSTR),
        (CUI==CUI0 ->
            process_cui_lui(InputStream,OutputStream,CUI0,LUI0,
                            [clinfo(Line,CUI,LUI,SUI,TS,STT,STR,SAB,
				    TTY,CODE,SRL,LCSTR)|CLInfoLines0])
        ;   write_filtered(CLInfoLines0,OutputStream),
            put_fact(saved,info,[CUI,LUI,[clinfo(Line,CUI,LUI,SUI,TS,STT,STR,
						 SAB,TTY,CODE,SRL,LCSTR)]])
        )
    ;   write_filtered(CLInfoLines0,OutputStream)
    ),
    !.


/* write_filtered(+CLInfoLines, +OutputStream)
   write_filtered(+CLInfoLines, +LCStrings, +OutputStream)

write_filtered/2 either writes some CLInfoLines (that should be suppressed,
e.g., because they are associated with brand names) or
it writes modified CLInfoLines to OutputStream suppressing
entries according to the criteria:

  left prefixes of synonymous strings of the form <left prefix> - <rest>
  right suffixes of synonymous strings of the form <rest> - <right suffix>
  left prefixes of synonymous strings of the form <left prefix>: <rest>

All suppressed entries must have term status (TS) of S, i.e., they must
already be synonyms, not preferred forms. */

write_filtered(CLInfoLines0,OutputStream) :-
    rev(CLInfoLines0,CLInfoLines),
    extract_lc_strings(CLInfoLines,LCStrings),
    (control_option(brand_name_suppression) ->
        write_suppressed_brand_names(CLInfoLines,LCStrings,OutputStream)
    ;   write_filtered(CLInfoLines,LCStrings,OutputStream)
    ),
    !.

extract_lc_strings(CLInfoLines,LCStrings) :-
    extract_lc_strings_aux(CLInfoLines,LCStrings0),
    sort(LCStrings0,LCStrings),
    !.

extract_lc_strings_aux([],[]) :-
    !.
extract_lc_strings_aux([clinfo(_Line,_CUI,_LUI,_SUI,_TS,_STT,_STR,_SAB,_TTY,
			       _CODE,_SRL,LCSTR)|Rest],
		       [LCSTR|ExtractedRest]) :-
    extract_lc_strings_aux(Rest,ExtractedRest).
extract_lc_strings_aux([First|_Rest],[]) :-
    format('~NFatal error: Bad clinfo in extract_lc_strings_aux: ~p~n',[First]),
    !,
    halt.

write_filtered([],_,_) :-
    !.
write_filtered([clinfo(Line,CUI,LUI,SUI,TS,STT,STR,SAB,TTY,CODE,SRL,
		       LCSTR)|Rest],LCStrings,OutputStream) :-
    ((TS=="S",
      (control_option(filter_all_vocabularies) ->
          true
      ;   filterable_vocabulary(SAB)
      ),
      substring_match(LCSTR,LCStrings,MatchType,MatchingLCSTR)) ->
        (control_option(dump_prefilter_cases) ->
	     format(OutputStream,'~s|~s|~s|~s|~s~n',
		    [MatchType,SAB,CUI,MatchingLCSTR,LCSTR])
	;    format(OutputStream,'~s:~s:~s|~s|s|~s|~s|~s|~s|~s|~n',
		    [CUI,LUI,SUI,STT,STR,SAB,TTY,CODE,SRL])
	)
    ;   (control_option(dump_prefilter_cases) ->
            true
	;   format(OutputStream,'~s~n',[Line])
	)
    ),
    write_filtered(Rest,LCStrings,OutputStream).

substring_match(LCSTR,LCStrings,"prefix-hyphen",MatchingLCSTR) :-
    left_substring_match(LCStrings,LCSTR," - ",MatchingLCSTR),
    !.
substring_match(LCSTR,LCStrings,"prefix-colon",MatchingLCSTR) :-
    left_substring_match(LCStrings,LCSTR,": ",MatchingLCSTR),
    !.
substring_match(LCSTR,LCStrings,"hyphen-suffix",MatchingLCSTR) :-
    right_substring_match(LCStrings,LCSTR," - ",MatchingLCSTR),
    !.

left_substring_match([],_,_,_) :-
    !,
    fail.
left_substring_match([First|_Rest],LCSTR,Separator,First) :-
    append([LCSTR,Separator,_],First),
    !.
left_substring_match([_First|Rest],LCSTR,Separator,MatchingLCSTR) :-
    left_substring_match(Rest,LCSTR,Separator,MatchingLCSTR).

right_substring_match([],_,_,_) :-
    !,
    fail.
right_substring_match([First|_Rest],LCSTR,Separator,First) :-
    append([_,Separator,LCSTR],First),
    !.
right_substring_match([_First|Rest],LCSTR,Separator,MatchingLCSTR) :-
    right_substring_match(Rest,LCSTR,Separator,MatchingLCSTR).


/* filterable_vocabulary(?Vocabulary)

filterable_vocabulary/1 is a factual predicate that is updated each year
to specify those vocabularies that can be filtered by substring matching. */

% 2006 filterable vocabularies
filterable_vocabulary("dummy").


/* write_suppressed_brand_names(+CLInfoLines, +LCStrings, +OutputStream)

write_suppressed_brand_names/3 writes those CLInfoLines corresponding to
the short brand names of full names. For example, it will write the line
corresponding to the string "Cold" for "Cold brand of ...". */

write_suppressed_brand_names(CLInfoLines,LCStrings,OutputStream) :-
    filter_full_brand_names(LCStrings,LCFullBrandNames),
    (LCFullBrandNames==[] ->
        true
    ;   update_suppression_count('potential_brand_name'),
        write_suppressed_brand_names_aux(CLInfoLines,LCFullBrandNames,
					 OutputStream)
    ),
    !.

write_suppressed_brand_names_aux([],_,_) :-
    !.
write_suppressed_brand_names_aux([clinfo(Line,_CUI,_LUI,_SUI,_TS,_STT,_STR,
					 _SAB,_TTY,_CODE,_SRL,LCSTR)|Rest],
				 LCFullBrandNames,OutputStream) :-
    (brand_name_matches(LCFullBrandNames,LCSTR) ->
        update_suppression_count('brand_name'),
        format(OutputStream,'~s~n',[Line])
    ;   true
    ),
    write_suppressed_brand_names_aux(Rest,LCFullBrandNames,OutputStream).
write_suppressed_brand_names_aux([First|_Rest],_,_) :-
    format('~NFatal error: Bad clinfo in write_suppressed_brand_names_aux: ~p~n',
	   [First]),
    !,
    halt.

filter_full_brand_names([],[]) :-
    !.
filter_full_brand_names([First|Rest],[First|FilteredRest]) :-
    append([_," brand ",_],First),
    !,
    filter_full_brand_names(Rest,FilteredRest).
filter_full_brand_names([_First|Rest],FilteredRest) :-
    filter_full_brand_names(Rest,FilteredRest).

brand_name_matches([],_LCSTR) :-
    !,
    fail.
brand_name_matches([First|_Rest],LCSTR) :-
    append([LCSTR," brand ",_],First),
    !.
brand_name_matches([_First|Rest],LCSTR) :-
    brand_name_matches(Rest,LCSTR).


/* update_suppression_count(+Type)

update_suppression_count/1 updates the Type kind of suppression count. */

update_suppression_count(Type) :-
    (retract(suppression_count(Type,Count)) ->
        NewCount is Count + 1,
        assert(suppression_count(Type,NewCount))
    ;   assert(suppression_count(Type,1))
    ).


/* write_suppression_counts(+OutputStream)

write_suppression_counts/1 writes the values stored in suppression_count/2
to OutputStream. */

write_suppression_counts :-
    format('~nSuppression counts:~n',[]),
    suppression_count(Type,Count),
    format('~a: ~d~n',[Type,Count]),
    fail.
write_suppression_counts :-
    format('End.~n',[]).

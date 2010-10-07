% File:     mwi_utilities.pl
% Module:   MetaWordIndex Utilities
% Author:   Lan
% Purpose:  Provide support for filter_mrcon and glean_mrcon and other satellite programs


:- module(mwi_utilities,[
	compute_unique_filename/3,
	fget_non_null_line/2,
	generate_syntactic_analysis/3,
	normalize_meta_string/2,
	normalize_meta_string/3,
	parse_record/2,
	parse_record/3,
	remove_hyphens/2,
	remove_left_parentheticals/2,
	strip_possessives/2
    ]).

:- use_module(lexicon(lex_access),[
	assemble_definitions/2,
	tokenize_string_for_lexical_lookup/2
    ]).


:- use_module(metamap(metamap_tokenization),[
	ends_with_s/1,
	is_ws/1,
	is_ws_word/1,
	tokenize_text_utterly/2
    ]).

:- use_module(skr_db(db_access),[
	default_year/1
   ]).


:- use_module(skr_lib(consulttt),[
	consult_tagged_text/5
   ]).

:- use_module(skr_lib(generate_varinfo),[
	generate_variant_info/2
    ]).


:- use_module(skr_lib(mincoman),[
	minimal_commitment_analysis/5
    ]).

:- use_module(skr_lib(retokenize),[
	remove_null_atom_defns/2,
	retokenize/2
    ]).


:- use_module(skr_lib(nls_strings),[
	concatenate_items_to_atom/2,
	eliminate_multiple_meaning_designator_string/2,
	eliminate_nos_string/2,
	normalized_syntactic_uninvert_string/2,
	replace_all_substrings/4,
	split_string/4,
	syntactic_uninvert_string/2,
	trim_whitespace/2,
	trim_whitespace_left/2
    ]).

:- use_module(skr_lib(ctypes),[
	is_endfile/1
    ]).

:- use_module(skr_lib(nls_io),[
	fget_line/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	lower/2
    ]).

:- use_module(library(lists),[
	append/2
    ]).

:- use_module(library(process),[
	process_id/1
    ]).

/* parse_record(+String, -Fields)
   parse_record(+String, +Delim, -Fields)

parse_record/2 calls parse_record/3 with Delim "|" which splits String into
a list of Fields using Delim as the field delimiter.  */

parse_record(String,Fields) :-
    parse_record(String,"|",Fields).

parse_record([],_,[]) :-
    !.
parse_record(Line,Delim,[FirstField|RestFields]) :-
    split_string(Line,Delim,FirstField,RestLine),
    parse_record(RestLine,Delim,RestFields),
    !.
parse_record(Field,_,[Field]) :-
    !.


/* normalize_meta_string(+String, -NormalizedMetaString)
   normalize_meta_string(+String, -NormalizedMetaString, -NormalizationTypes)

normalize_meta_string/2 performs "normalization" on String to produce
NormalizedMetaString.  The purpose of normalization is to detect strings
which are effectively the same.  The normalization process (also called
lexical filtering) consists of the following steps:
      1. removal of (left []) parentheticals;
      2. removal of multiple meaning designators (<n>);
      3. NOS normalization;
      4. syntactic uninversion;
      5. conversion to lowercase;
      6. replacement of hyphens with spaces; and
      7. stripping of possessives.
Some right parentheticals used to be stripped, but no longer are.
Lexical Filtering Examples:
The concept "Abdomen" has strings "ABDOMEN" and "Abdomen, NOS".
Similarly, the concept "Lung Cancer" has string "Cancer, Lung".
And the concept "1,4-alpha-Glucan Branching Enzyme" has a string
"1,4 alpha Glucan Branching Enzyme".

Note that the order in which the various normalizations occur is important.
The above order is correct.
e.g., parentheticals must be removed before either lowercasing
or normalized syntactic uninversion (which includes NOS normalization)
are performed.

normalize_meta_string/3 also computes NormalizationTypes, a possibly empty
subset of {parenth, mult, nos, uninv, case, hyphen, poss}. */

normalize_meta_string(STR,NMSTR) :-
    remove_left_parentheticals(STR,PSTR),
    normalized_syntactic_uninvert_string(PSTR,UnPSTR),
    lower(UnPSTR,LCUnPSTR),
    remove_hyphens(LCUnPSTR,HLCUnPSTR),
    strip_possessives(HLCUnPSTR,NMSTR0),
    trim_whitespace(NMSTR0,NMSTR),
    % temp
    (split_string(NMSTR,"  ",_,_) ->
        format(user_output,
               'FATAL ERROR: Normalization ~s of ~s contains extra space.~n',
               [STR,NMSTR]),
        halt
    ;   true
    ).

normalize_meta_string(STR,NMSTR,NMTypes) :-
    remove_left_parentheticals(STR,PSTR),
    (STR==PSTR ->
        NMTypes0=[]
    ;   NMTypes0=[parenth]
    ),
    normalized_syntactic_uninvert_string(PSTR,UnPSTR,UnTypes),
    lower(UnPSTR,LCUnPSTR),
    (UnPSTR==LCUnPSTR ->
        NMTypes1=[]
    ;   NMTypes1=[case]
    ),
    remove_hyphens(LCUnPSTR,HLCUnPSTR),
    (LCUnPSTR==HLCUnPSTR ->
        NMTypes2=[]
    ;   NMTypes2=[hyphen]
    ),
    strip_possessives(HLCUnPSTR,NMSTR0),
    (HLCUnPSTR==NMSTR0 ->
        append([UnTypes,NMTypes0,NMTypes1,NMTypes2],NMTypes)
    ;   append([UnTypes,NMTypes0,NMTypes1,NMTypes2,[poss]],NMTypes)
    ),
    trim_whitespace(NMSTR0,NMSTR),
    % temp
    (split_string(NMSTR,"  ",_,_) ->
        format(user_output,
               'FATAL ERROR: Normalization ~s of ~s contains extra space.~n',
               [STR,NMSTR]),
        halt
    ;   true
    ).

generate_syntactic_analysis(ListOfAscii, TagList, SyntAnalysis) :-
%    format('~ngsa/3: ~s~nTagList=~p~n',[ListOfAscii,TagList]),
    once(tokenize_string_for_lexical_lookup(ListOfAscii,Words0)),
    retokenize(Words0,Words),
%    format('Words=~p~n',[Words]),
    %%% ( control_option(longest_lexicon_match) ->
    %%%   assemble_definitions(Words,Definitions0)
    %%% ; assemble_definitions_shortest(Words,Definitions0)
    %%% ),
    % FML 11/30/2006 Thursday @ 12:29:14
    % Hard-code longest lexical lookup
    assemble_definitions(Words,Definitions0),
    remove_null_atom_defns(Definitions0,Definitions),
%    format('Definitions=~p~n',[Definitions]),
    once(generate_variant_info(Definitions,VarInfoList)),
%    format('VarInfoList=~p~n',[VarInfoList]),
    consult_tagged_text(Definitions,VarInfoList,TagList,LabeledText,1),
%    format('LabeledText=~p~n',[LabeledText]),
    minimal_commitment_analysis(tag,Definitions,VarInfoList,
                                LabeledText,SyntAnalysis).
%    format('SyntAnalysis=~p~n',[SyntAnalysis]).

normalized_syntactic_uninvert_string(String,NormSUninvString,NormTypes) :-
    normalize_string(String,NormString,NormTypes0),
    syntactic_uninvert_string(NormString,NormSUninvString),
    (NormString==NormSUninvString ->
        NormTypes=NormTypes0
    ;   append(NormTypes0,[uninv],NormTypes)
    ).

/* remove_left_parentheticals(+String, -ModifiedString)

remove_left_parentheticals/2 removes all left parentheticals
(see left_parenthetical/1) from String. ModifiedString is what is left.  */

remove_left_parentheticals([0'[|Rest],ModifiedString) :-
    left_parenthetical(LP),
    append(LP,Right0,[0'[|Rest]),
    !,
    trim_whitespace_left(Right0,Right),
    remove_left_parentheticals(Right,ModifiedString).
remove_left_parentheticals(String,String).



/* left_parenthetical(?Parenthetical)

left_parenthetical/1 is factual predicates of left parentheticals.  The
data is derived from the 2000 Metathesaurus. See
.../metawordindex/data.00/05Parentheticals/ for details. */

left_parenthetical("[X]").
left_parenthetical("[V]").
left_parenthetical("[D]").
left_parenthetical("[M]").
left_parenthetical("[EDTA]").
left_parenthetical("[SO]").
left_parenthetical("[Q]").

/* remove_hyphens(+String, -ModifiedString)

remove_hyphens/2 removes hyphens from String and removes extra blanks
to produce ModifiedString. */

remove_hyphens(String,ModifiedString) :-
    replace_all_substrings(String,"-"," ",NoHyphenString),
    remove_extra_blanks(NoHyphenString,ModifiedString),
    !.

/* strip_possessives(+String, -StrippedString)

strip_possessives/2 tokenizes String, uses
metamap_tokenization:remove_possessives/2, and then rebuilds StrippedString. */

strip_possessives(String,StrippedString) :-
    tokenize_text_utterly(String,Tokens),
    remove_possessives(Tokens,StrippedTokens),
    (Tokens==StrippedTokens ->
        StrippedString=String
    ;   append(StrippedTokens,StrippedString)
    ).

/* remove_extra_blanks(+String, -ModifiedString)

remove_extra_blanks/2 replaces all occurrences of two consecutive blanks
with one.  */

remove_extra_blanks(String, ModifiedString) :-
	split_string(String, "  ", Left, Right),
	!,
	append([Left," ",Right], StringInOut),
	remove_extra_blanks(StringInOut, ModifiedString).
remove_extra_blanks(String, String).

/* fget_non_null_line(+Stream, -Line)

fget_non_null_line/2 reads lines from Stream until it encounters a non-null
one, Line.  It fails at end-of-file.  */

fget_non_null_line(Stream, Line) :-
	% \+ at_end_of_stream(Stream),
	peek_code(Stream, Code),
	% This needs to be =\= and not =:=!!
	Code =\= -1,
	!,
	fget_line(Stream, Line0),
	( Line0 == "" ->
	  fget_non_null_line(Stream, Line)
	; Line = Line0
	).
    
/* compute_unique_filename(+FileName, +Qualifier, -UniqueFileName)

compute_unique_filename/3 computes UniqueFileName, the concatenation of
FileName, Qualifier and PID (process ID).  */

compute_unique_filename(FileName, Qualifier, UniqueFileName) :-
	process_id(PID),
	concatenate_items_to_atom([FileName,".",Qualifier,".",PID], UniqueFileName).

/*    remove_possessives(+UTokensIn, -UTokensOut)

remove_possessives/2 filters out possessives
from the results of tokenize_text_utterly/2. */

remove_possessives([],[]).
% singular possessives
remove_possessives([Word,"'","s"],[Word]) :-
    is_ws_word(Word),
    !.
remove_possessives([Word,"'","s",WhiteSpace|Rest],
		   [Word,WhiteSpace|FilteredRest]) :-
    is_ws_word(Word),
    is_ws(WhiteSpace),
    !,
    remove_possessives(Rest,FilteredRest).
% plural possessives
remove_possessives([Word,"'"],[Word]) :-
    is_ws_word(Word),
    ends_with_s(Word),
    !.
remove_possessives([Word,"'",WhiteSpace|Rest],
		   [Word,WhiteSpace|FilteredRest]) :-
    is_ws_word(Word),
    ends_with_s(Word),
    is_ws(WhiteSpace),
    !,
    remove_possessives(Rest,FilteredRest).
remove_possessives([First|Rest],[First|FilteredRest]) :-
    remove_possessives(Rest,FilteredRest).

normalize_string(String,NormString,NormTypes) :-
    eliminate_multiple_meaning_designator_string(String,String1),
    (String==String1 ->
        NormTypes0=[]
    ;   NormTypes0=[mult]
    ),
    eliminate_nos_string(String1,NormString),
    (String1==NormString ->
        NormTypes=NormTypes0
    ;   append(NormTypes0,[nos],NormTypes)
    ).


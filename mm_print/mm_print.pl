% File:     mm_print.pl
% Module:   mm_print
% Author:   Lan
% Purpose:  Print MetaMap machine output.

:- module(mm_print,[
	go/0,
	go/1,
	go/2,
	stop_mm_print/0
   ]).

:- use_module(metamap(metamap_utilities),[
	build_concept_name_1/4,
	dump_evaluations_indented/7
    ]).

:- use_module(metamap(metamap_tokenization),[
	filter_tokens/3,
	get_phrase_item_name/2,
	get_phrase_item_feature/3,
	get_subitems_feature/3,
	linearize_components/2,
	parse_phrase_word_info/3
    ]).

:- use_module(metamap(metamap_evaluation),[
	extract_components/3
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	normalize_meta_string/2
    ]).

:- use_module(skr_db(db_access),[
	default_release/1
    ]).

:- use_module(skr_lib(negex), [
	generate_negex_output/1
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

:- use_module(skr_lib(nls_strings), [
	normalized_syntactic_uninvert_string/2,
	replace_all_substrings/4,
	split_string_completely/3
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_text), [
	concatenate_text/3
    ]).

:- use_module(skr_lib(nls_lists),[
	first_n_or_less/3
    ]).

:- use_module(skr_lib(semtype_translation_2012AB),[
	expand_semtypes/2,
	semtype_translation/2
    ]).

:- use_module(skr(skr_utilities),[
	skr_write_phrase/1
    ]).

:- use_module(skr(skr_xml),[
	generate_and_print_xml/1,
	conditionally_print_xml_footer/3,
	conditionally_print_xml_header/2,
	xml_output_format/1,
	xml_header_footer_print_setting/3
    ]).

:- use_module(skr_lib(ctypes),[
	is_alnum/1,
	is_newline/1,
	is_white/1
    ]).

:- use_module(skr_lib(nls_io),[
	fget_line/2
    ]).

:- use_module(skr_lib(print_chars)).

:- use_module(skr_lib(sicstus_utils),[
       	lower/2
    ]).

:- use_module(lexicon(lexical), [
	lowercase_list/2
    ]).

:- use_module(library(aggregate),[
	aggregate/3
    ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

:- use_module(library(lists),[
	append/2,
	rev/2
    ]).

:- use_module(library(sets),[
	intersect/2
    ]).

:- dynamic stop_phrase/1.
:- dynamic saved_results/1.

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    reset_control_options(mm_print),
    (initialize_mm_print(Options,Args,InterpretedArgs) ->
        (mm_print(InterpretedArgs); true)
    ;   usage
    ),
    (HaltOption == halt ->
        halt
    ;   true
    ).

initialize_mm_print(Options,Args,IArgs) :-
    retractall(stop_phrase(_)),
    retractall(saved_results(_)),
    get_control_options_for_modules([mm_print],AllOptions),
    interpret_options(Options,AllOptions,mm_print,IOptions),
    \+member(iopt(help,_),IOptions),
    ArgSpec=[aspec(infile,mandatory,file,read,no_default,
                           'Input file containing machine-readable MetaMap output'),
             aspec(outfile,mandatory,file,write,user_output,
                           'Output file')
            ],
    interpret_args(IOptions,ArgSpec,Args,IArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,IArgs),
    default_release(Release),
    display_current_control_options(mm_print, Release),
    initialize_mm_print,
    !.

initialize_mm_print.

stop_mm_print.

usage :-
	format('~nUsage: mm_print [<options>] <infile> [<outfile>]~n~n', []),
	format('  <infile> contains machine-readable MetaMap output~n', []),
	format('  and <outfile> is a file for results (default is user_output).~n', []),
	display_control_options_for_modules(mm_print, []).


mm_print(InterpretedArgs) :-
	( get_from_iargs(stop_phrase_file, stream, InterpretedArgs, StopPhraseStream) ->
	  read_stop_phrases(StopPhraseStream),
	  close(StopPhraseStream)
	; true
	),
	get_from_iargs(infile, name, InterpretedArgs, InputFile),
	get_from_iargs(infile, stream, InterpretedArgs, InputStream),
	get_from_iargs(outfile, name, InterpretedArgs, OutputFile),
	get_from_iargs(outfile, stream, InterpretedArgs, OutputStream),
	% begin by reading in the first term from the MMO file,
	% which should be an args/2 term
	get_args_term(InputStream, ArgsTerm),
	format('~nProcessing ~a --> ~a~n~n', [InputFile,OutputFile]),
	process_all_documents(InputStream, ArgsTerm, OutputStream),
	close(OutputStream),
	close(InputStream).

% ArgsTerm will be the initial args/2 term in the MMO file
process_all_documents(InputStream, ArgsTerm, OutputStream) :-
	% 1 means outer XML header/footer
	xml_header_footer_print_setting(1, XMLSetting, PrintSetting),
	conditionally_print_xml_header(PrintSetting, OutputStream),
	process_one_document(InputStream, OutputStream, ArgsTerm, NextTerm),
	% NextTerm will be either
	% * end_of_file, if we've read to EOF, or
	% * 'EOT', if the input file was generated with -E
	% * the args/2 term from the MMO for the next document, if we're not at EOF.
	process_rest_documents(InputStream, NextTerm, OutputStream),
	conditionally_print_xml_footer(PrintSetting, XMLSetting, OutputStream).

process_one_document(InputStream, OutputStream, PrevTerm, NextTerm) :-
	get_header_terms(InputStream, PrevTerm, ArgsTerm, MMOAAsTerm, MMONegExTerm),
	MMONegExTerm = neg_list(MMONegations),
	generate_negex_output(MMONegations),
	get_all_utterances(InputStream, OutputStream, Utterances, NextTerm),
	convert_all_utterances_to_orig_MMO(Utterances, OriginalUtteranceMMO, []),
	AllMMO = [ArgsTerm,MMOAAsTerm,MMONegExTerm|OriginalUtteranceMMO],
        current_output(CurrentOutputStream),
        set_output(OutputStream),
	generate_and_print_xml(AllMMO),
        set_output(CurrentOutputStream).

% If we're at EOF, then quit; otherwise process a document, and recurse.
process_rest_documents(InputStream, PrevTerm, OutputStream) :-
	( PrevTerm == end_of_file ->
	  true
	; process_one_document(InputStream, OutputStream, PrevTerm, NextTerm),
	  process_rest_documents(InputStream, NextTerm, OutputStream)
	).

% convert from terms of the form
%    from utterance(Label,String,Phrases,PosInfo,ReplPos)
% in which all the phrases, candidates, and mappings are in the Phrases term
% to the original MMO generated by MetaMap, i.e.,
% one utterance term followed by one or more sequences of
% a phrase term followed by a candidates term followed by a mappings term
convert_all_utterances_to_orig_MMO([], OrigMMO, OrigMMO).
convert_all_utterances_to_orig_MMO([FirstUtterance|RestUtterances], OrigMMOIn, OrigMMOOut) :-
	convert_one_utterance_to_orig_MMO(FirstUtterance, OrigMMOIn, OrigMMONext),
	convert_all_utterances_to_orig_MMO(RestUtterances, OrigMMONext, OrigMMOOut).

convert_one_utterance_to_orig_MMO(utterance(Label,String,Phrases,PosInfo,ReplPos),
				  OrigMMOIn, OrigMMOOut) :-
	OrigMMOIn = [utterance(Label,String,PosInfo,ReplPos)|OrigMMONext],
	convert_all_phrases_to_orig_MMO(Phrases, OrigMMONext, OrigMMOOut).	

convert_all_phrases_to_orig_MMO([], OrigMMO, OrigMMO).
convert_all_phrases_to_orig_MMO([FirstPhrase|RestPhrases],
		     [FirstOrigPhrase,Candidates,Mappings|RestOrigPhrases],
		     RestOrigMMO) :-
	FirstPhrase = phrase(Text,Syntax,Candidates,Mappings,PhrasePosInfo,ReplPos),
	FirstOrigPhrase = phrase(Text,Syntax,PhrasePosInfo,ReplPos),
	convert_all_phrases_to_orig_MMO(RestPhrases, RestOrigPhrases, RestOrigMMO).

get_all_utterances(InputStream, OutputStream, [Utterance|RestUtterances], NextTerm) :-
	get_one_utterance(InputStream, OutputStream, Utterance),
	get_rest_utterances(InputStream, OutputStream, RestUtterances, NextTerm).

get_rest_utterances(InputStream, OutputStream, Utterances, NextTerm) :-
	% ( at_end_of_stream(InputStream) ->
	% ( peek_code(InputStream, Code),
    	( peek_past_whitespace(InputStream, Code),
	  Code =:= -1 ->
	  NextTerm = end_of_file,
	  Utterances = []
	; get_one_utterance(InputStream, OutputStream, Utterance),
	  ( Utterance = args(_,_) ->
	    Utterances = [],
	    NextTerm = Utterance
	  ; Utterance = 'EOT' ->
	    Utterances = [],
	    NextTerm = Utterance
	  ; Utterances = [Utterance|RestUtterances],
	    get_rest_utterances(InputStream, OutputStream, RestUtterances, NextTerm)
	  )
	).

peek_past_whitespace(InputStream, Code) :-
	peek_code(InputStream, NextCode),
	( is_whitespace(NextCode) ->
	  get_code(InputStream, NextCode),
	  peek_past_whitespace(InputStream, Code)
	; Code is NextCode
	).	    

is_whitespace(Code) :-
	( is_white(Code) ->
	  true
	; is_newline(Code)
	).

read_stop_phrases(StopPhraseStream) :-
	repeat,
	( fget_line(StopPhraseStream, Line) ->
	  atom_codes(StopPhrase, Line),
	  assert(stop_phrase(StopPhrase)),
	  fail
	; true
	).

get_header_terms(InputStream, PrevTerm, ArgsTerm, AAsTerm, NegExTerm) :-
	( PrevTerm == 'EOT' ->
	  get_args_term(InputStream, ArgsTerm)
	; ArgsTerm = PrevTerm
	),
	get_aas_term(InputStream, AAsTerm),
	get_negex_term(InputStream, NegExTerm).

get_args_term(InputStream, ArgsTerm) :-
	fread_term(InputStream, ArgsTerm),
	( ArgsTerm = args(_,_) ->
	  true
	; format('~NERROR: get_args_term/3 expected an args/3 term; found~n~p~n', [ArgsTerm]),
	  fail
	).

get_aas_term(InputStream, AAsTerm) :-
	fread_term(InputStream, AAsTerm),
	( AAsTerm = aas(_) ->
	  true
	; format('~NERROR: get_aas_term/3 expected an aas/1 term; found~n~p~n', [AAsTerm]),
	  fail
	).

get_negex_term(InputStream, NegExTerm) :-
	fread_term(InputStream, NegExTerm),
	( NegExTerm = neg_list(_) ->
	  true
	; format('~NERROR: get_negex_term/3 expected a neg_list/1 term; found~n~p~n', [NegExTerm]),
	  fail
	).

% process_utterances(InputStream, OutputStream) :-
% 	control_option('94_filter'),
% 	!,
% 	filter_utterances(InputStream, OutputStream).

get_one_utterance(InputStream, OutputStream, Utterance) :-
	maybe_atom_gc(_,_),
	get_utterance(InputStream, _Label, Utterance),
	( Utterance = utterance(_Label,_String,_Phrases,_PosInfo,_ReplPos) ->
	  print_utterance(Utterance, OutputStream),
	  do_organize_semantic_types(OutputStream)
	; Utterance = args(_,_) ->
	  true
	; Utterance = 'EOT' ->
	  true
	).

/* get_utterance(+InputStream, ?TagLabel, -Utterance)
   get_phrases(+InputStream, -Phrases)

get_utterance/3 reads from InputStream until it finds Utterance with TagLabel.
Input is machine-readable MetaMap output and consists of a sequence of:
     one utterance/2 term,
     one or more sequences of a phrase/2, candidates/1 and mappings/1 terms, and
     one 'EOU'/0 term.
Utterance is of the form
     utterance(Label,String,Phrases) where
     Label is an atom,
     Text is a string, and
     Phrases is a list of the form
             phrase(Text,Syntax,Candidates,Mappings,StartPos/Length,ReplPos).
See MetaMap documentation for further information.
get_phrases/2 is an auxiliary predicate.  */

get_utterance(InputStream, TagLabel, Term) :-
	( fread_term(InputStream, NextTerm) ->
	  ( NextTerm = utterance(Label,String,PosInfo,ReplPos) ->
	    get_phrases(InputStream, String, Phrases),
	    verify_non_null_phrases(Phrases, Label),
	    Label = TagLabel,
	    Term = utterance(Label,String,Phrases,PosInfo,ReplPos)
	  ; NextTerm = args(_,_) ->
	    Term = NextTerm
	  ; NextTerm = 'EOT' ->
	    Term = NextTerm
	  ; format('~NERROR: get_utterance returned neither utterance/5 nor args/2 nor EOT term:~n~p~n',
		   [NextTerm]),
	    !,
	    fail
	  )
	; complain_no_utterance_term(TagLabel)
	).

get_phrase_term(InputStream, UtteranceText, PhraseTerm) :-
	fread_term(InputStream, PhraseTerm),
	( PhraseTerm = phrase(_PhraseText,_Syntax,_PosInfo,_ReplPos) ->
	  true
	; PhraseTerm == 'EOU' ->
	  true
	; format('~NERROR: (get_phrase_term/3) Missing phrase for "~p".~n', [UtteranceText]),
	  fail
	).

get_candidates(InputStream, UtteranceText, CandidatesTerm) :-
	( fread_term(InputStream, CandidatesTerm),
	  CandidatesTerm = candidates(_TotalCandidateCount,
				      _ExcludedCandidateCount,_PrunedCandidateCount,
				      _RemainingCandidateCount,_CandidateList) ->
	  true
	; format('~NERROR: (get_candidates_term/3) Missing candidates for "~p".~n', [UtteranceText]),
	  fail
	).

get_mappings(InputStream, UtteranceText, mappings(Mappings)) :-
	( fread_term(InputStream, mappings(Mappings)) ->
	  true
	; format('~NERROR: (get_mappings_term/3) Missing mappings for "~p".~n', [UtteranceText]),
	  fail
	).

get_phrases(InputStream, UtteranceText, Phrases) :-
	get_phrase_term(InputStream, UtteranceText, PhraseTerm),
	( PhraseTerm == 'EOU' ->
	  Phrases = []
	; PhraseTerm = phrase(PhraseText,Syntax,PosInfo,ReplPos) ->
	  get_candidates(InputStream, UtteranceText, CandidatesTerm),
	  get_mappings(InputStream, UtteranceText, MappingsTerm),
	  % splice the candidates and mappings into the new phrase/5 term.
	  Phrases = [phrase(PhraseText,Syntax,CandidatesTerm,MappingsTerm,PosInfo,ReplPos)|RestPhrases],
	  get_phrases(InputStream, UtteranceText, RestPhrases)
	; format('~NERROR: (get_phrases/3) Missing mappings for "~p".~n', [UtteranceText]),
	  fail
	).

verify_non_null_phrases(Phrases, Label) :-
	( Phrases == [] ->
	  format('~NERROR: (get_utterance/3) No phrases found for ~p.~n', [Label]),
	  !,
	  fail
	; true
	).

% If complain_no_utterance_term/1 is called,
% it means that fread_term(InputStream, UttTerm) failed.
complain_no_utterance_term(TagLabel) :-
	( var(TagLabel) ->
	  true
	; format('~NERROR: (get_utterance/3) Cannot find utterance for ~p.~n',[TagLabel])
	),
	!,
	fail.

do_organize_semantic_types(OutputStream) :-
	( control_option(organize_semantic_types) ->
	  ( saved_results(STsConcepts) ->
            format(OutputStream, '~n~n~nCombined concepts by semantic type:~n', []),
            % format(OutputStream,'~p~n',[STsConcepts]),
	    write_sts_concepts(STsConcepts, null, OutputStream)
	  ; true
	  )
	; true
	).

print_utterance(_Utterance, _OutputStream) :-
	xml_output_format(_XMLOutputFormat),
	!.
print_utterance(utterance(Label,String,_Phrases,_PosInfo,_ReplPos), OutputStream) :-
	control_option(label_text_field_dump),
	!,
	format(OutputStream, '~a|~s~n', [Label,String]).
print_utterance(utterance(Label,String,Phrases0,_PosInfo,_ReplPos), OutputStream) :-
	do_filter_out_01(Phrases0),
	do_null_only(Phrases0),
	do_non_null_only(Phrases0),
	do_default(OutputStream, Label, String),
	do_alnum_filter(Phrases0, Phrases),
	do_mapping_summary_dump_1(OutputStream, Label, String),
	print_phrases(Phrases, String, Label, OutputStream),
	do_mapping_summary_dump_2(OutputStream),
	do_organize_semantic_types(Phrases, OutputStream).

do_filter_out_01(Phrases0) :-
	( control_option(filter_out_01) ->
	  \+ satisfy_01(Phrases0)
	; true
	).

do_null_only(Phrases0) :-
	( control_option(null_only) ->
	  contains_null(Phrases0)
	; true
	).

do_non_null_only(Phrases0) :-
	( control_option(non_null_only) ->
	  contains_non_null(Phrases0)
	; true
	).

do_default(OutputStream, Label, String) :-
	( ( control_option(not_in_lex_dump)
	  ; control_option(syntax_dump)
	  ; control_option(syntactic_pattern_dump)
	  ; control_option(potential_stopphrase_dump)
	  ; control_option(phrase_dump)
	  ; control_option(prepositional_phrase_dump)
	  ; control_option(inputmatch_lexmatch_dump)
	  ; control_option(candidate_count_dump)
	  ; control_option(candidate_text_dump)
	  ; control_option(mapping_summary_dump)
	  ; control_option(mapping_text_dump)
	  ; control_option(unique_mapping_text_dump)
	  ; control_option(mapping_count_dump)
	  ; control_option(non_monotonic_mapping_dump)
	  ; control_option(odd_mapping_dump)
	  ; control_option(filter_for_meta)
	  ; control_option(filter_for_meta_with_sy_id)
	  ; control_option(phrase_specificity_dump)
	  ) -> true
	; format(OutputStream, 'Processing ~a: ~s~n', [Label,String])
	).

do_alnum_filter(Phrases0, Phrases) :-
	( control_option(alnum_filter) ->
	  filter_alnum_phrases(Phrases0, Phrases)
	; Phrases = Phrases0
	).

do_mapping_summary_dump_1(OutputStream, Label, String) :-
	( control_option(mapping_summary_dump) ->
	  format(OutputStream, '~p|0|0|0|~s~n', [Label,String])
	; true
	).


do_mapping_summary_dump_2(OutputStream) :-
	( control_option(mapping_summary_dump) ->
	  format(OutputStream, '~n', [])
	; true
	).

do_organize_semantic_types(Phrases, OutputStream) :-
	( control_option(organize_semantic_types) ->
          organize_semantic_types(Phrases,OutputStream)
	; true
	).



/* filter_alnum_phrases(+Phrases, -FilteredPhrases)

filter_alnum_phrases/2 filters out phrases which consist entirely of
non-alphanumeric characters, e.g., ')', ').' or ':'. */

% filter_alnum_phrases([],[]).
% filter_alnum_phrases([phrase(Text,Syntax,Candidates,Mappings,PosInfo,ReplPos)|Rest],
% 		     [phrase(Text,Syntax,Candidates,Mappings,PosInfo,ReplPos)
% 		     |FilteredRest]) :-
%     atom_codes(Text,String),
%     string_contains_alnum(String),
%     !,
%     filter_alnum_phrases(Rest,FilteredRest).
% phrase/4 retrofit
filter_alnum_phrases([], []).
filter_alnum_phrases([phrase(Text,Syntax,Candidates,Mappings,PosInfo,ReplPos)|Rest],
		     [phrase(Text,Syntax,Candidates,Mappings,PosInfo,ReplPos)|FilteredRest]) :-
    atom_codes(Text,String),
    string_contains_alnum(String),
    !,
    filter_alnum_phrases(Rest,FilteredRest).
filter_alnum_phrases([_First|Rest],FilteredRest) :-
    !,
    filter_alnum_phrases(Rest,FilteredRest).

print_phrases([],_,_,_).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(not_in_lex_dump),
    !,
    Phrase = phrase(Text,Syntax,_Candidates,_Mappings,_PosInfo,_ReplPos),
    print_not_in_lex(Syntax,Text,String,Label,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(syntax_dump),
    !,
    Phrase = phrase(_Text,Syntax,_Candidates,_Mappings,_PosInfo,_ReplPos),
    print_syntax(Syntax,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(syntactic_pattern_dump),
    !,
    Phrase = phrase(Text,Syntax,_Candidates,_Mappings,_PosInfo,_ReplPos),
    print_syntactic_pattern(Syntax,Text,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(potential_stopphrase_dump),
    !,
    Phrase = phrase(Text,Syntax,Candidates,_Mappings,_PosInfo,_ReplPos),
    (Candidates = candidates(_TotalCandidateCount,_ExcludedCandidateCount,
			     _PrunedCandidateCount,_RemainingCandidateCount,[]) ->

        lower(Text,LCText),
% oh yes it does; cf. "dose"	
% syntactic tagging didn't make enough difference to warrant its use
	extract_syntactic_tags(Syntax,STags),
        format(OutputStream,'~a|~p~n',[LCText,STags])
%        format(OutputStream,'~a~n',[LCText])
    ;   true
    ),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(phrase_dump),
    !,
    Phrase = phrase(Text,_Syntax,_Candidates,_Mappings,_PosInfo,_ReplPos),
    lower(Text,LCText),
    format(OutputStream,'~a~n',[LCText]),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([First|Rest],String,Label,OutputStream) :-
    control_option(prepositional_phrase_dump),
    !,
    (is_composite_phrase([First|Rest],CompositePhrase,NewRest) ->
	dump_composite_phrase(CompositePhrase,OutputStream)
    ;   NewRest=Rest
    ),
    print_phrases(NewRest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(inputmatch_lexmatch_dump),
    !,
    Phrase = phrase(Text,Syntax,_Candidates,_Mappings,_PosInfo,_ReplPos),
    print_match_diffs(Syntax,Text,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(candidate_count_dump),
    !,
    Phrase = phrase(_Text,_Syntax,Candidates,_Mappings,_PosInfo,_ReplPos),
    length(Candidates,NC),
    format(OutputStream,'~d~n',[NC]),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(candidate_text_dump),
    !,
    Phrase = phrase(_Text,Syntax,Candidates,_Mappings,_PosInfo,_ReplPos),
    print_candidate_text(Candidates,Label,Syntax,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(mapping_summary_dump),
    !,
    Phrase = phrase(_Text,Syntax,_Candidates,Mappings,_PosInfo,_ReplPos),
    print_mapping_summary(Mappings,Label,String,Syntax,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(mapping_text_dump),
    !,
    Phrase = phrase(_Text,Syntax,_Candidates,Mappings,_PosInfo,_ReplPos),
    print_mapping_text(Mappings,Label,Syntax,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(unique_mapping_text_dump),
    !,
    Phrase = phrase(_Text,Syntax,_Candidates,Mappings,_PosInfo,_ReplPos),
    print_unique_mapping_text(Mappings,Label,Syntax,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(mapping_count_dump),
    !,
    Phrase = phrase(_Text,_Syntax,_Candidates,Mappings,_PosInfo,_ReplPos),
    length(Mappings,NM),
    format(OutputStream,'~d~n',[NM]),
%    this counts the length of each mapping, not the number of mappings
%    print_mapping_counts(Mappings,Label,OutputStream),
    print_phrases(Rest,String,Label,OutputStream).
% print_phrases([Phrase|Rest],String,Label,OutputStream) :-
%     control_option(non_monotonic_mapping_dump),
%     !,
%     Phrase = phrase(Text,_Syntax,Candidates,Mappings,_PosInfo,_ReplPos),
%     (( Candidates=[ev(NegBestC,_,_,_,_,_,_,_,_,_Sources,_PosInfo)|_],
%       Mappings=[map(NegBestM,_)|_],
%       NegBestM>NegBestC) ->
% 	format(OutputStream,'~nNon-monotonic mapping for~n~p ~p~n',[Label,Text]),
% %	print_evaluations(Candidates,'Candidates',OutputStream),
% 	dump_evaluations_indented(Candidates,'Candidates',OutputStream),
% 	print_mappings(Mappings,'Mappings',OutputStream)
%     ;   true
%     ),
%     print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    control_option(odd_mapping_dump),
    !,
    Phrase = phrase(Text,_Syntax,Candidates,Mappings,_PosInfo,_ReplPos),
    length(Candidates,NC),
    length(Mappings,NM),
    ((NM=:=0, NC =\=0) ->
        format(OutputStream,'~nNo mappings for~n~p~n~p~n~p~n',
	       [Label,Text,Candidates])
    ;   true
    ),
    print_phrases(Rest,String,Label,OutputStream).
%print_phrases([phrase(Text,_Syntax,Candidates0,_Mappings,_PosInfo,_ReplPos)|Rest],String,Label,
%              OutputStream) :-
%    control_option(filter_for_meta),
%    !,
%    lower(Text,LCText),
%    \+stop_phrase(LCText),
%    (control_option(threshold) ->
%        filter_evaluations_by_threshold(Candidates0,Candidates1)
%    ;   Candidates1=Candidates0
%    ),
%    filter_out_duplicate_concepts(Candidates1,Candidates2),
%    atom_codes(Atom,String),
%    split_text(Label,'.',CUI,_),
%    filter_out_input_concept(Candidates2,Atom,CUI,Candidates),
%    (Candidates == [] ->
%        true
%    ;   (Label == '' ->
%            true
%        ;   format(OutputStream,'~n~n~s~n',[String])
%        ),
%        print_evaluations_briefly(Candidates,OutputStream)
%    ),
%    !,
%    print_phrases(Rest,String,'',OutputStream).
%print_phrases([phrase(Text,_Syntax,Candidates0,_Mappings,_PosInfo,_ReplPos)|Rest],String,Label,
%              OutputStream) :-
%    control_option(filter_for_meta_with_sy_id),
%    !,
%    lower(Text,LCText),
%    \+stop_phrase(LCText),
%    (control_option(threshold) ->
%        filter_evaluations_by_threshold(Candidates0,Candidates1)
%    ;   Candidates1=Candidates0
%    ),
%    filter_out_duplicate_concepts(Candidates1,Candidates2),
%    atom_codes(Atom,String),
%    compute_meta_cuis(Atom,CUIs),
%    filter_out_input_concept2(Candidates2,Atom,CUIs,Candidates),
%    split_text(Label,'.',SY_ID,RestLabel),
%    split_text(RestLabel,'.',Termgroup,_),
%    (Candidates == [] ->
%        true
%    ;   (Label == '' ->
%            true
%        ;   format(OutputStream,'~n~n~a|~a|~s~n',[SY_ID,Termgroup,String])
%        ),
%        print_evaluations_briefly(Candidates,OutputStream)
%    ),
%    !,
%    print_phrases(Rest,String,'',OutputStream).
%print_phrases([phrase(Text,_Syntax,_Candidates,Mappings,_PosInfo,_ReplPos)|Rest],String,Label,
%              OutputStream) :-
%    control_option(phrase_specificity_dump),
%    !,
%    (atom_codes(Text,String) ->
%        true
%    ;   format('WARNING: "~a" does not exhaust ~a.~n',
%               [Text,Label])
%    ),
%    print_phrase_specificity(Mappings,Text,Label,OutputStream),
%    print_phrases(Rest,String,Label,OutputStream).
print_phrases([Phrase|Rest],String,Label,OutputStream) :-
    Phrase = phrase(Text,Syntax,Candidates0,Mappings0,_PosInfo,_ReplPos),

    Candidates0 = candidates(TotalCandidateCount,
			     ExcludedCandidateCount,PrunedCandidateCount,
			     RemainingCandidateCount,CandidateList),
    lower(Text,LCText),
    \+stop_phrase(LCText),
    (control_option(filter_out_01) ->
        \+satisfy_01(Candidates0,Text)
    ;   true
    ),
    (control_option(null_only) ->
        Candidates0 == []
    ;   true
    ),
    (control_option(non_null_only) ->
        Candidates0 \== []
    ;   true
    ),
    (control_option(simple_syntax) ->
        format(OutputStream,'~nPhrase: "~a"~n',[Text])
    ;   true
    ),
    (control_option(syntax) ->
	format(OutputStream,'~n',[]),
        current_output(CurrentOutput),
        set_output(OutputStream),
	format('msu~n',[]),
        skr_write_phrase(Syntax),
        set_output(CurrentOutput)
    ;   true
    ),
    (control_option(candidates) ->
        (control_option(threshold) ->
            filter_evaluations_by_threshold(Candidates0,Candidates1)
        ;   Candidates1=Candidates0
        ),
	(control_option(truncate_output) ->
	    truncate_list(Candidates1,Candidates,10,NTruncatedC)
	;   Candidates=Candidates1,
	    NTruncatedC=0
	),
	(Candidates == [] ->
	    true
        ;   dump_evaluations_indented(CandidateList,TotalCandidateCount,
				      ExcludedCandidateCount, PrunedCandidateCount,
				      RemainingCandidateCount, 'Candidates', OutputStream),
	    (NTruncatedC=:=0 ->
	        true
	    ;   format(OutputStream,'     [~d candidates were not printed]~n',
		       [NTruncatedC])
	    )
        )
    ;   true
    ),
    (control_option(mappings) ->
	(control_option(truncate_output) ->
	    truncate_list(Mappings0,Mappings,2,NTruncatedM)
	;   Mappings=Mappings0,
	    NTruncatedM=0
	),
        (Mappings == [] ->
	    true
        ;   print_mappings(Mappings,'Mapping',OutputStream),
	    (NTruncatedM=:=0 ->
	        true
	    ;   format(OutputStream,'     [~d mappings were not printed]~n',
		       [NTruncatedM])
	    )
	)
    ;   true
    ),
    !,
    print_phrases(Rest,String,Label,OutputStream).
% go on in case of failure
print_phrases([_|Rest],String,Label,OutputStream) :-
    print_phrases(Rest,String,Label,OutputStream).

truncate_list(List,TruncatedList,MaxN,NTruncated) :-
    length(List,N),
    (MaxN < N ->
        NTruncated is N - MaxN,
	first_n_or_less(List,MaxN,TruncatedList)
    ;   TruncatedList=List,
	NTruncated=0
    ),
    !.


satisfy_01([]).
satisfy_01([Phrase|Rest]) :-
    Phrase = phrase(Text,_Syntax,Candidates,_Mappings,_PosInfo,_ReplPos),
    satisfy_01(Candidates,Text),
    satisfy_01(Rest).

satisfy_01([],_Text).
satisfy_01([_,_|_],_Text) :-
    !,
    fail.
satisfy_01([ev(_NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,_SemTypes,
              _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo)],
          Text) :-
    (case_equal(Text,MetaTerm); case_equal(Text,MetaConcept)),
    !.
satisfy_01([ev(_NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,_SemTypes,
              _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo)],
          Text) :-
    normalize_meta_text(Text,NMText),
    normalize_meta_text(MetaTerm,NMMetaTerm),
    (NMText == NMMetaTerm ->
        true
    ;   normalize_meta_text(MetaConcept,NMMetaConcept),
        NMText == NMMetaConcept
    ),
    !.

case_equal(Text1,Text1) :-
    !.
case_equal(Text1,Text2) :-
    lower(Text1,LCText1),
    lower(Text2,LCText1),
    !.

contains_null([]) :-
    !,
    fail.
contains_null([Phrase|_Rest]) :-
    Phrase = phrase(_Text,_Syntax,[],_Mappings,_PosInfo,_ReplPos),
    !.
contains_null([_|Rest]) :-
    contains_null(Rest).

contains_non_null([]) :-
    !,
    fail.
contains_non_null([Phrase|_Rest]) :-
    Phrase = phrase(_Text,_Syntax,Candidates,_Mappings,_PosInfo,_ReplPos),
    Candidates \== [],
    !.
contains_non_null([_|Rest]) :-
    contains_non_null(Rest).


print_not_in_lex([],_,_,_,_).
print_not_in_lex([not_in_lex(Subitems)|Rest],_Text,String,Label,OutputStream) :-
    !,
    get_subitems_feature(Subitems,inputmatch,InputMatch0),
    concatenate_text(InputMatch0,' ',InputMatch),
    get_subitems_feature(Subitems,tokens,Tokens0),
    concatenate_text(Tokens0,' ',Tokens),
    format(OutputStream,'~a|~a|~s|~a|~n',
           [Tokens,InputMatch,String,Label]),
    print_not_in_lex(Rest,_Text,String,Label,OutputStream).
print_not_in_lex([_First|Rest],_Text,String,Label,OutputStream) :-
    print_not_in_lex(Rest,_Text,String,Label,OutputStream).


print_syntax([],_).
print_syntax([First|Rest],OutputStream) :-
    get_phrase_item_name(First,Category),
    get_phrase_item_feature(First,tokens,Tokens),
    format(OutputStream,'~a:~p~n',[Category,Tokens]),
    print_syntax(Rest,OutputStream).


extract_syntactic_tags([],[]).
extract_syntactic_tags([First|Rest],[FirstSTag|RestSTags]) :-
    !,
    functor(First,FirstSTag,_Arity),
    extract_syntactic_tags(Rest,RestSTags).


print_syntactic_pattern([],_,_).
print_syntactic_pattern([Singleton],Text,OutputStream) :-
    !,
    functor(Singleton,Category,_Arity),
    (control_option(with_text) ->
        format(OutputStream,'~a:~a~n',[Category,Text])
    ;   format(OutputStream,'~a~n',[Category])
    ).
print_syntactic_pattern([First|Rest],Text,OutputStream) :-
    functor(First,Category,_Arity),
    format(OutputStream,'~a|',[Category]),
    print_syntactic_pattern(Rest,Text,OutputStream).


% filter_utterances(InputStream, OutputStream) :-
% 	repeat,
% 	   maybe_atom_gc(_,_),
% 	   ( fget_line(InputStream,Line) ->
% 	     ( append("Phrase: ",Phrase,Line),
% 	       is_stop_phrase_string(Phrase) ->
% 	       skip_phrase(InputStream)
% 	     ; format(OutputStream,'~s~n',[Line])
% 	     ),
% 	    fail
% 	   ; true
% 	   ).
% 
% is_stop_phrase_string(String) :-
% 	atom_codes(Atom, String),
% 	lower(Atom, LCAtom),
% 	stop_phrase(LCAtom),
% 	!.
% 
% skip_phrase(InputStream) :-
% 	repeat,
% 	( fget_line(InputStream, Line) ->
% 	  Line == ""
% 	; true
% 	),
% 	!.
% 

print_match_diffs([],_,_).
print_match_diffs([First|Rest],Text,OutputStream) :-
    arg(1,First,Subitems),
    ((get_subitems_feature(Subitems,inputmatch,InputMatch),
      get_subitems_feature(Subitems,lexmatch,LexMatch),
      LexMatch \== []) ->
        lowercase_list(InputMatch,LCInputMatch),
        lowercase_list(LexMatch,LCLexMatch),
        (LCInputMatch == LCLexMatch ->
            true
        ;   format(OutputStream,'~p|~p|~p|~p~n',[InputMatch,LexMatch,First,Text])
        )
    ;    true
    ),
    print_match_diffs(Rest,Text,OutputStream).


/* filter_evaluations_by_threshold(+Evaluations, -FilteredEvaluations)
   filter_evaluations_by_threshold(+Evaluations, +Threshold,
                                   -FilteredEvaluations)

filter_evaluations_by_threshold/2 retains only those Evaluations with
value Threshold or better.  */

filter_evaluations_by_threshold(Evaluations,FilteredEvaluations) :-
    control_value(threshold,Threshold),
    NegThreshold is -Threshold,
    filter_evaluations_by_threshold(Evaluations,NegThreshold,
                                    FilteredEvaluations).

filter_evaluations_by_threshold([],_,[]).
filter_evaluations_by_threshold([First|_Rest],NegThreshold,[]) :-
    First=ev(NegValue,_,_,_,_,_,_,_,_,_Sources,_PosInfo),
    NegValue > NegThreshold,
    !.
filter_evaluations_by_threshold([First|Rest],NegThreshold,
                                [First|FilteredRest]) :-
    filter_evaluations_by_threshold(Rest,NegThreshold,FilteredRest).


/* filter_out_duplicate_concepts(+Evaluations, -FilteredEvaluations)

filter_out_duplicate_concepts/2
*/

filter_out_duplicate_concepts([],[]) :-
    !.
filter_out_duplicate_concepts(Evaluations,FilteredEvaluations) :-
    rev(Evaluations,RevEvaluations),
    filter_out_duplicate_concepts_aux(RevEvaluations,RevFilteredEvaluations),
    rev(RevFilteredEvaluations,FilteredEvaluations).

filter_out_duplicate_concepts_aux([],[]).
filter_out_duplicate_concepts_aux([First|Rest],Result) :-
    First=ev(_,_,_,MetaConcept,_,_,_,_,_,_Sources,_PosInfo),
    (concept_occurs(MetaConcept,Rest) ->
        Result=FilteredRest
    ;   Result=[First|FilteredRest]
    ),
    filter_out_duplicate_concepts_aux(Rest,FilteredRest).

concept_occurs(_,[]) :-
    !,
    fail.
concept_occurs(MetaConcept,[ev(_,_,_,MetaConcept,_,_,_,_,_,_Sources,_PosInfo)|_Rest]) :-
    !.
concept_occurs(MetaConcept,[_First|Rest]) :-
    concept_occurs(MetaConcept,Rest).


/* filter_out_input_concept(+Evaluations, +Text, +CUI, -FilteredEvaluations)
   filter_out_input_concept2(+Evaluations, +Text, +CUIs, -FilteredEvaluations)

filter_out_input_concept/4
filter_out_input_concept2/4
*/

%filter_out_input_concept([],_,_,[]).
%filter_out_input_concept([First|Rest],Text,CUI,FilteredRest) :-
%    First=ev(_NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,_SemTypes,
%             _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo),
%    % first try simple matching
%    (Text == MetaConcept; Text == MetaTerm),
%    !,
%    filter_out_input_concept(Rest,Text,CUI,FilteredRest).
%filter_out_input_concept([First|Rest],Text,CUI,FilteredRest) :-
%    First=ev(_NegValue,_CUI,MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
%             _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo),
%    % then actually compute the cuis
%    compute_meta_cuis(MetaTerm,CUIs),
%    member(CUI,CUIs),
%    !,
%    filter_out_input_concept(Rest,Text,CUI,FilteredRest).
%filter_out_input_concept([First|Rest],Text,CUI,[First|FilteredRest]) :-
%    filter_out_input_concept(Rest,Text,CUI,FilteredRest).

%filter_out_input_concept2([],_,_,[]).
%filter_out_input_concept2([First|Rest],Text,CUIs,FilteredRest) :-
%    First=ev(_NegValue,_CUI,MetaTerm,MetaConcept,_MetaWords,_SemTypes,
%             _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo),
%    % first try simple matching
%    (Text == MetaConcept; Text == MetaTerm),
%    !,
%    filter_out_input_concept2(Rest,Text,CUIs,FilteredRest).
%filter_out_input_concept2([First|Rest],Text,CUIs,FilteredRest) :-
%    First=ev(_NegValue,_CUI,MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
%             _MatchMap,_InvolvesHead,_IsOvermatch,_Sources,_PosInfo),
%    % then actually compute the cuis
%    compute_meta_cuis(MetaTerm,TermCUIs),
%    intersect(TermCUIs,CUIs),
%    !,
%    filter_out_input_concept2(Rest,Text,CUIs,FilteredRest).
%filter_out_input_concept2([First|Rest],Text,CUIs,[First|FilteredRest]) :-
%    filter_out_input_concept2(Rest,Text,CUIs,FilteredRest).


/* normalize_meta_text(+Atom, -NMAtom)
/* normalize_meta_string(+String, -NMString)

normalize_meta_text/2
normalize_meta_string/2
*/

normalize_meta_text(Atom,NMAtom) :-
    atom_codes(Atom,String),
    normalize_meta_string(String,NMString),
    atom_codes(NMAtom,NMString).

% see specialist:mwi_utilities
%normalize_meta_string(STR,NMSTR) :-
%    normalized_syntactic_uninvert_string(STR,UnSTR),
%    lower(UnSTR,LCUnSTR),
%    replace_all_substrings(LCUnSTR,"-"," ",NMSTR).


/* print_evaluations_briefly(+Evaluations, +OutputStream)

print_evaluations_briefly/2
xxx
*/

print_evaluations_briefly([],_).
print_evaluations_briefly([ev(_,_,MetaTerm,_,_,_,_,_,_,_Sources,_PosInfo)|Rest],OutputStream) :-
    format(OutputStream,'  ~a~n',[MetaTerm]),
    print_evaluations_briefly(Rest,OutputStream).

/* print_candidate_text(+Candidates, +Label, +Syntax, +OutputStream)

print_candidate_text/4 prints candidate concepts and the text which mapped
to them to OutputStream.  */

print_candidate_text([],_,_,_).
print_candidate_text([ev(_NegScore,_CUI,MetaTerm,MetaConcept,_,_,MatchMap,_,_,_Sources,_PosInfo)|Rest],
		     Label,Syntax,OutputStream) :-
    extract_components(MatchMap,PhraseComponents,_),
    linearize_components(PhraseComponents,LPhraseComponents),
    append(LPhraseComponents,PhraseIndices),
    filter_all_tokens(Syntax,AllPhraseWords,_),
    extract_words_by_indices(PhraseIndices,AllPhraseWords,PhraseWords),
    format(OutputStream,'~p|~p|~p|~p~n',
           [MetaConcept,MetaTerm,PhraseWords,Label]),
    print_candidate_text(Rest,Label,Syntax,OutputStream).

/* print_mapping_summary(+Mappings, +Label, +String, +Syntax, +OutputStream)
   print_mapping_summary(+Mappings, +I, +N, +Label, +String, +Syntax,
                         +OutputStream)

print_mapping_summary/5 prints a mapping summary for String to OutputStream
for each mapping in Mappings using print_mapping_summary/7. */

%print_mapping_summary([],Label,String,_Syntax,OutputStream) :-
%    format(OutputStream,'~p|0|0|0|~s|~n',[Label,String]),
%    !.
print_mapping_summary(Mappings0,Label,String,Syntax,OutputStream) :-
    length(Mappings0,N),
    (control_option(first_mappings_only) ->
	Mappings0=[First|_],
	Mappings=[First]
    ;   Mappings=Mappings0
    ),
    print_mapping_summary(Mappings,1,N,Label,String,Syntax,OutputStream).


print_mapping_summary([],_,_,_,_,_,_) :-
    !.
print_mapping_summary([map(NegScore,Candidates)|Rest],I,N,Label,String,Syntax,
		      OutputStream) :-
    !,
    Score is -NegScore,
%    length(Candidates,NConcepts),
%    compute_phrase_coverage(Candidates,Syntax,PhraseCoverage),
% temp
%format(OutputStream,'~n~p~n',[Candidates]),
%    format(OutputStream,'~p|~d|~d|~d|~d|~2f|',
%	   [Label,I,N,Score,NConcepts,PhraseCoverage]),
    format(OutputStream,'~p|~d|~d|~d|',
	   [Label,I,N,Score]),
    print_candidate_concepts(Candidates,OutputStream),
    format(OutputStream,'~n',[]),
    NewI is I + 1,
    print_mapping_summary(Rest,NewI,N,Label,String,Syntax,OutputStream).
print_mapping_summary(Mappings,_,_,Label,String,_,_) :-
    format('~nFATAL ERROR: print_mapping_summary/7 failed for~n~p~n~p~n~p~n',
	   [Label,String,Mappings]),
    halt.

compute_phrase_coverage(Candidates,Syntax,PhraseCoverage) :-
    parse_phrase_word_info(nofilter,Syntax,NPWI),
    NPWI=pwi(wdl(WordList,_),_,_):pwi(_,_,_),
    length(WordList,N),
    compute_each_coverage(Candidates,Coverages),
    add_coverages(Coverages,0,Sum),
    (Sum > N ->
	format('~nFATAL ERROR: compute_phrase_coverage/3 for~n~p~n~p~n',
	       [Candidates,Syntax])
    ;   true
    ),
    PhraseCoverage is Sum/N,
    !.

compute_each_coverage([],[]) :-
    !.
compute_each_coverage([ev(_,_,_MetaTerm,_MetaConcept,_,_,MatchMap,_,_,_Sources,_PosInfo)|Rest],
		      [I|RestCoverages]) :-
    extract_components(MatchMap,PhraseComponents,_),
    linearize_components(PhraseComponents,LPhraseComponents),
    append(LPhraseComponents,PhraseIndices),
    length(PhraseIndices,I),
    compute_each_coverage(Rest,RestCoverages).
add_coverages([],Sum,Sum) :-
    !.
add_coverages([I|Rest],SumIn,Sum) :-
    SumInOut is SumIn + I,
    !,
    add_coverages(Rest,SumInOut,Sum).
    

/* print_candidate_concepts(+Candidates, +OutputStream)

print_candidate_concepts/4 prints candidate concepts to OutputStream.  */

print_candidate_concepts([],_).
print_candidate_concepts([ev(_,CUI,MetaTerm,MetaConcept,_,SemTypes,_,_,_,_Sources,_PosInfo)|Rest],
			 OutputStream) :-
    control_option(semantic_types),
    !,
    (control_option(show_cuis) ->
	(control_option(show_preferred_names_only) ->
	    format(OutputStream,'~p:~p ~p',[CUI,MetaConcept,SemTypes])
	;   (MetaTerm == MetaConcept ->
		format(OutputStream,'~p:~p ~p',[CUI,MetaConcept,SemTypes])
	    ;   format(OutputStream,'~p:~p (~p) ~p',[CUI,MetaTerm,MetaConcept,
						     SemTypes])
	    )
	)
    ;   (control_option(show_preferred_names_only) ->
	    format(OutputStream,'~p ~p',[MetaConcept,SemTypes])
	;   (MetaTerm == MetaConcept ->
		format(OutputStream,'~p ~p',[MetaConcept,SemTypes])
	    ;   format(OutputStream,'~p (~p) ~p',[MetaTerm,MetaConcept,
						  SemTypes])
	    )
	)
    ),
    (Rest == [] ->
	true
    ;   format(OutputStream,' AND ',[])
    ),
    print_candidate_concepts(Rest,OutputStream).
print_candidate_concepts([ev(_,CUI,MetaTerm,MetaConcept,_,_SemTypes,_,_,_,_Sources,_PosInfo)
			 |Rest],OutputStream) :-
    \+control_option(semantic_types),
    !,
    (control_option(show_cuis) ->
	(control_option(show_preferred_names_only) ->
	    format(OutputStream,'~p:~p',[CUI,MetaConcept])
	;   (MetaTerm == MetaConcept ->
		format(OutputStream,'~p:~p',[CUI,MetaConcept])
	    ;   format(OutputStream,'~p:~p (~p)',[CUI,MetaTerm,MetaConcept])
	    )
	)
    ;   (control_option(show_preferred_names_only) ->
	    format(OutputStream,'~p',[MetaConcept])
	;   (MetaTerm == MetaConcept ->
		format(OutputStream,'~p',[MetaConcept])
	    ;   format(OutputStream,'~p (~p)',[MetaTerm,MetaConcept])
	    )
	)
    ),
    (Rest == [] ->
	true
    ;   format(OutputStream,' AND ',[])
    ),
    print_candidate_concepts(Rest,OutputStream).

/* print_mapping_text(+Mappings, +Label, +Syntax, +OutputStream)

print_mapping_text/4 prints mapping concepts and the text which mapped
to them to OutputStream.  */

print_mapping_text([],_,_,_).
print_mapping_text([map(_,Candidates)|Rest],Label,Syntax,OutputStream) :-
    print_candidate_text(Candidates,Label,Syntax,OutputStream),
    print_mapping_text(Rest,Label,Syntax,OutputStream).


/* print_unique_mapping_text(+Mappings, +Label, +Syntax, +OutputStream)

print_unique_mapping_text/4 prints mapping concepts and the text which mapped
to them for unique mappings only to OutputStream.  */

print_unique_mapping_text([map(_,Candidates)],Label,Syntax,OutputStream) :-
    !, % singleton
    print_candidate_text(Candidates,Label,Syntax,OutputStream).
print_unique_mapping_text(_,_,_,_).


/* print_mapping_counts(+Mappings, +Label, +OutputStream)

print_mapping_counts/3
this counts the length of each mapping, not the number of mappings
xxx
*/

%print_mapping_counts([],_,_).
%print_mapping_counts([map(_,Elements)|Rest],Label,OutputStream) :-
%    length(Elements,N),
%    format(OutputStream,'~d|~a~n',[N,Label]),
%    print_mapping_counts(Rest,Label,OutputStream).


/* print_phrase_specificity(+Mappings, +Text, +Label, +OutputStream)

print_phrase_specificity/4
xxx
*/

%print_phrase_specificity(Maps,Text,Label,OutputStream) :-
%    compute_map_confidence(Maps,MConfidence),
%    compute_specificity_from_maps(Maps,MSpecificity),
%    compute_specificity_from_text(Text,WSpecificity,CSpecificity),
%%    combine_specificities(MSpecificity,WSpecificity,CSpecificity,SpecificityX),
%%    Specificity='X',
%    format(OutputStream,'~a|~a|~p|~p|~p|~p~n',[Label,Text,
%                        MConfidence,MSpecificity,WSpecificity,CSpecificity]),
%    !.
%print_phrase_specificity(_Mappings,Text,Label,_OutputStream) :-
%    format('ERROR: print_phrase_specificity/4 failed for ~a: "~a"~n',
%           [Label,Text]).

%compute_map_confidence([],0) :-
%    !.
%compute_map_confidence([map(NegValue,_)|_],MSpecificity) :-
%    MSpecificity is -NegValue.

%compute_specificity_from_maps([],0) :-
%    !.
%compute_specificity_from_maps(Maps,MSpecificity) :-
%    compute_map_specificities(Maps,MSpecificities),
%    combine_map_specificities(MSpecificities,MSpecificity).

%compute_map_specificities([],[]).
%compute_map_specificities([First|Rest],[FirstSpecificity|RestSpecificities]) :-
%    compute_map_specificity(First,FirstSpecificity),
%    compute_map_specificities(Rest,RestSpecificities).

%compute_map_specificity(Map,MSpecificity) :-
%    Map=map(_Value,Maps),
%    compute_each_specificity(Maps,MSpecificities),
%    aggregate(max(X),member(X,MSpecificities),MSpecificity).
%% temp
%%format('mspec: ~p sum ~p~n',[MSpecificities,MSpecificity]).

%compute_each_specificity([],[]).
%compute_each_specificity([First|Rest],[FirstSpecificity|RestSpecificity]) :-
%    First=ev(_,_,_,MetaConcept,_,_,_,_,_,_Sources,_PosInfo),
%% temp
%%format('concept: ~p~n',[MetaConcept]),
%    ((compute_mesh_tree_codes(MetaConcept,TreeCodes),
%      TreeCodes \== []) ->
%% temp
%%format('  treecodes: ~p~n',[TreeCodes]),
%        compute_specificity_from_codes(TreeCodes,FirstSpecificity)
%    ;   FirstSpecificity=0
%    ),
%    compute_each_specificity(Rest,RestSpecificity).

%compute_specificity_from_codes(TreeCodes,Specificity) :-
%    compute_each_specificity_from_codes(TreeCodes,Specificities),
%    aggregate(max(X),member(X,Specificities),Specificity).

%compute_each_specificity_from_codes([],[]).
%compute_each_specificity_from_codes([First|Rest],
%                                    [FirstSpecificity|RestSpecificities]) :-
%    atom_codes(First,String),
%    split_string_completely(String,".",Strings),
%    length(Strings,FirstSpecificity),
%    compute_each_specificity_from_codes(Rest,RestSpecificities).

%combine_map_specificities(MSpecificities,MSpecificity) :-
%    aggregate(max(X),member(X,MSpecificities),MSpecificity).
%% temp
%%format('mspecs: ~p max ~p~n',[MSpecificities,MSpecificity]).

%compute_specificity_from_text(Text,WSpecificity,CSpecificity) :-
%    !,
%    atom_codes(Text,String),
%    length(String,CSpecificity),
%    split_string_completely(String," ",Tokens),
%    length(Tokens,WSpecificity).

% current combination method is simply to prefer specificity from maps
%combine_specificities(0,WSpecificity,CSpecificity,Specificity) :-
%    !,
%% temp
%    Specificity=0.
%combine_specificities(MSpecificity,WSpecificity,CSpecificity,Specificity) :-
%% temp
%    Specificity=0.


/* is_composite_phrase(+Phrases, -CompositePhrase, -NewRest)
   gather_prep_phrases(+Phrases, -PrepPhrases, -NewRest)
   is_prep_phrase(+Phrase, -PrepList)
   dump_composite_phrase(+CompositePhrase, +OutputStream)

is_composite_phrase/3 succeeds if Phrases begins with a composite phrase,
i.e., a simple phrase followed by one or more prepositional phrases.
It returns CompositePhrase and NewRest.
gather_prep_phrases/3 computes PrepPhrases and NewRest from Phrases; PrepPhrases
is the initial sequence of prepositional phrases and NewRest is what remains.
is_prep_phrase/2 succeeds if Phrase is a prepositional phrase and returns
PrepList, a list containing the preposition.
dump_composite_phrase/2 writes CompositePhrase to OutputStream.
*/

is_composite_phrase([First,Second|Rest],[First,Second|RestComposite],NewRest) :-
    is_prep_phrase(Second,_),
    !,
    gather_prep_phrases(Rest,RestComposite,NewRest).

gather_prep_phrases([],[],[]).
gather_prep_phrases([First|Rest],[First|RestOf],NewRest) :-
    is_prep_phrase(First,_),
    !,
    gather_prep_phrases(Rest,RestOf,NewRest).
gather_prep_phrases(Phrases,[],Phrases).

is_prep_phrase(Phrase,PrepList) :-
    Phrase = phrase(_Text,[PhraseItem|_],_Candidates,_Mappings,_PosInfo,_ReplPos),
    get_phrase_item_name(PhraseItem,prep),
    get_phrase_item_feature(PhraseItem,lexmatch,PrepList).

dump_composite_phrase(CompositePhrase,OutputStream) :-
    length(CompositePhrase,Length),
    extract_composite_text(CompositePhrase,CompositeText),
    extract_composite_preps(CompositePhrase,CompositePreps0),
    append(CompositePreps0,CompositePreps),
    format(OutputStream,'~d|~p|~p~n',[Length,CompositePreps,CompositeText]).

extract_composite_text([],[]) :-
    !.
extract_composite_text([First|Rest],[FirstText|RestText]) :-
    First=phrase(Text,_Syntax,_Candidates,_Mappings,_PosInfo,_ReplPos),
    !,
    lower(Text,FirstText),
    extract_composite_text(Rest,RestText).
extract_composite_text(Phrases,_) :-
    format('~NError: extract_composite_text/2 failed for~n~p~n',[Phrases]),
    !,
    fail.

extract_composite_preps([],[]) :-
    !.
extract_composite_preps([First|Rest],[FirstPrep|RestPreps]) :-
    is_prep_phrase(First,FirstPrep),
    !,
    extract_composite_preps(Rest,RestPreps).
extract_composite_preps([_|Rest],RestPreps) :-
    extract_composite_preps(Rest,RestPreps).


/* organize_semantic_types(+Phrases, +OutputStream)

organize_semantic_types/2 writes to OutputStream the concepts in the
first mappings in Phrases by semantic type. */

organize_semantic_types(Phrases, OutputStream) :-
	% format(OutputStream,'~norganize_semantic_types: TDB~n~p~n',[Phrases]),
	extract_mappings(Phrases, Mappings),
	% format(OutputStream,'~nMappings:~n~p~n',[Mappings]),
	extract_sts_concepts(Mappings, STsConcepts),
	% format(OutputStream,'~nSTsConcepts0:~n~p~n',[STsConcepts0]),
	% sort(STsConcepts0,STsConcepts),
	update_saved_results(STsConcepts),
	write_sts_concepts(STsConcepts, null, OutputStream),
	!.

extract_mappings([],[]) :-
    !.
extract_mappings([Phrase|Rest],ExtractedRest) :-
    Phrase = phrase(_,_,_,mappings([]),_PosInfo,_ReplPos),
    !,
    extract_mappings(Rest,ExtractedRest).
extract_mappings([Phrase|Rest],Result) :-
    Phrase = phrase(_,_,_,Mappings0,_PosInfo,_ReplPos),
    Mappings0=mappings([FirstMapping|RestMappings]),
    (control_option(first_mappings_only) ->

	Mappings=[FirstMapping]
    ;   Mappings=[FirstMapping|RestMappings]
    ),
    append(Mappings,RemainingMappings,Result),
    extract_mappings(Rest,RemainingMappings).

extract_sts_concepts([],[]) :-
    !.
extract_sts_concepts([map(_,Evs)|Rest],Result) :-
    extract_sts_concepts_aux(Evs,STsConcepts),
    append(STsConcepts,RestSTsConcepts,Result),
    extract_sts_concepts(Rest,RestSTsConcepts).

extract_sts_concepts_aux([],[]) :-
    !.
extract_sts_concepts_aux([ev(_,_,Term,Concept,_,STs,_,_,_,_Sources,_PosInfo,_Status)|Rest],Result) :-
    form_sts_concepts(STs,Term,Concept,STsConcepts),
    append(STsConcepts,RestSTsConcepts,Result),
    extract_sts_concepts_aux(Rest,RestSTsConcepts).

form_sts_concepts([],_,_,[]) :-
    !.
form_sts_concepts([First|Rest],Term,Concept,
		  [stcon(First,Term,Concept)|RestSTsConcepts]) :-
    form_sts_concepts(Rest,Term,Concept,RestSTsConcepts).

write_sts_concepts([],_,_) :-
    !.
write_sts_concepts([stcon(ST,Term,Concept)|Rest],ST,OutputStream) :-
    !,
    (Term == Concept ->
	format(OutputStream,'  ~p~n',[Term])
    ;   format(OutputStream,'  ~p (~p)~n',[Term,Concept])
    ),
    write_sts_concepts(Rest,ST,OutputStream).
write_sts_concepts([stcon(ST,Term,Concept)|Rest],_,OutputStream) :-
    (semtype_translation(Semtype,ST) ->
	true
    ;   Semtype='<unknown>'
    ),
    format(OutputStream,'~n~p (~p):~n',[Semtype,ST]),
    (Term == Concept ->
	format(OutputStream,'  ~p~n',[Term])
    ;   format(OutputStream,'  ~p (~p)~n',[Term,Concept])
    ),
    write_sts_concepts(Rest,ST,OutputStream).

update_saved_results(STsConcepts) :-
    (retract(saved_results(Saved)) ->
	true
    ;   Saved=[]
    ),
    append(Saved,STsConcepts,New0),
    sort(New0,New),
    assert(saved_results(New)),
    !.
	
% dump_evaluations_indented(Evaluations,Label,OutputStream) :-
%     current_output(CurrentOutput),
%     set_output(OutputStream),
%     (dump_evaluations_indented(Evaluations,Label); true),
%     !,
%     set_output(CurrentOutput).

/* extract_words_by_indices(+Indices, +Words, -ExtractedWords)

extract_words_by_indices/3 extracts ExtractedWords from Words corresponding
to (1-based) Indices.  */

extract_words_by_indices(Indices,Words,ExtractedWords) :-
    extract_words_by_indices(Indices,1,Words,ExtractedWords).

extract_words_by_indices([],_,_,[]).
extract_words_by_indices([N|Rest],N,[FirstWord|RestWords],
                         [FirstWord|RestExtractedWords]) :-
    !,
    NewN is N + 1,
    extract_words_by_indices(Rest,NewN,RestWords,RestExtractedWords).
extract_words_by_indices(Indices,N,[_FirstWord|RestWords],
                         RestExtractedWords) :-
    NewN is N + 1,
    extract_words_by_indices(Indices,NewN,RestWords,RestExtractedWords).


filter_all_tokens(PhraseItems,TokenWords,TokenHeadWords) :-
    filter_all_tokens_aux(PhraseItems,TokenWords0,TokenHeadWords0),
    append(TokenWords0,TokenWords),
    append(TokenHeadWords0,TokenHeadWords).

filter_all_tokens_aux([],[],[]).
filter_all_tokens_aux([First|Rest],[FirstTW|RestTW],
                      [FirstTHW|RestTHW]) :-
    filter_tokens(First,FirstTW,FirstTHW),
    filter_all_tokens_aux(Rest,RestTW,RestTHW).



/* string_contains_alnum(+String)

string_contains_alnum/1 succeeds if String contains an alphanumeric
character. */

string_contains_alnum("") :-
    !,
    fail.
string_contains_alnum([Char|_Rest]) :-
    is_alnum(Char),
    !.
string_contains_alnum([_Char|Rest]) :-
    string_contains_alnum(Rest).

/* split_text(+Text, +Subtext, ?Left, ?Right)
split_text/4 embodies the property that Text is the concatenation of
Left, Subtext and Right in that order.  Subtext (and hence Text) must be
non-null.  Backtracking is not allowed. */

split_text(Text, SubText, Left, Right) :-
	\+ SubText= '',
	append_text(Left, S1, Text),
	append_text(SubText, Right, S1),
	!.


/* append_text(?Text1, ?Text2, ?Text)

append_text/3 is analogous to append/3 for text (atoms) except that at least
one of its arguments MUST be instantiated. */

append_text(Text1, Text2, Text) :-
	atom_codes(Text1, S1),
	atom_codes(Text2, S2),
	append(S1, S2, S),
	atom_codes(Text, S).



print_mappings([],Label,OutputStream) :-
    !,
    format(OutputStream,'Meta ~a: <none>~n',[Label]).
print_mappings(mappings(Mappings),Label,OutputStream) :-
    print_mappings_aux(Mappings,Label,OutputStream).

print_mappings_aux([],_,_).
print_mappings_aux([map(NegValue,Mapping)|Rest],Label,OutputStream) :-
    Value is -NegValue,
    format(OutputStream,'Meta ~a (~d):~n',[Label,Value]),
    print_evaluations(Mapping,OutputStream),
    print_mappings_aux(Rest,Label,OutputStream).


/* fread_term(+Stream, -Term)

fread_term/2 reads a Term from Stream.  It fails at end-of-file.  */

fread_term(Stream, Term) :-
	% \+ at_end_of_stream(Stream),
	peek_code(Stream, Code),
	Code =\= -1,
	read(Stream, Term),
	\+ Term == end_of_file.

% Can this be replaced by metamap_evaluation:dump_evaluations?
print_evaluations([], _).
print_evaluations([ev(NegValue,CUI,MetaTerm,MetaConcept,_MetaWords,SemTypes0,
                      _MatchMap,_InvolvesHead,_IsOvermatch,SourceInfo,_PosInfo,_Status)|Rest],
                  OutputStream) :-
	Value is -NegValue,
	build_concept_name_1(MetaConcept, CUI, SourceInfo, ConceptName),
	( control_option(show_cuis) ->
	  ( ( MetaTerm == MetaConcept; control_option(show_preferred_names_only)) ->
            format(OutputStream, '~t~d~6| ~p:~p', [Value,CUI,ConceptName])
	  ; format(OutputStream, '~t~d~6| ~p:~p (~p)', [Value,CUI,MetaTerm,ConceptName])
	  )
	; ( ( MetaTerm == MetaConcept; control_option(show_preferred_names_only)) ->
	    format(OutputStream, '~t~d~6| ~p', [Value,ConceptName])
	  ; format(OutputStream, '~t~d~6| ~p (~p)', [Value,MetaTerm,ConceptName])
	  )
	),
	( control_option(semantic_types) ->
	  expand_semtypes(SemTypes0, SemTypes),
	  format(OutputStream, ' ~p', [SemTypes])
	; true
	),
	format(OutputStream, '~n', []),
	print_evaluations(Rest, OutputStream).

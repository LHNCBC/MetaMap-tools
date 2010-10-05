% File:     mm_variants.pl
% Module:   MM Variants
% Author:   Lan
% Purpose:  To pre-compute MetaMap variants

:- module(mm_variants,[
	go/0,
	go/1,
	go/2,
	stop_mm_variants/0
    ]).

:- use_module(mm_tools_lib(factbase),[
	erase_all_facts/2,
	put_fact/3
    ]).

:- use_module(mm_tools_lib(mwi_utilities),[
	compute_unique_filename/3
   ]).

:- use_module(lexicon(lex_access),[
	initialize_lexicon/2,
	is_a_form/1,
	get_categories_for_form/2
    ]).

:- use_module(lexicon(qp_lexicon),[
	use_multi_word_lexicon/0
    ]).

:- use_module(metamap(metamap_variants),[
	initialize_metamap_variants/1,
	augment_GVCs_with_variants/1
    ]).

:- use_module(metamap(metamap_tokenization),[
	tokenize_text_mm/2
    ]).

:- use_module(skr_db(db_access),[
	default_full_year/1,
	initialize_db_access/0,
	stop_db_access/0
    ]).

:- use_module(skr_lib(efficiency),[
	maybe_atom_gc/2
    ]).

:- use_module(skr_lib(nls_system), [
	get_control_options_for_modules/2,
	reset_control_options/1,
	toggle_control_options/1,
	set_control_values/2,
	display_control_options_for_modules/2,
	display_current_control_options/2,
	control_option/1,
	parse_command_line/1,
	interpret_options/4,
	interpret_args/4,
	get_from_iargs/4
    ]).

:- use_module(mm_tools_lib(reader),[
	input_text/2
    ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

:- use_module(library(lists),[
	rev/2
    ]).


/* go
   go(+HaltFlag)
   go(+HaltFlag, +CommandLineTerm)

go/0 is the executive predicate for the MM Variants.
go/0 uses go/1 with HaltFlag set to halt.
go/1 parses the command line and calls go/2 which controls the processing.  */

go :-
    go(halt).

go(HaltOption) :-
    parse_command_line(CLTerm),
    go(HaltOption,CLTerm).

go(HaltOption,command_line(Options,Args)) :-
    reset_control_options(mm_variants),
    (initialize_mm_variants(Options,Args,InterpretedArgs) ->
        (mm_variants(InterpretedArgs); true)
    ;   usage
    ),
    stop_mm_variants,
    (HaltOption==halt ->
        halt
    ;   true
    ).


/* initialize_mm_variants(+Options, +Args, -InterpretedArgs)

initialize_mm_variants/3 interprets command line options and arguments (opening
files as necessary) and sets and displays the MM Variants control options
discovered.  */

initialize_mm_variants(Options,Args,IArgs) :-
    get_control_options_for_modules([mm_variants],AllOptions),
    interpret_options(Options,AllOptions,mm_variants,IOptions),
    \+member(iopt(help,_),IOptions),
    ArgSpecs=[aspec(infile,mandatory,file,read,
                           no_default,
                           'Input file containing labelled terms'),
             aspec(outfile,mandatory,file,write,
                           ['<infile>','.','out'],
                           'Output file')
            ],
    use_multi_word_lexicon,
    interpret_args(IOptions,ArgSpecs,Args,IArgs),
    toggle_control_options(IOptions),
    set_control_values(IOptions,IArgs),
    default_full_year(FullYear),
    display_current_control_options(mm_variants, FullYear),
    initialize_mm_variants,
    !.

initialize_mm_variants :-
    initialize_lexicon(_,_),
    initialize_metamap_variants(dynamic),
    initialize_db_access.

stop_mm_variants :-
    stop_db_access,
    close_all_streams,
    !.


/* usage

usage/0 displays MM Variants usage.  */

usage :-
    format('~nUsage: mm_variants [<options>] <infile> [<outfile>]~n~n',[]),
    format('  <infile> contains labeled terms (default is user_input),~n',[]),
    format('  and <outfile> is a file for results (default is <infile>.out).~n~n',[]),
    display_control_options_for_modules(mm_variants,[]).


/* mm_variants(+InterpretedArgs)

mm_variants/1 redirects I/O streams and then calls process_all/0.  */

mm_variants(InterpretedArgs) :-
    erase_all_facts(input,file),  % for reader:input_text/2's benefit
    get_from_iargs(infile,name,InterpretedArgs,InputFile),
    get_from_iargs(infile,stream,InterpretedArgs,InputStream),
    get_from_iargs(outfile,name,InterpretedArgs,OutputFile),
    get_from_iargs(outfile,stream,InterpretedArgs,OutputStream),
    format('~n~nBeginning to process ~a sending output to ~a.~n~n',
           [InputFile,OutputFile]),
    current_input(SavedCurrentInput),
    set_input(InputStream),
    put_fact(input,file,InputFile),  % for reader:input_text/2's benefit
    current_output(SavedCurrentOutput),
    set_output(OutputStream),
    process_all,
    (control_option(end_of_processing) ->
	format(OutputStream,'<<< EOT >>>~n',[])
    ;   true
    ),
    set_input(SavedCurrentInput),
    set_output(SavedCurrentOutput),
    close(OutputStream),
    close(InputStream),
    format('~nBatch processing is finished.~n',[]).
mm_variants(_InterpretedArgs).


/* process_all

process_all/0 reads labelled terms from user_input and writes variants
onto user_output.  (user_input and user_output have been redirected to
files.)  */

process_all :-
    repeat,
    ((input_text(sentence,ListOfAscii), ListOfAscii\=="") ->
        process_text(ListOfAscii),
        fail
    ;   true
    ),
    !.


/* process_text(+Text)

process_text/1 finds and writes variants of the labelled term Text.  */

process_text(Text) :-
    parse_input(Text,_Label0,Term0),
    Term0\=="",
    atom_codes(Term,Term0),
    current_output(CurrentOutput),
    flush_output(CurrentOutput),
%    (CurrentOutput==user_output ->
%        true
%    ;   format(user_output,'Processing ~s~n',[Text]),
%        flush_output(user_output)
%    ),
    maybe_atom_gc(DidGC,SpaceCollected),
    ((control_option(info), DidGC==yes) ->
        format('Atom GC performed collecting ~d bytes.~n',[SpaceCollected])
    ;   true
    ),
    compute_and_write_variants(Term),
    !.

% parse_input/3 parses InputString into LabelString and TextString.  
% InputString is of the form <label><text> where <label> is optional 
% and each can be preceded by blanks.  parse_input/3 originated in 
% the phrase extractor.

parse_input(InputString,LabelString,TextString) :-
    phrase(r_labelled_text([LabelString,TextString]),InputString).

% ---------- GRAMMAR FOR LABELLED TEXT 

r_labelled_text(LT) --> ([0' ], !, r_labelled_text(LT)
                    ;    r_label(L), r_text(T), {LT=[L,T]}
                    ;    r_text(T), {LT=[[],T]}
                        ), !.

r_label(L) --> [0'[], r_non_r_bracket(L), [0']].

r_text(T) --> ([0' ], !, r_text(T)
          ;    r_any(T)
              ), !.

r_non_r_bracket(S) --> ([Char], {\+Char==0']}, r_non_r_bracket(T), {S=[Char|T]}
                   ;    {S=[]}
                       ), !.

r_any(S) --> ([Char], r_any(T), {S=[Char|T]}
         ;    {S=[]}
             ), !.


/* compute_and_write_variants(+Term)
   compute_and_write_variants(+SplitCategories, +Term)

compute_and_write_variants/1 computes the variants (see MetaMap's 0DATA.DEFNS)
of Term by computing its categories which are split into SplitCategories and
passed on to augment_GVCs_with_variants/1.  */

compute_and_write_variants(Term) :-
% temp
%format('~a|0~n',[Term]),
    (is_a_form(Term) ->
        true
    ;   format(user_output,'WARNING: is_a_form/1 failed for ~p.~n',
               [Term])
    ),
    (get_categories_for_form(Term,Categories) ->
        true
    ;   Categories=[]
    ),
    split_categories(Categories,SplitCategories),
% temp
%format('~a|1|~p~n',[Term,SplitCategories]),
    compute_and_write_variants(SplitCategories,Term).
compute_and_write_variants(Term) :-
    format(user_output,'ERROR: compute_and_write_variants/1 failed for ~p.~n',
           [Term]),
    fail.

split_categories([],[[]]) :- !.
split_categories(Categories,SplitCategories) :-
    split_categories_aux(Categories,SplitCategories).

split_categories_aux([],[]) :- !.
split_categories_aux([First|Rest],[[First]|SplitRest]) :-
    split_categories_aux(Rest,SplitRest).

compute_and_write_variants([],_).
compute_and_write_variants([Categories|RestCategories],Term) :-
    maybe_atom_gc(DidGC,SpaceCollected),
    ((control_option(info), DidGC==yes) ->
        format('Atom GC performed collecting ~d bytes.~n',[SpaceCollected])
    ;   true
    ),
% temp
%format('~a|2|~p~n',[Term,Categories]),
    GVCs=[gvc(v(Term,Categories,0,"",_,_),_,_)],
    augment_GVCs_with_variants(GVCs),
    GVCs=[gvc(_,Variants,_)],
% temp
%format('~a|3|~p~n',[Term,Variants]),
    simplify_categories(Categories,SimplifiedCats),
    write_variants(Term,SimplifiedCats,Variants),
    compute_and_write_variants(RestCategories,Term),
    !.
compute_and_write_variants(Categories,Term) :-
    format('ERROR: compute_and_write_variants/2 failed for ~p  ~p~n',
           [Categories,Term]).


/* write_variants(+Term, +SimplifiedTermCats, +Variants)

write_variants/2 writes Variants for Term on user_output (redirected).  */

write_variants(_Term,_SimplifiedTermCats,[]).
write_variants(Term,SimplifiedTermCats,
               [v(Word,Categories,VarLevel,History,Roots,_NFR)
	       |Rest]) :-
%    rev(History0,History),
    simplify_categories(Categories,SimplifiedCats),
    (tokenize_text_mm(Word,[FirstOfWord|_]) ->
	true
    ;   FirstOfWord='',
        format('~NERROR: "~a" has no first word.',[Word])
    ),
    (var(Roots) ->
	Roots=[]
    ;   true
    ),
    format('~a|~p|~a|~p|~d|~s|~a|~p~n',[Term,SimplifiedTermCats,
           Word,SimplifiedCats,VarLevel,History,FirstOfWord,Roots]),
    write_variants(Term,SimplifiedTermCats,Rest).

simplify_categories([],none) :- !.
simplify_categories([Singleton],Singleton) :- !.
simplify_categories(Cats,Cats).


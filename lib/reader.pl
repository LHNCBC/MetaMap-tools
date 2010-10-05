% File:	    reader.pl
% Module:   reader
% Author:   Paoli
% Purpose:  Specialist parser component.

:- module(reader, [ % exported predicates
      input_text/2
      ]).


% :- no_style_check(multiple).

:- use_module(mm_tools_lib(factbase), [
	erase_all_facts/2,
	erase_fact/3,
	get_fact/3,
	put_fact/3
   ]).

:- use_module(skr_lib(sicstus_utils),[
	ttyflush/0
    ]).


/* This file contains a new tokenizer for Pundit.  The old tokenizer
	had several problems, including:

	- it was hard to understand and modify.
	- it did not handle apostrophes correctly
	- it was in no way rule driven.

	This new tokenizer is rule driven (it is a DCG), and handles
	aprostrophes correctly.  Being rule driven, it is easy to 
	modify.

	Here is the definition I have decided on for a "token"


	A token will always be the longest sequence of characters 
		allowed by these rules.
	A token can be a sequence of one or more letters.
	A token can be a sequence of one or more digits.
	A token can be an ' character, followed by one or more letters, 
		if it is preceded by a letter and the previous word does 
		not start with an '.
	A token can be a "single character word" which include '.?!:;-[]()%/.

*/

% ----------------------- Modification History -------------------------
%
% 14 Apr 89 FML
% 
% Added one clause for isa_a_single_character_token/1 for opreps
%   and one clause (commented out) for translate_single_character/1
% 
% 
% 

input_text(Prompt,LoA):-
    (get_fact(input,file,_) ->
        true
    |   nl,nl,write(Prompt),write(': '),ttyflush
    ),
   % LRA -- added second parameter (prompt) to get_until_dbl_CR/3
    get_until_dbl_CR(32,': ',ListOfAscii),
%%%Suresh - strips leading 32
    strip_leading_whitespace(ListOfAscii, LoA),
%%%   ListOfAscii = [32|LoA],
    !.

strip_leading_whitespace([F|X], Y) :-
  ( F = 32 % space
  ; F = 9  % tab
  ; F = 10 % newline
  ),
  !,
  strip_leading_whitespace(X, Y).
strip_leading_whitespace(X, X).


get_until_dbl_CR(Char,NewPrompt,[Char|RestChars]) :-
    get_code(NextChar),
    prompt(_,NewPrompt),
    (NextChar == -1 ->
        !,
        fail
    |   true
    ),
    NextChar =\= 10,
    !,
    get_until_dbl_CR(NextChar,NewPrompt,RestChars).
get_until_dbl_CR(Char,NewPrompt,Rest) :-
    get_code(NextChar),
    (NextChar == -1 ->
        !,
        fail
    |   true
    ),
    (NextChar = 10 ->
         Rest = [Char]
    |    Rest = [Char,32|Ascii],
         get_until_dbl_CR(NextChar,NewPrompt,Ascii)
    ).




/* makeWordList(+ListOfAsciiCharacters,-ListOfTokens)

	This is the top level of the tokenizer.
*/

makeWordList(ListOfAsciiCharacters,ListOfTokens):-
	list_of_tokens(normal,ListOfTokens,ListOfAsciiCharacters,[]),
	!.


/* list_of_tokens(+ApostropheReadingMode,-ListOfTokens).

	This is the main recursive rule in the DCF.
	ApostropheReadingMode will have one of two values:  
				normal and dont_connect.

	dont_connect mode is used to disallow more than one apostrophe
	in a word.  In normal mode, the string "barb's" will be 
	tokenized as two tokens [barb,'''s'].  In dont_connect mode,
	it would be tokenized as [barb,'''',s].  The overall effect
	is to tokenize strings like "i'd've" as [i,'''d','''',ve].
	This will prevent over generation problems with apostrophe.
*/
list_of_tokens(normal,[Token|MoreTokens]) -->
	word_token(Token),
	optional_apostrophe(MoreTokens,RestOfTokens,ApostropheReadingMode),
	list_of_tokens(ApostropheReadingMode,RestOfTokens).
list_of_tokens(dont_connect,[Token|RestOfTokens]) -->
	word_token(Token),
	list_of_tokens(dont_connect,RestOfTokens).
/*
list_of_tokens(ApostropheReadingMode,[Token|RestOfTokens]) -->
	number_token(Token),
	list_of_tokens(ApostropheReadingMode,RestOfTokens).
*/
list_of_tokens(_,[Token|RestOfTokens]) -->
	single_character_token(Token),
	list_of_tokens(normal,RestOfTokens).
list_of_tokens(_,ListOfTokens) -->
	[_],
	list_of_tokens(normal,ListOfTokens).
list_of_tokens(_,[]) --> [].

/* a word token is one or more consecutive letters. */
word_token(Word) -->
	consecutive_letters(Letters),
	{name(Word,Letters)}.

/* an optional aprostrophe followed by one or more letters is allowed
	if in normal mode.  If it occurs, it puts the tokenizer into
	dont_connect mode until a single character word or white
	space is reached.
	
	consecutive_letters_helper is called (instead of consecutive_letters)
	to allow zero or more letters following the apostrophe.
*/
optional_apostrophe([ApostropheToken|RestTokens],RestTokens,dont_connect) -->
	"'",
	consecutive_letters_helper(Letters),
	!,
	{append("'",Letters,TokenCharacters),
	 name(ApostropheToken,TokenCharacters)}.
optional_apostrophe(ListOfTokens,ListOfTokens,normal) -->
	[].

/* a number token is one or more consecutive digits. */
%LRAnumber_token(Number) -->
%LRA	consecutive_digits(Digits),
%LRA	{name(Number,Digits)}.

/* a single character token is a punctuation mark. */
single_character_token(SingleCharacterToken) -->
	[SingleCharacter],
	{is_a_single_character_token(SingleCharacter),
	 name(SingleCharacterToken,[SingleCharacter])}.

/* consecutive_digits consumes one or more digits. */
%LRAconsecutive_digits([Digit|RestDigits]) -->
%LRA	[Digit],
%LRA	{is_a_digit(Digit)},
%LRA	consecutive_digits_helper(RestDigits).

/* consecutive_digits_helper consumes zero or more digits. */
%LRAconsecutive_digits_helper([Digit|RestDigits]) -->
%LRA	[Digit],
%LRA	{is_a_digit(Digit)},
%LRA	!,
%LRA	consecutive_digits_helper(RestDigits).
%LRAconsecutive_digits_helper([]) --> 
%LRA	[].

/* consecutive_letters consumes one or more letters */
consecutive_letters([LowerCaseLetter|RestLetters]) -->
	[Letter],
	{is_a_letter_or_digit(Letter,LowerCaseLetter)},
	consecutive_letters_helper(RestLetters).

/* consecutive_letters_helper consumes zero or more letters. */
consecutive_letters_helper([LowerCaseLetter|RestLetters]) -->
	[Letter],
	{is_a_letter_or_digit(Letter,LowerCaseLetter)},
	!,
	consecutive_letters_helper(RestLetters).
consecutive_letters_helper([]) --> 
	[].


is_a_letter_or_digit(Letter,LowerCaseLetter):-
	is_a_letter(Letter,LowerCaseLetter).
is_a_letter_or_digit(Digit,Digit):-
	is_a_digit(Digit).

/* Here is a Quintus Prolog specific implementation of is_a_digit.
	Commented out below is a general (but slower) version.
*/

is_a_digit('0').
is_a_digit('1').
is_a_digit('2').
is_a_digit('3').
is_a_digit('4').
is_a_digit('5').
is_a_digit('6').
is_a_digit('7').
is_a_digit('8').
is_a_digit('9').

/*
	is_a_digit(Digit):-
		translate_digit([Digit]).
	
	translate_digit("0").
	translate_digit("1").
	translate_digit("2").
	translate_digit("3").
	translate_digit("4").
	translate_digit("5").
	translate_digit("6").
	translate_digit("7").
	translate_digit("8").
	translate_digit("9").
*/


/* Here is a Quintus Prolog specific implementation of is_a_letter.
	Commented out below is a general (but slower) version.
*/

is_a_letter(0'a, 0'a).
is_a_letter(0'b, 0'b).
is_a_letter(0'c, 0'c).
is_a_letter(0'd, 0'd).
is_a_letter(0'e, 0'e).
is_a_letter(0'f, 0'f).
is_a_letter(0'g, 0'g).
is_a_letter(0'h, 0'h).
is_a_letter(0'i, 0'i).
is_a_letter(0'j, 0'j).
is_a_letter(0'k, 0'k).
is_a_letter(0'l, 0'l).
is_a_letter(0'm, 0'm).
is_a_letter(0'n, 0'n).
is_a_letter(0'o, 0'o).
is_a_letter(0'p, 0'p).
is_a_letter(0'q, 0'q).
is_a_letter(0'r, 0'r).
is_a_letter(0's, 0's).
is_a_letter(0't, 0't).
is_a_letter(0'u, 0'u).
is_a_letter(0'v, 0'v).
is_a_letter(0'w, 0'w).
is_a_letter(0'x, 0'x).
is_a_letter(0'y, 0'y).
is_a_letter(0'z, 0'z).
is_a_letter(0'A, 0'a).
is_a_letter(0'B, 0'b).
is_a_letter(0'C, 0'c).
is_a_letter(0'D, 0'd).
is_a_letter(0'E, 0'e).
is_a_letter(0'F, 0'f).
is_a_letter(0'G, 0'g).
is_a_letter(0'H, 0'h).
is_a_letter(0'I, 0'i).
is_a_letter(0'J, 0'j).
is_a_letter(0'K, 0'k).
is_a_letter(0'L, 0'l).
is_a_letter(0'M, 0'm).
is_a_letter(0'N, 0'n).
is_a_letter(0'O, 0'o).
is_a_letter(0'P, 0'p).
is_a_letter(0'Q, 0'q).
is_a_letter(0'R, 0'r).
is_a_letter(0'S, 0's).
is_a_letter(0'T, 0't).
is_a_letter(0'U, 0'u).
is_a_letter(0'V, 0'v).
is_a_letter(0'W, 0'w).
is_a_letter(0'X, 0'x).
is_a_letter(0'Y, 0'y).
is_a_letter(0'Z, 0'z).


/*
	is_a_letter(Char, LowerCaseOfChar):-
		translate_char([Char], [LowerCaseOfChar]).
	
	translate_char("a", "a").
	translate_char("b", "b").
	translate_char("c", "c").
	translate_char("d", "d").
	translate_char("e", "e").
	translate_char("f", "f").
	translate_char("g", "g").
	translate_char("h", "h").
	translate_char("i", "i").
	translate_char("j", "j").
	translate_char("k", "k").
	translate_char("l", "l").
	translate_char("m", "m").
	translate_char("n", "n").
	translate_char("o", "o").
	translate_char("p", "p").
	translate_char("q", "q").
	translate_char("r", "r").
	translate_char("s", "s").
	translate_char("t", "t").
	translate_char("u", "u").
	translate_char("v", "v").
	translate_char("w", "w").
	translate_char("x", "x").
	translate_char("y", "y").
	translate_char("z", "z").
	translate_char("A", "a").
	translate_char("B", "b").
	translate_char("C", "c").
	translate_char("D", "d").
	translate_char("E", "e").
	translate_char("F", "f").
	translate_char("G", "g").
	translate_char("H", "h").
	translate_char("I", "i").
	translate_char("J", "j").
	translate_char("K", "k").
	translate_char("L", "l").
	translate_char("M", "m").
	translate_char("N", "n").
	translate_char("O", "o").
	translate_char("P", "p").
	translate_char("Q", "q").
	translate_char("R", "r").
	translate_char("S", "s").
	translate_char("T", "t").
	translate_char("U", "u").
	translate_char("V", "v").
	translate_char("W", "w").
	translate_char("X", "x").
	translate_char("Y", "y").
	translate_char("Z", "z").
*/

/* Here is a Quintus Prolog specific implemenation
	single_character_token.  Commented out below is 
	a general version.
*/

is_a_single_character_token(',').  % ,
is_a_single_character_token(';').  % ;
is_a_single_character_token(':').  % :
is_a_single_character_token('?').  % ?
is_a_single_character_token('!').  % !
is_a_single_character_token('.').  % .
is_a_single_character_token('&').  % &
is_a_single_character_token('@').  % @
is_a_single_character_token('[').  % [
is_a_single_character_token(']').  % ]
is_a_single_character_token('(').  % (
is_a_single_character_token(')').  % )
is_a_single_character_token('%').  % %
is_a_single_character_token('/').  % /
is_a_single_character_token('-').  % -
is_a_single_character_token(''''). % '
is_a_single_character_token('"').  % "  %%% FML added this for opreps


/*
	
	single_character_token(SingleCharacter):-
		translate_single_character([SingleCharacter]).
	
	translate_single_character(","). % ,
	translate_single_character(";"). % ;
	translate_single_character(":"). % :
	translate_single_character("?"). % ?
	translate_single_character("!"). % !
	translate_single_character("."). % .
	translate_single_character("&"). % &
	translate_single_character("@"). % @
	translate_single_character("["). % [
	translate_single_character("]"). % ]
	translate_single_character("("). % (
	translate_single_character(")"). % )
	translate_single_character("%"). % %
	translate_single_character("/"). % /
	translate_single_character("-"). % -
	translate_single_character("'"). % '
	translate_single_character(""""). % "  %%% FML added this for opreps

*/


terminalWord('.').
terminalWord('?').
terminalWord('!').

% to_upper_ascii(+AsciiChar, ?AsciiUpperCaseChar) takes an ASCII value
% and converts it to upper case if it's a lower case letter
% and leaves it alone otherwise

to_upper_ascii(AsciiChar, AsciiUpperCaseChar) :-
	AsciiChar >= 97,
	AsciiChar =< 122,
	!,
	AsciiUpperCaseChar is AsciiChar - 32.
to_upper_ascii(AsciiChar, AsciiChar).


% upper_case_word(+Word, ?UpperCaseWord) converts Word
% to upper case

upper_case_word(Word, UpperCaseWord) :-
	atomic(Word),
	!,
	name(Word, WordName),
	word_to_upper_ascii(WordName, UpperCaseWordName),
	name(UpperCaseWord, UpperCaseWordName).
upper_case_word(Word, Word).


word_to_upper_ascii([], []).
word_to_upper_ascii([H|T], [UpperH|UpperT]) :-
	to_upper_ascii(H, UpperH),
	word_to_upper_ascii(T, UpperT).



% LRA--moved pundit_current_stream/3, pundit_write/2 and pundit_writeq/2
% LRA  from pde_io_procedures.pl.


pundit_current_stream(File,Mode,Stream) :-
	current_stream(File,Mode,Stream).

pundit_write(Stream,Term) :-
	write(Stream,Term).

pundit_writeq(Stream,Term) :-
	writeq(Stream,Term).


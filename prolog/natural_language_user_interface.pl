%This is a semantic parser for SWI-Prolog.
%Use the verb_synonym predicate to simplify the verbs in the output.
 
:- initialization(main).
:- set_prolog_flag(double_quotes, chars).  % This is for SWI 7+ to revert to the prior interpretation of quoted strings.
:- use_module(library(chr)).

main :-
	term_to_atom(b(a),B),
	writeln(b(a)),
	%Type any kind of input here to see the output! The input must be compatible with the grammar that is defined below.
	Input = "3 people are walking toward a house.",
	translate(Input,Output), writeln(Output).

downcase_string(AnyCase,DownCase) :-
    atom_string(AnyCase_,AnyCase),downcase_atom(AnyCase_,DownCase_),atom_string(DownCase_,DownCase__),string_codes(DownCase__,DownCase).

translate(Input,Output) :- phrase((statements(Output);statement(Output)),Input).

statements([Output]) -->
    statement(Output),ws,".".
 
statements([Output|Outputs]) -->
    statement(Output),ws,".",ws_,statements(Outputs).

statement(Output) -->
	verb_phrase([infinitive,Stem],Adverbs,Prep,Object,Number),
	{Output=verb_phrase([infinitive,Stem],Adverbs,Prep,Object,Number)}.

statement(Output) -->
	optional_an(expr(noun,Subject,recursive(true))),ws,verb_phrase(Verb,Adverbs,Prep,Object,Number),
	{Output=describes(Subject,verb_phrase(Verb,adverbs(Adverbs),Prep,Object,plural))};

    (expr(bool,B,recursive(true)),ws_,synonym("if"),ws_,expr(bool,A,recursive(true));expr(bool,A,recursive(true)),ws_,synonym("implies"),ws_,expr(bool,B);synonym("if"),ws_,expr(bool,A,recursive(true)),(ws,",",ws_;ws_),"then",ws_,expr(bool,B,recursive(true))),{Output=[A,"implies",B]}.

statement(Output) -->
	optional_an(expr(noun,Subject,recursive(true))),ws_,coordinating_conjunctions(A),
	{Output=describes(Subject,A)}.

coordinating_conjunctions([verb_phrase(Verb,Adverbs,Prep,Object,Number)]) --> verb_phrase(Verb,Adverbs,Prep,Object,Number,recursive(true)).
coordinating_conjunctions([verb_phrase(Verb,Adverbs,Prep,Object,Number)|B]) --> verb_phrase(Verb,Adverbs,Prep,Object,Number,recursive(true)),ws_,synonym("and"),ws_,coordinating_conjunctions(B).

verb_phrase(Verb_,Adverbs,preposition(Prep),Object,Number,recursive(Recursive)) -->
	(verb_tense(Verb,Adverbs,Number),{Prep=none};
	verb_tense(Verb,Adverbs,Number),ws_,preposition(Prep),ws_,optional_an(expr(noun,Object,recursive(Recursive)));
	verb_tense(Verb,Adverbs,Number),ws_,preposition(Prep),ws_,optional_an(expr(noun,Object,recursive(Recursive)));
	verb_tense(Verb,Adverbs,Number),ws_,optional_an(expr(noun,Object,recursive(Recursive))),{Prep=none};
	passive_verb_tense(Verb,Adverbs,Number),{Prep=none};
	passive_verb_tense(Verb,Adverbs,Number),ws_,"by",ws_,optional_an(expr(noun,Object,recursive(Recursive)))),
	{get_verb_synonym(Verb,Verb_)}.

verb_phrase(Verb,Adverbs,Prep,Object,Number) --> verb_phrase(Verb,Adverbs,Prep,Object,Number,recursive(true)).

expr(A,B,recursive(false)) --> noun_phrase(A,B,_).
expr(A,B,recursive(false)) --> parentheses_expr(A,B).

noun_phrase(_,singular(describes(Noun,adjectives(Adj))),singular) -->
	adjectives(Adj),ws_,noun(Noun).
noun_phrase(_,plural(describes(Noun,adjectives(Adj))),plural) -->
	adjectives(Adj),ws_,plural(Noun).

noun_phrase(_,adjective(Adj),_) -->
	adjective(Adj).

prepositional_phrase(expr(A,B),preposition(Prep),expr(C,D)) -->
	expr(A,B,recursive(false)),ws_,preposition(Prep),ws_,optional_an(expr(C,D,recursive(true))).

%series of adjectives like "light blue"
adjectives([present_tense(A)]) --> present_tense(A).
adjectives([adjective(A)]) --> adjective(A).
adjectives([present_tense(A)|B]) --> present_tense(A),ws_,adjectives(B).
adjectives([adjective(A)|B]) --> adjective(A),ws_,adjectives(B).

adverbs(adverbs(A)) --> adverbs_(A).
adverbs_([A|B]) --> adverb(A),ws_,("and";"but"),ws,adverbs_(B).
adverbs_([A]) --> adverb(A).

noun_phrase(_,plural(Noun),plural) --> plural(Noun).
noun_phrase(_,singular(Noun),singular) --> noun(Noun).

expr(noun,Output,recursive(true)) -->
	"to",ws_,verb_phrase([infinitive,Stem],Adverbs,Prep,Object,Number),
	{Output=verb_phrase([infinitive,Stem],Adverbs,Prep,Object,Number)}.

expr(_,prepositional_phrase(A,B,C),recursive(true)) -->
	prepositional_phrase(A,B,C).

expr(_,gerund(A),recursive(true)) -->
	present_tense(A).
 
expr(noun,Output,recursive(true)) -->
	%"A that is B"
	(
		(expr(noun,Subject,recursive(false))),ws_,("that";"who"),ws_,verb_phrase(Verb,Adverbs,Prep,Object,singular,recursive(Recursive))
	),
	{active_to_passive(Verb,Verb3),Output=describes(Subject,verb_phrase(Verb3,adverb(Adverbs),Prep,Object,singular,recursive(Recursive)))}.

expr(noun,Output,recursive(true)) -->
	parentheses_expr(int,Number),ws_,expr(noun,Thing,recursive(false)),
	{Output = number_of(Number,Thing)}.

expr(noun,Output,recursive(true)) -->
	(
		optional_an(noun_phrase(thing,Subject,Number)),ws_,"that",ws_,optional_an(noun_phrase(thing,Object,Number)),ws_,verb_tense(Verb,Adverbs,Number);
		optional_an(noun_phrase(thing,Subject,Number)),ws_,"that",ws_,optional_an(noun_phrase(thing,Object,Number)),ws_,verb_tense(Verb,Adverbs,Number),ws_,preposition(Prep)
	),
	{active_to_passive(Verb,Verb3),Output=describes(Subject,verb_phrase(Verb3,adverb(Adverbs),Prep,Object,singular))}.

expr(bool,Output,recursive(true)) -->
	%"A eats (preposition) B"
		optional_an(noun_phrase(thing,singular(Subject),singular)),ws_,verb_phrase(Verb,Adverbs,Prep,Object,singular),
	{Output=describes(Subject,verb_phrase(Verb,adverbs(Adverbs),Prep,Object,singular))};

		optional_an(noun_phrase(thing,plural(Subject),plural)),ws_,verb_phrase(Verb,Adverbs,Prep,Object,plural),
		
	{Output=describes(Subject,verb_phrase(Verb,adverbs(Adverbs),Prep,Object,plural))}.

active_to_passive(Active,Passive) :-
	Passive = [A,passive,B],Active=[A,B].

expr(bool,Output,recursive(true)) -->
	%"A likes B and vice versa"
	(
		optional_an(expr(noun,Subject,recursive(false))),ws_,verb_tense(Verb,Adverbs,singular),ws_,optional_an(expr(noun,Object,recursive(true))),ws_,"and",ws_,"vice",ws_,"versa";
		expr(noun,Object,recursive(false)),ws_,passive_verb_tense(Verb,Adverbs,singular),ws_,"by",ws_,expr(noun,Subject,recursive(true)),ws_,"and",ws_,"vice",ws_,"versa";
		expr(noun,Subject,recursive(false)),ws_,"and",ws_,expr(noun,Object,recursive(true)),ws_,verb_tense(Verb,Adverbs,plural),ws_,synonym("each other")
	),
	{Output=[[Subject,Verb,Object],and,[Object,Verb,Subject]]}.
 
expr(bool,Output,recursive(true)) -->
    %{comparative_adjectives(Synonym,Antonym,Noun)},
    %(  
    %   optional_an(expr(noun,A,recursive(false))),ws_,"is",ws_,comparative_adjective(Synonym),ws_,"than",ws_,optional_an(expr(noun,B,recursive(true)));
    %   optional_an(expr(noun,B,recursive(false))),ws_,"is",ws_,comparative_adjective(Antonym),ws_,"than",ws_,optional_an(expr(noun,A,recursive(true)))
    %),{Output=[[Noun,of,A],'>',[Noun,of,B]]};
    
    expr(bool,Output,recursive(false));
    ({Type=int;Type=noun;Type=color},
    (
        (
            expr(Type,A,recursive(false)),ws,synonym("!="),ws,expr(Type,B,recursive(true));
            expr(Type,A,recursive(false)),ws_,"and",ws_,noun_phrase(Type,B,singular),ws_,"are",ws_,"not",ws_,synonym("equal")
        ),{Output=[A,'!=',B]};
        (
            expr(Type,A,recursive(false)),ws,synonym("="),ws,expr(Type,B,recursive(true));
            expr(Type,A,recursive(false)),ws_,"and",ws_,expr(Type,B,recursive(true)),ws_,"are",ws_,synonym("equal")
        ),{Output=[A,'=',B]}
    ));
     
    {inverse_preposition(Synonym,Antonym)},
    (   
        optional_an(expr(noun,A,recursive(false))),ws_,"is",ws_,synonym(Synonym),ws_,optional_an(expr(noun,B,recursive(true)));
        optional_an(expr(noun,B,recursive(false))),ws_,"is",ws_,synonym(Antonym),ws_,optional_an(expr(noun,A,recursive(true)))
    ),{Output=[A,'is',Synonym,B]};
 
     
    (parentheses_expr(int,A),ws_,synonym(">"),ws_,expr(int,B,recursive(true))),{Output=[A,>,B]};
    (parentheses_expr(int,A),ws_,synonym("<"),ws_,expr(int,B,recursive(true))),{Output=[A,<,B]};
    (parentheses_expr(bool,A),ws_,"and",ws_,expr(bool,B,recursive(true))),{Output=[A,and,B]};
    (parentheses_expr(bool,A),ws_,"and",ws_,expr(bool,B,recursive(true)),ws_,"are",ws_,synonym("equal")),{Output=[A,=,B]};
    (parentheses_expr(bool,A),ws_,"or",ws_,expr(bool,B,recursive(true))),{Output=[A,or,B]};
    (parentheses_expr(int,A),ws_,synonym("<="),ws_,expr(int,B,recursive(true))),{Output=[A,'<=',B]}.
 

expr(int,Output,recursive(true)) -->
    parentheses_expr(int,Output);
     
    (parentheses_expr(int,A),ws_,"squared"),{Output=[A*A]};
     
    (
        parentheses_expr(int,A),ws,synonym("+"),ws,expr(int,B,recursive(true));
        optional_prefix("the","sum"),ws_,"of",ws_,parentheses_expr(int,A),ws_,"and",ws_,expr(int,B,recursive(true))
    ),{Output=[A,+,B]};
     
    (
        parentheses_expr(int,A),ws,synonym("^"),ws,expr(int,B,recursive(true));
        parentheses_expr(int,A),ws_,"to",ws_,"the",ws_,"power",ws_,"of",ws_,expr(int,B,recursive(true))
    ),{Output=[A,^,B]};
    
    (
        parentheses_expr(int,A),ws,synonym("/"),ws,expr(int,B,recursive(true));
        optional_prefix("the","quotient"),ws_,"of",ws_,parentheses_expr(int,A),ws_,"and",ws_,expr(int,B,recursive(true))
    ),{Output=[A,'/',B]};
     
    (
        parentheses_expr(int,A),ws,synonym("*"),ws,expr(int,B,recursive(true));
        optional_prefix("the","product"),ws_,"of",ws_,parentheses_expr(int,A),ws_,"and",ws_,expr(int,B,recursive(true))
    ),{Output=[A,*,B]}.
 
expr(noun,A,recursive(true)) --> expr(int,A,recursive(true)).
expr(noun,A,recursive(true)) --> noun_phrase(thing,A,_).
 
expr(noun,Output,recursive(true)) --> (
    optional_prefix("the",noun_phrase(noun,A,singular)),ws_,"of",ws_,optional_an(expr(noun,B,recursive(false)));
    optional_an(noun_phrase(noun,B,singular)),ws,"'s",ws_,expr(noun,A,recursive(false))
),{Output=[A,of,B]}.
 
expr(bool,Output,recursive(true)) --> (
    noun_phrase(noun,A,singular),ws_,"is",ws_,"not",ws_,synonym("an"),ws_,parentheses_expr(noun,B)
),{Output=[[A,is,a,B],=,false]}.
 
expr(bool,Output,recursive(true)) -->
    optional_an(parentheses_expr(noun,A)),ws_,"and",ws_,optional_an(parentheses_expr(noun,B)),ws_,"have",ws_,"the",ws_,"same",ws_,expr(noun,C,recursive(true)),{Output=[[C,of,A],=,[C,of,B]]}.
 
expr(bool,Output,recursive(true)) -->
    optional_an(parentheses_expr(noun,A)),ws_,"and",ws_,optional_an(parentheses_expr(noun,B)),ws_,"have",ws_,synonym("different"),ws_,plural(expr(noun,C,recursive(true))),{Output=[[C,of,A],'!=',[C,of,B]]}.
 
adjective(A) --> parentheses_expr(color,A).

adjective(Output) --> comparative_adjective(Adj),ws_,"than",ws_,expr(noun,A,recursive(false)),{Output=comparative_adjective(Adj,A)}.

adjective(A) --> adjective_synonym(A).
adjective(A) --> symbol(A),{same_adjective_declension(A,A)}.

parentheses_expr(color,A) --> symbol(A),{member(A,["red","green","blue","purple","orange","pink","yellow","white","black","transparent","brown"])}.

expr(noun,singular_or_plural(A),recursive(true)) --> singular_or_plural(A).
 
parentheses_expr(noun,singular_or_plural(A)) --> singular_or_plural(A).
 
parentheses_expr(noun,singular(A)) --> noun(A).

unit_of_length(A) --> symbol(A),{member(A,["foot","meter","inch","centimeter","mile","kilometer","light-year"])}.
unit_of_time(A) --> symbol(A),{member(A,["day","hour","year","second","month","minute","millisecond","microsecond","nanosecond","picosecond","femtosecond"])}.

unit_of_mass(A) --> symbol(A),{member(A,["kilogram","pound","ton", "gram"])}.

parentheses_expr(adjective,A) --> adjective(A).
adjective(measurement([X,Unit,"tall"])) --> a_number(X),ws_,unit_of_length(Unit),ws_,"tall".
adjective(measurement([X,Unit,"long"])) --> a_number(X),ws_,unit_of_length(Unit),ws_,"long".
adjective(measurement([X,Unit,"wide"])) --> a_number(X),ws_,unit_of_length(Unit),ws_,"wide".
adjective(measurement([X,Unit,"old"])) --> a_number(X),ws_,unit_of_time(Unit),ws_,"old".
adjective(measurement([X,Unit])) --> a_number(X),ws_,unit_of_mass(Unit).
 
singular_or_plural(plural(X)) -->
    plural(X).
 
singular_or_plural(singular(X)) -->
    noun(X).

parentheses_expr(noun,Output) -->
    (plural(Output);noun(Output);"(",ws,expr(noun,Output,recursive(true)),ws,")").
 
parentheses_expr(int,Output) -->
    a_number(Output);"(",ws,expr(int,Output,recursive(true)),ws,")".

parentheses_expr(int,[1]) -->
    "one".

parentheses_expr(int,[2]) -->
    "two".

parentheses_expr(int,[3]) -->
    "three".

parentheses_expr(int,[4]) -->
    "four".

parentheses_expr(int,[5]) -->
    "five".

parentheses_expr(int,[6]) -->
    "six".

parentheses_expr(int,[7]) -->
    "seven".

parentheses_expr(int,[8]) -->
    "eight".

parentheses_expr(int,[9]) -->
    "nine".

parentheses_expr(int,[1,0]) -->
    "ten".

parentheses_expr(int,[1,1]) -->
    "eleven".

parentheses_expr(int,[1,2]) -->
    "twelve".

parentheses_expr(int,[1,3]) -->
    "thirteen".

parentheses_expr(int,[1,4]) -->
    "fourteen".

parentheses_expr(int,[1,5]) -->
    "fifteen".

parentheses_expr(int,[1,6]) -->
    "sixteen".

parentheses_expr(int,[1,7]) -->
    "seventeen".

parentheses_expr(int,[1,8]) -->
    "eighteen".

parentheses_expr(int,[1,9]) -->
    "nineteen".

parentheses_expr(int,[2,0]) -->
    "twenty".

parentheses_expr(int,[3,0]) -->
    "thirty".

parentheses_expr(int,[4,0]) -->
    "forty".

parentheses_expr(int,[5,0]) -->
    "fifty".

parentheses_expr(int,[6,0]) -->
    "sixty".

parentheses_expr(int,[7,0]) -->
    "seventy".

parentheses_expr(int,[8,0]) -->
    "eighty".

parentheses_expr(int,[9,0]) -->
    "ninety".

% whitespace
ws --> "";((" ";"\t";"\n";"\r"),ws).
ws_ --> (" ";"\n";"\r"),ws.

 
a_number(A) --> a_double(A);an_int(A).
a_double([A,B]) -->
        (an_int(A), ".", an_int(B)).
an_int([L|Ls]) --> digit(L), an_int_r(Ls).
an_int_r([L|Ls]) --> digit(L), an_int_r(Ls).
an_int_r([])     --> [].
digit(Let)     --> [Let], { code_type(Let, digit) }.
 
symbol([L|Ls]) --> letter(L), symbol_r(Ls).
symbol_r([L|Ls]) --> csym(L), symbol_r(Ls).
symbol_r([])     --> [].
letter(Let)     --> [Let], { code_type(Let, alpha) }.
csym(Let)     --> [Let], {code_type(Let, csym)}.
optional_an(A) --> optional_prefix(("an";"a";"the";"A";"An";"The"),A).
 
 
optional_prefix(Prefix,A) --> A;Prefix,ws_,A.
 
inverse_preposition("above","below").
 
comparative_adjectives("tall","short","height").
comparative_adjectives("strong","weak","strength").
comparative_adjectives("old","young","age").
comparative_adjectives("large","small","size").
comparative_adjectives("happy","sad","happiness").
comparative_adjectives("hard","soft","hardness").
 
 

comparative_adjective(X) -->
    symbol(X),"der",
    {same_adjective_declension(X,"sad")};
    
    symbol(X),"r",
    {same_adjective_declension(X,"large")};
    
    symbol(X),"ter",
    {same_adjective_declension(X,"fat")};
    
    symbol(X),"er",
    {same_adjective_declension(X,"tall")};
    
    "more",ws_,symbol(X),{same_adjective_declension(X,_)}.
 
last_char(S_, X) :-
    atom_codes(S,S_),
    name(S, N),
    reverse(N, [F|_]),
    name(X, [F]).


adverb(X) -->
	symbol(X),
	{member(X,["also","now"])};
	"happily",
	{X="happy"};
	symbol(X),"ly",
	{member(X,["large","nice","safe","gentle"]);same_adjective_declensions(X,["great","huge"])}.

plural(X) -->
    same_noun_declension(X,"sheep");
    "wolves",
		{same_noun_declension(X,"wolf")};
    same_noun_declension(X,"fish"),
		"es";
    same_noun_declension(X,"dog"),
		"s";
    "people",
    {same_noun_declension(X,"person")}.

same_conjugations(A,B) --> symbol(A),{same_conjugations(A,B)}.
same_noun_declension(A,B) --> symbol(A),{same_noun_declension(A,B)}.


present_tense(X) -->
    same_conjugations(X,["talk","miss","writ","stat","eat","read","pee"]),
		"ing";
    same_conjugations(X,["drop"]),
		"ping";
    same_conjugations(X,["empt"]),
		"ying";
    same_conjugations(X,["sit"]),
		"ting";
    same_conjugations(X,["run"]),
		"ning"; 
    "is",
    {X="be"};
    "has",
    {X="have"}.

past_tense(X) -->
	"was",
	{X="be"};
	"flew",
	{X="fly"};
	"drank",
	{X="drink"};
	same_conjugations(X,["talk","stat","hiss"]),"ed";
	same_conjugations(X,["drop"]),"ped";
	"ran",
	{same_conjugations(X,["run"])};
	same_conjugations(X,["pee"]),"d";
	"ate",
	{same_conjugations(X,["eat"])};
	"wrote",
	{same_conjugations(X,["writ"])};
	"spoke",
	{same_conjugations(X,["speak"])};
	same_conjugations(X,["read"]);
	"sat",
	{same_conjugations(X,["sit"])};
	"had",
	{X="have"};
	same_conjugations(X,["tr"]),
		"ied".
     
passive_verb(X) -->
    same_conjugations(X,["eat"]),
		"en";
    past_tense(X),
		{same_conjugations(X,["miss","drop","pee","read","writ","drop","tr"])};
    same_conjugations(X,["read","run"]).
 
present_simple_verb(X) -->
    same_conjugations(X,["work","eat","read","writ","speak","pee","drop"]),
		"s";
    same_conjugations(X,["lov","miss"]),
		"es";
    "is",
		{X="be"};
    "has",
		{X="have"};
	same_conjugations(X,["tr"]),
		"ies".
 
infinitive(X) -->
    same_conjugations(X,["work","pee","read","eat","speak","miss"]);
    same_conjugations(X,["writ","lik"]),
		"e";
    "be",
		{X="be"};
    "have",
		{X="have"};
	same_conjugations(X,["tr"]),
		"y".

verb_tense(Verb,Adverbs,X) -->
	adverbs(Adverbs),ws_,verb_tense(Verb,X);verb_tense(Verb,X),{Adverbs=adverbs(none)};verb_tense(Verb,X),ws_,adverbs(Adverbs).

passive_verb_tense(Verb,Adverbs,X) -->
	adverbs(Adverbs),ws_,passive_verb_tense(Verb,X);passive_verb_tense(Verb,X);passive_verb_tense(Verb,X),ws_,adverbs(Adverbs).

verb_tense([infinitive,X],_) -->
    infinitive(X).
 
%singular or plural
verb_tense([future_tense,X],_) -->
    "will",ws_,infinitive(X).
 
verb_tense([past_tense,X],singular) -->
    "was",ws_,present_tense(X);"has",ws_,past_tense(X);past_tense(X).
verb_tense([past_tense,X],singular) -->
    "were",ws_,present_tense(X);"have",ws_,past_tense(X);past_tense(X).

verb_tense([present_tense,X],singular) -->
    "is",ws_,present_tense(X).
verb_tense([present_tense,X],plural) -->
    "are",ws_,present_tense(X).
 
verb_tense([present_simple_verb,X],singular) -->
    present_simple_verb(X).
 
passive_verb_tense([present_simple_verb,passive,X],singular) -->
	"is",ws_,past_tense(X).
passive_verb_tense([present_simple_verb,passive,X],plural) -->
	"are",ws_,past_tense(X).

passive_verb_tense([past_tense,passive,X],singular) -->
	"was",(ws_;ws_,"being",ws_),passive_verb(X).

passive_verb_tense([past_tense,passive,X],plural) -->
	"are",(ws_;ws_,"being",ws_),passive_verb(X).

passive_verb_tense([future_tense,passive,X],_) -->
	"will",ws_,"be",ws_,passive_verb(X).

passive_verb_tense([present_tense,passive,X],singular) -->
	"is",ws_,"being",ws_,passive_verb(X).

passive_verb_tense([present_tense,passive,X],plural) -->
	"are",ws_,"being",ws_,passive_verb(X).
 
%is_are(Tense,Person,Number)
is_are(past,plural,first_person) -->
    "were".
 
is_are(past,singular,first_person) -->
    "was".
 
is_are(present,singular,second_person) -->
    "was".
 
is_are(past,singular,second_person) -->
    "were".
 
is_are(future,_,_) -->
    "will",ws_,"be".

preposition(X) -->
    {member(X,["for","among","through","from","like","between","during","outside","in","up","down","inside","within","on","to","into","onto","with","over","under","underneath","beneath","upon","below","near","beside","toward",(("next";"close"),ws_,"to")])},X.

%X has the same conjugation as one of the words in this list
same_conjugations(X,List) :-
    member(Y,List),
    same_conjugation(X,Y).

same_adjective_declensions(X,List) :-
	member(Y,List),
	same_adjective_declension(X,Y).

get_verb_synonym(Synonym,Output) :-
    Synonym = [Tense,Synonym1],verb_synonym(Synonym1,Synonym2),Output=[Tense,Synonym2];
    Synonym=Output.

 
noun(X) --> symbol(X),{same_noun_declension(X,_)}.

:- include(thesaurus).

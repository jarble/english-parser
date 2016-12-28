%This is a semantic parser for SWI-Prolog.
%Use the verb_synonym predicate to simplify the verbs in the output.
 
:- initialization(main).
:- set_prolog_flag(double_quotes, chars).  % This is for SWI 7+ to revert to the prior interpretation of quoted strings.
:- use_module(library(chr)).

main :-
	term_to_atom(b(a),B),
	writeln(b(a)),
    %Type any kind of input here to see the output! The input must be compatible with the grammar that is defined below.
    Input = "A river is flowing.",
    translate(Input,Output), writeln(Output).

downcase_string(AnyCase,DownCase) :- 
    atom_string(AnyCase_,AnyCase),downcase_atom(AnyCase_,DownCase_),atom_string(DownCase_,DownCase__),string_codes(DownCase__,DownCase).

translate(Input,Output) :- phrase((statements(Output);statement(Output)),Input).

statements([Output]) -->
    statement(Output),ws,".".
 
statements([Output|Outputs]) -->
    statement(Output),ws,".",ws_,statements(Outputs).
 
statement(Output) -->
	
	optional_an(expr(thing,Subject)),ws,verb_phrase(Verb,Adverbs,Prep,Object,Number),
	{get_verb_synonym(Verb,Verb2),Output=describes(Subject,verb_phrase(Verb2,adverbs(Adverbs),Prep,Object,plural))};

    expr(bool,Output);
    (expr(bool,B),ws_,synonym("if"),ws_,expr(bool,A);expr(bool,A),ws_,synonym("implies"),ws_,expr(bool,B);synonym("if"),ws_,expr(bool,A),(ws,",",ws_;ws_),"then",ws_,expr(bool,B)),{Output=[A,"implies",B]};
    
    optional_an(expr(thing,Subject)),ws,coordinating_conjunctions(A),
	{get_verb_synonym(Verb,Verb2),Output=describes(Subject,A)}.

coordinating_conjunctions([verb_phrase(Verb,Adverbs,Prep,Object,Number)]) --> verb_phrase(verb_synonym(Verb),Adverbs,Prep,Object,Number,recursive(false)).
coordinating_conjunctions([verb_phrase(Verb,Adverbs,Prep,Object,Number)|B]) --> verb_phrase(verb_synonym(Verb),Adverbs,Prep,Object,Number,recursive(false)),ws_,"and",ws_,coordinating_conjunctions(B).

verb_phrase(Verb,Adverbs,preposition(Prep),Object,Number,recursive(Recursive)) -->
	verb_tense(Verb,Adverbs,Number),{Prep=none};
	verb_tense(Verb,Adverbs,Number),ws_,preposition(Prep),ws_,optional_an(expr(thing,Object,recursive(Recursive)));
	verb_tense(Verb,Adverbs,Number),ws_,optional_an(expr(thing,Object,recursive(Recursive))),{Prep=none};
	passive_verb_tense(Verb,Adverbs,Number),{Prep=none};
	passive_verb_tense(Verb,Adverbs,Number),ws_,"by",ws_,optional_an(expr(thing,Object,recursive(Recursive))).

verb_phrase(Verb,Adverbs,Prep,Object,Number) --> verb_phrase(Verb,Adverbs,Prep,Object,Number,recursive(true)).

expr(A,B,recursive(true)) --> expr(A,B).
expr(A,B,recursive(false)) --> noun_phrase(A,B).

noun_phrase(_,singular(describes(Noun,adjectives(Adj)))) -->
	adjectives(Adj),ws_,noun(Noun).
noun_phrase(_,plural(describes(Noun,adjectives(Adj)))) -->
	adjectives(Adj),ws_,plural(Noun).

noun_phrase(_,adjective(Adj)) -->
	adjective(Adj).

%series of adjectives like "light blue"
adjectives([present_tense(A)]) --> present_tense(A).
adjectives([adjective(A)]) --> adjective(A).
adjectives([present_tense(A)|B]) --> present_tense(A),ws_,adjectives(B).
adjectives([adjective(A)|B]) --> adjective(A),ws_,adjectives(B).

adverbs(adverbs(A)) --> adverbs_(A).
adverbs_([A|B]) --> adverb(A),ws_,("and";"but"),ws,adverbs_(B).
adverbs_([A]) --> adverb(A).

noun_phrase(_,plural(Noun)) --> plural(Noun).
noun_phrase(_,singular(Noun)) --> noun(Noun).
 
expr(thing,Output) -->
	%"A that is B"
	(
		(noun_phrase(thing,Subject)),ws_,("that";"who"),ws_,verb_phrase(Verb,Adverbs,Prep,Object,singular)
	),
	{get_verb_synonym(Verb,Verb2),active_to_passive(Verb2,Verb3),Output=describes(Subject,verb_phrase(Verb3,adverb(Adverbs),Prep,Object,singular))}.

expr(thing,Output) -->
	(
		optional_an(noun_phrase(thing,Subject)),ws_,"that",ws_,optional_an(noun_phrase(thing,Object)),ws_,verb_tense(Verb,Adverbs,Number);
		optional_an(noun_phrase(thing,Subject)),ws_,"that",ws_,optional_an(noun_phrase(thing,Object)),ws_,verb_tense(Verb,Adverbs,Number),ws_,preposition(Prep)
	),
	{get_verb_synonym(Verb,Verb2),active_to_passive(Verb2,Verb3),Output=describes(Subject,verb_phrase(Verb3,adverb(Adverbs),Prep,Object,singular))}.

expr(bool,Output) -->
	%"A eats (preposition) B"
		optional_an(noun_phrase(thing,singular(Subject))),ws_,verb_phrase(Verb,Adverbs,Prep,Object,singular),
	{get_verb_synonym(Verb,Verb2),Output=describes(Subject,verb_phrase(Verb2,adverbs(Adverbs),Prep,Object,singular))};

		optional_an(noun_phrase(thing,plural(Subject))),ws_,verb_phrase(Verb,Adverbs,Prep,Object,plural),
		
	{get_verb_synonym(Verb,Verb2),Output=describes(Subject,verb_phrase(Verb2,adverbs(Adverbs),Prep,Object,plural))}.

active_to_passive(Active,Passive) :-
	Passive = [A,passive,B],Active=[A,B].

expr(bool,Output) -->
	%"A likes B and vice versa"
	(
		optional_an(parentheses_expr(thing,Subject)),ws_,verb_tense(Verb,Adverbs,singular),ws_,optional_an(expr(thing,Object)),ws_,"and",ws_,"vice",ws_,"versa";
		parentheses_expr(thing,Object),ws_,passive_verb_tense(Verb,Adverbs,singular),ws_,"by",ws_,expr(thing,Subject),ws_,"and",ws_,"vice",ws_,"versa";
		parentheses_expr(thing,Subject),ws_,"and",ws_,expr(thing,Object),ws_,verb_tense(Verb,Adverbs,plural),ws_,synonym("each other")
	),
	{get_verb_synonym(Verb,Verb2),Output=[[Subject,Verb2,Object],and,[Object,Verb2,Subject]]}.
 
expr(bool,Output) -->
    parentheses_expr(bool,Output);
    ({Type=int;Type=noun;Type=color},
    (
        (
            noun_phrase(Type,A),ws,synonym("!="),ws,expr(Type,B);
            noun_phrase(Type,A),ws_,"and",ws_,noun_phrase(Type,B),ws_,"are",ws_,"not",ws_,synonym("equal")
        ),{Output=[A,'!=',B]};
        (
            noun_phrase(Type,A),ws,synonym("="),ws,expr(Type,B);
            noun_phrase(Type,A),ws_,"and",ws_,expr(Type,B),ws_,"are",ws_,synonym("equal")
        ),{Output=[A,'=',B]}
    ));
     
    %{comparative_adjective(Synonym,Antonym,Noun)},
    %(  
    %   optional_an(noun_phrase(thing,A)),ws_,"is",ws_,comparative_adjective(Synonym),ws_,"than",ws_,optional_an(expr(thing,B));
    %   optional_an(noun_phrase(thing,B)),ws_,"is",ws_,comparative_adjective(Antonym),ws_,"than",ws_,optional_an(expr(thing,A))
    %),{Output=[[Noun,of,A],'>',[Noun,of,B]]};
     
    {inverse_preposition(Synonym,Antonym)},
    (   
        optional_an(parentheses_expr(thing,A)),ws_,"is",ws_,synonym(Synonym),ws_,optional_an(expr(thing,B));
        optional_an(parentheses_expr(thing,B)),ws_,"is",ws_,synonym(Antonym),ws_,optional_an(expr(thing,A))
    ),{Output=[A,'is',Synonym,B]};
 
     
    (parentheses_expr(int,A),ws_,synonym(">"),ws_,expr(int,B)),{Output=[A,>,B]};
    (parentheses_expr(int,A),ws_,synonym("<"),ws_,expr(int,B)),{Output=[A,<,B]};
    (parentheses_expr(bool,A),ws_,"and",ws_,expr(bool,B)),{Output=[A,and,B]};
    (parentheses_expr(bool,A),ws_,"and",ws_,expr(bool,B),ws_,"are",ws_,synonym("equal")),{Output=[A,=,B]};
    (parentheses_expr(bool,A),ws_,"or",ws_,expr(bool,B)),{Output=[A,or,B]};
    (parentheses_expr(int,A),ws_,synonym("<="),ws_,expr(int,B)),{Output=[A,'<=',B]}.
 
expr(thing,A) --> expr(noun,A).

expr(int,Output) -->
    parentheses_expr(int,Output);
     
    (parentheses_expr(int,A),ws_,"squared"),{Output=[A*A]};
     
    (
        parentheses_expr(int,A),ws,synonym("+"),ws,expr(int,B);
        optional_prefix("the","sum"),ws_,"of",ws_,parentheses_expr(int,A),ws_,"and",ws_,expr(int,B)
    ),{Output=[A,+,B]};
     
    (
        parentheses_expr(int,A),ws,synonym("^"),ws,expr(int,B);
        parentheses_expr(int,A),ws_,"to",ws_,"the",ws_,"power",ws_,"of",ws_,expr(int,B)
    ),{Output=[A,^,B]};
    
    (
        parentheses_expr(int,A),ws,synonym("/"),ws,expr(int,B);
        optional_prefix("the","quotient"),ws_,"of",ws_,parentheses_expr(int,A),ws_,"and",ws_,expr(int,B)
    ),{Output=[A,'/',B]};
     
    (
        parentheses_expr(int,A),ws,synonym("*"),ws,expr(int,B);
        optional_prefix("the","product"),ws_,"of",ws_,parentheses_expr(int,A),ws_,"and",ws_,expr(int,B)
    ),{Output=[A,*,B]}.
 
expr(noun,A) --> expr(int,A).
expr(noun,A) --> noun_phrase(thing,A).
 
expr(noun,Output) --> (
    optional_prefix("the",noun_phrase(noun,A)),ws_,"of",ws_,optional_an(expr(noun,B));
    optional_an(noun_phrase(noun,B)),ws,"'s",ws_,expr(noun,A)
),{Output=[A,of,B]}.
 
expr(bool,Output) --> (
    noun_phrase(noun,A),ws_,"is",ws_,"not",ws_,synonym("an"),ws_,parentheses_expr(noun,B)
),{Output=[[A,is,a,B],=,false]}.
 
expr(bool,Output) -->
    optional_an(parentheses_expr(noun,A)),ws_,"and",ws_,optional_an(parentheses_expr(noun,B)),ws_,"have",ws_,"the",ws_,"same",ws_,expr(noun,C),{Output=[[C,of,A],=,[C,of,B]]}.
 
expr(bool,Output) -->
    optional_an(parentheses_expr(noun,A)),ws_,"and",ws_,optional_an(parentheses_expr(noun,B)),ws_,"have",ws_,synonym("different"),ws_,plural(expr(noun,C)),{Output=[[C,of,A],'!=',[C,of,B]]}.
 
adjective(A) --> parentheses_expr(color,A).
adjective(A) --> adjective_synonym(A).
adjective(A) --> symbol(A),{same_adjective_declension(A,A)}.

parentheses_expr(color,A) --> symbol(A),{member(A,["red","green","blue","purple","orange","pink","yellow","white","black","transparent","brown"])}.
 
expr(thing,singular_or_plural(A)) --> singular_or_plural(A).
 
parentheses_expr(thing,singular_or_plural(A)) --> singular_or_plural(A).
 
parentheses_expr(thing,singular(A)) --> noun(A).

unit_of_length(A) --> symbol(A),{member(A,["foot","meter","inch","centimeter","mile","kilometer","light-year"])}.
unit_of_time(A) --> symbol(A),{member(A,["day","hour","year","second","month","minute","millisecond","microsecond","nanosecond","picosecond","femtosecond"])}.

unit_of_mass(A) --> symbol(A),{member(A,["kilogram","pound","ton", "gram"])}.

parentheses_expr(adjective,A) --> adjective(A).
adjective(measurement([X,Unit,tall])) --> a_number(X),ws_,unit_of_length(Unit),ws_,"tall".
adjective(measurement([X,Unit,long])) --> a_number(X),ws_,unit_of_length(Unit),ws_,"long".
adjective(measurement([X,Unit,wide])) --> a_number(X),ws_,unit_of_length(Unit),ws_,"wide".
adjective(measurement([X,Unit,old])) --> a_number(X),ws_,unit_of_time(Unit),ws_,"old".
adjective(measurement([X,Unit])) --> a_number(X),ws_,unit_of_mass(Unit).
 
singular_or_plural(plural(X)) -->
    plural(X).
 
singular_or_plural(singular(X)) -->
    noun(X).
 
parentheses_expr(Type,Output) -->
    (plural(Output);noun(Output);"(",ws,expr(Type,Output),ws,")").
 
parentheses_expr(int,Output) -->
    a_number(Output).
 
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
 
comparative_adjective("tall","short","height").
comparative_adjective("strong","weak","strength").
comparative_adjective("old","young","age").
comparative_adjective("big","small","size").
comparative_adjective("happy","sad","happiness").
 
 
comparative_adjective(X) -->
    {last_char(X,'d')} -> X,"der";
    X,"er";
    "more",ws_,X.
 
last_char(S_, X) :-
    atom_codes(S,S_),
    name(S, N),
    reverse(N, [F|_]),
    name(X, [F]).


adverb(X) -->
	symbol(X),
	{member(X,["also"])};
	"happily",
	{X="happy"};
	symbol(X),"ly",
	{member(X,["large","nice","safe","gentle"]);same_adjective_declensions(X,["great","huge"])}.

plural(X) -->
    symbol(X),{same_noun_declension(X,"sheep")};
    {same_noun_declension(X,"apple")};
     
    "wolves",
    {same_noun_declension(X,"wolf")};
     
    symbol(X),"es",
    {same_noun_declension(X,"fish")};
     
    symbol(X),"s",
    {same_noun_declension(X,"dog")};
     
    "people",
    {same_noun_declension(X,"person")}.
 
present_tense(X) -->
    symbol(X),"ing",
    {same_conjugations(X,["talk","miss","writ","stat","eat","read","pee"])};
     
    symbol(X),"ping",
    {same_conjugations(X,["drop"])};
     
    symbol(X),"ting",
    {same_conjugations(X,["sit"])};
     
    symbol(X),"ning",
    {same_conjugations(X,["run"])};
     
    "is",
    {X="be"};
     
    "has",
    {X="have"}.
 
past_tense(X) -->
	symbol(X),"ed",
	{same_conjugations(X,["talk","stat","hiss"])};
	symbol(X),"ped",
	{same_conjugations(X,["drop"])};
	"ran",
	{same_conjugations(X,["run"])};
	symbol(X),"d",
	{same_conjugation(X,"pee")};
	"ate",
	{same_conjugation(X,"eat")};
	"wrote",
	{same_conjugation(X,"writ")};
	"spoke",
	{same_conjugation(X,"speak")};
	symbol(X),
	{same_conjugation(X,"read")};
	"were",
	{X="be"};
	"sat",
	{same_conjugations(X,["sit"])};
	"had",
	{X="have"}.
     
passive_verb(X) -->
    symbol(X),"en",
    {same_conjugation(X,"eat")};
    past_tense(X),
    {same_conjugations(X,["miss","drop","pee","read","writ","drop"])};
    symbol(X),
    {same_conjugations(X,["read","run"])}.
 
present_simple_verb(X) -->
    symbol(X),"s",
    {same_conjugations(X,["work","eat","read","writ","speak","pee","drop"])};
    symbol(X),"es",
    {same_conjugations(X,["lov","miss"])};
    "is",
    {X="be"};
    "has",
    {X="have"}.
 
infinitive(X) -->
    symbol(X),
    {same_conjugations(X,["work","pee","read","eat","speak"])};
    symbol(X),"e",
    {same_conjugations(X,["writ","lik"])};
    "be",
    {X="be"};
    "have",
    {X="have"}.

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
    {member(X,["for","like","between","during","outside","in","up","down","inside","within","on","to","into","onto","with","over","under","underneath","beneath","upon","below","near","beside","toward",(("next";"close"),ws_,"to")])},X.
 
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

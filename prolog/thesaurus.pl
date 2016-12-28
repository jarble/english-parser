verb_synonym(X,"mix") :- member(X,["mix","combin"]).
verb_synonym(X,"anger") :- member(X,["anger","enrag","madden"]).
verb_synonym(X,"eat") :- member(X,["eat","ingest"]).
verb_synonym(X,"urinat") :- member(X,["pee","urinat","piss"]).
verb_synonym(X,"hat") :- member(X,["hat","deplor","dislik"]).
verb_synonym(X,"mix") :- member(X,["mix","combin"]).
verb_synonym(X,"cut") :- member(X,["sever","cut"]).

%each stem is an infinitive
same_conjugation(X,Y) :-
    member(List,[
         
        %add "ing" to present tense and "e" to infinitive
        ["fak","pursu","chas","mat","mak","smash","knock","tear","enrag","combin","hat","deplor","dislik","play","bak","cak","stat","creat","hat","lik","dislik","injur","giv","lov","tak","receiv","trad","urinat"],
         
        
        ["look","spray","flow","lean","learn","destroy","wreck","insert","ruin","madden","anger","sever","walk","build","burn","talk","speak","hurt","need","burn","knead","kill","lick","suck","melt","pelt","belt","stand","ingest"],
        
        
        %add "es" to the stem in past tense
        ["mix","miss","kiss","hiss","piss"],
         
        %add "ting" to the stem for present tense
        ["sit","cut"],
         
        %add "ning" to the stem for present tense
        ["run"],
         
        %add "ped" to past tense
        ["sip","tap","drop","shop","stop","hop","mop","slap","rap"],
         
        ["writ"],
        ["fall"],
        ["read"],
        ["speak"],
        ["eat"],
        ["have"],
        ["pee"]
    ]),
    memberchk(X,List),memberchk(Y,List).
 
same_noun_declension(X,Y) :-
    member(List,[
        ["sheep"],
        ["person"],
        ["wolf"],
        ["half"],
        ["dog","bottle","water","river","apple","hound","day","cat","horse","donkey","food","truck","tree","kangaroo","wall","house","building","car","bar","beam","stream","hill","rock","stone","boulder","ship","noun","verb","pronoun"],
         
        %add "es" to plural
        ["fish","dingo","mango","walrus"]
    ]),
    memberchk(X,List),memberchk(Y,List).
 
same_adjective_declension(X,Y) :-
    member(List,[
        ["big"],
        ["large","true","false","huge","same","some"],
        
        ["small","tall","full"],
        
        ["great","slow"]
    ]),
    memberchk(X,List),memberchk(Y,List).

synonym("eat") --> "ingest".
synonym("+") --> "plus".
synonym("equal") --> "equivalent";"identical";"the",ws_,"same".
synonym("equivalent") --> synonym("equal").
synonym("identical") --> synonym("equal").
synonym("greater") --> "more";"greater".
synonym("more") --> synonym("greater").
synonym(">") --> "is",ws_,synonym("greater"),ws_,"than".
synonym("<=") --> "is",ws_,("not",ws_,synonym("greater"),ws_,"than";"less",ws_,"than",ws_,"or",ws_,synonym("equal"),ws_,"to").
synonym("<") --> "is",ws_,"less",ws_,"than".
synonym("=") --> "equals";"is",ws_,synonym("equal"),ws_,"to";"==";"is",ws_,"the",ws_,"same",ws_,"as";"is".
synonym("!=") --> "is",ws_,"not",ws_,synonym("equal"),ws_,"to";"!==".
synonym("*") --> "times";"multiplied",ws_,"by".
synonym("/") --> "divided",ws_,"by".
synonym("^") --> "to",ws_,"the",ws_,"power",ws_,"of";"**".
 
synonym("each other") --> "each",ws_,"other";"one",ws_,"another".
 
synonym("color") --> "hue".
 
synonym("an") --> "a".
 
synonym("and") --> "&";"&&".
synonym("or") --> "|";"||".
 
synonym("below") --> "under";"underneath";"beneath".
synonym("above") --> "over";"atop";"on".
 
synonym("inside") --> "within".
synonym("dog") --> "hound".
 
synonym("if") --> "If".
 
synonym(A) --> A.

adjective_synonym("large") --> "large";"big";"huge";"enormous".
adjective_synonym("small") --> "small";"tiny";"minute".
adjective_synonym("tall") --> "tall".

adjective_synonym("male") --> "male".
adjective_synonym("female") --> "female".

adjective_synonym("sad") --> "sad";"unhappy".
adjective_synonym("angry") --> "angry";"enraged".

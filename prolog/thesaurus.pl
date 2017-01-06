verb_synonym(X,Y) :-
    member(List,[
		["mix","combin"],
		["anger","enrag","madden"],
		["eat","ingest"],
		["urinat", "pee","piss"],
		["hat","deplor","dislik","loath"],
		["mix","combin"],
		["cut","sever"],
		["jump","leap"],
		["spray","spew","gush"]
    ]),
    nth0(0,List,Y),memberchk(X,List).

verb_synonym(X,X).

%each stem is an infinitive
same_conjugation(X,Y) :-
    member(List,[
         
        %add "ing" to present tense and "e" to infinitive
        ["fak","pursu","chas","mat","mak","smash","knock","tear","enrag","combin","hat","deplor","dislik","play","bak","cak","stat","creat","hat","lik","dislik","injur","giv","lov","tak","receiv","trad","urinat"],
         
        
        ["look","print","throw","want","jump","leap","boil","spoil","toil","gush","spray","spew","flow","lean","learn","drink","destroy","wreck","insert","ruin","madden","anger","sever","walk","build","burn","talk","speak","hurt","need","burn","knead","kill","lick","suck","melt","pelt","belt","stand","ingest"],
        
        
        %add "es" to the stem in past tense
        ["mix","miss","kiss","hiss","piss"],
         
        %add "ting" to the stem for present tense
        ["sit","cut"],
         
        %add "ning" to the stem for present tense
        ["run"],
         
        %add "ped" to past tense
        ["sip","tap","drop","shop","stop","hop","mop","slap","rap"],
        
        %trying, emptying, etc.
        ["tr","empt"],
        
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
        ["sheep","deer","headquarters","aircraft","crossroads","spacecraft","urine","pee","semen"],
        ["person"],
        ["wolf"],
        ["half"],
        ["dog","number","day","monday","eve","today","yesterday","tomorrow","monday","tuesday","wednesday","thursday","friday","saturday","sunday","year","month","time","hour","coyote","ball","boy","girl","book","elk","lion","tiger","leopard","jaguar","raccoon","essay","letter","steak","word","poem","stag","hyena","bear","rabbit","frog","hag","blanket","bed","bead","head","bread","pillow","chair","squirrel","snake","rat","pear","pearl","bottle","water","river","apple","hound","day","cat","horse","donkey","food","salad","hamburger","cake","carrot","truck","tree","kangaroo","wall","house","building","car","bar","beam","stream","hill","rock","stone","boulder","ship","noun","verb","pronoun","bladder","toilet"],
        
        %add "es" to plural
        ["fish","fox","dingo","mango","walrus","sandwich","penis","iris","piss"],
        ["story"]
    ]),
    memberchk(X,List),memberchk(Y,List).
 
same_adjective_declension(X,Y) :-
    member(List,[
		
		%add "ger" to comparative
        ["big"],
        
        %add "der" to comparative
        ["bad","mad","sad","glad"],
        
        %add "ter" to comparative
        ["fat"],
        
        %add "r" to comparative and "ly" to adverb
        ["large","true","false","huge","same","some"],
        
        %add "y" to adverb
        ["small","tall","full"],
        
        %add "ly" to adverb and "er" to comparative
        ["great","slow","round","hard","soft","neat","long","short"]
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
 
synonym("if") --> "If".

synonym("today") --> "today".
synonym("evening") --> "eve".

synonym(A) --> A.

adjective_synonym("large") --> "big";"huge";"enormous".
adjective_synonym("small") --> "tiny";"minute".
adjective_synonym("tall") --> "tall".

adjective_synonym("good") --> "good".
adjective_synonym("bad") --> "evil".


adjective_synonym("male") --> "male".
adjective_synonym("female") --> "female".

adjective_synonym("sad") --> "unhappy".
adjective_synonym("angry") --> "enraged".

adjective_synonym("new") --> "new".
adjective_synonym(A,A).

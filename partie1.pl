%Vérification sémantique :                                              │

% Prédicat "Alphabet"
concept(C) :- cnamea(C). % Vérification des concepts atomique
concept(CG) :- cnamena(CG). % Vérification des concepts non atomique
instance(I) :- iname(I). % Vérification des identificateurs d instance
role(R) :- rname(R). % Vérification des identificateurs de rôle.

% On vérifie la grammaire de la logique ALC (sujet I.3)
concept(not(C)) :- concept(C).
concept(and(C1, C2)) :- concept(C1), concept(C2).
concept(or(C1, C2)) :- concept(C1), concept(C2).
concept(some(R, C)) :- role(R), concept(C).
concept(all(R, C)) :- role(R), concept(C).

% Vérification syntaxique 

% Pour la Tbox
definition(CA, CG) :- cnamena(CA), concept(CG).
verif_Tbox([(CA, CG) | Q]) :- 
    definition(CA, CG), 
    verif_Tbox(Q).
verif_Tbox([]).

% Pour la Abox
instanciationC(I, CG) :- instance(I), concept(CG).
instanciationR(I1, I2, R) :- instance(I1), instance(I2), role(R).

verif_AboxC([]).
verif_AboxC([(I, CG) | Q]) :- instanciationC(I, CG), verif_AboxC(Q).

verif_AboxR([]).
verif_AboxR([(I1, I2, R) | Q]) :- instanciationR(I1, I2, R), verif_AboxR(Q).

verif_Abox(AboxC,AboxR) :- verif_AboxC(AboxC), verif_AboxR(AboxR).

Auto-référencement  :   

% verif_Autoref(Lcc,Lca) vrai ssi les concept non atomique de Lcc ne sont pas auto-référencé
verif_Autoref([]).
verif_Autoref([C|L]) :-
	equiv(C, Def_C),
	pautoref(C, Def_C),
	verif_Autoref(L).

% pautoref(C, Def, Lca) vrai ssi le concept non atomique C n est pas dans la def récursive Def

pautoref(_, Def) :-
	cnamea(Def).

pautoref(C, Def) :-
	C \== Def,
	cnamena(Def),
	equiv(Def, Def_developpe),
	pautoref(C, Def_developpe).

pautoref(C, and(D1,D2)) :-
	pautoref(C, D1),
	pautoref(C, D2).
	
pautoref(C, or(D1,D2)) :-
	pautoref(C, D1),
	pautoref(C, D2).

pautoref(C, all(_,D)) :-
	pautoref(C,D).

pautoref(C, some(_,D)) :-
	pautoref(C,D).
	
pautoref(C, not(D)) :-
	pautoref(C,D).
	
% Traitement des Boxs   

% developpe(C, D) vrai ssi D est le développement atomique de C
developpe(C, C) :- cnamea(C).

developpe(C, D) :- 
	equiv(C, E),
	developpe(E, D).

developpe(not(C), not(D)) :- developpe(C, D).

developpe(or(C1,C2), or(D1,D2)) :- 
	developpe(C1, D1), 
	developpe(C2, D2).

developpe(and(C1,C2),and(D1,D2)) :- 
	developpe(C1,D1),
	developpe(C2,D2).

developpe(some(R,C), some(R,D)) :- developpe(C, D).

developpe(all(R,C), all(R,D)) :- developpe(C, D).

% équivalance entre traitement_Tbox et traitement_Abox
transforme([], []).
transforme([(X,C) | L], [(X,D) | M]) :- 
	developpe(C, E),
	nnf(E, D),
	transforme(L, M).
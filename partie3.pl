/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Tri box : génère des listes contenant chaque type d'assertion: ∃,⊓,∀,⊔  │                                                                  │
  └──────────────────────────────────────────────────────────────────────────┘
 */
tri_Abox([], [], [], [], [], []).
tri_Abox([(I, some(R,C)) | L], [(I, some(R,C)) | Lie], Lpt, Li, Lu, Ls) :-
    tri_Abox(L, Lie, Lpt, Li, Lu, Ls).
tri_Abox([(I, all(R,C)) | L], Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls) :-
    tri_Abox(L, Lie, Lpt, Li, Lu, Ls).
tri_Abox([(I, and(C1,C2)) | L], Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Ls) :-
    tri_Abox(L, Lie, Lpt, Li, Lu, Ls).
tri_Abox([(I, or(C1,C2)) | L], Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls) :-
    tri_Abox(L, Lie, Lpt, Li, Lu, Ls).
tri_Abox([(I,C)|L], Lie, Lpt, Li, Lu, [(I,C)|Ls]) :-
    cnamea(C),
    tri_Abox(L, Lie, Lpt, Li, Lu, Ls).
tri_Abox([(I,not(C))|L], Lie, Lpt, Li, Lu, [(I,not(C))|Ls]) :-
    cnamea(C),
    tri_Abox(L, Lie, Lpt, Li, Lu, Ls).
/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Evolue : intègre le premier paramètre dans la liste lui correspondant    │
  └──────────────────────────────────────────────────────────────────────────┘
 */
evolue((I, some(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, some(R,C)), Lie).
evolue((I, some(R,C)), Lie, Lpt, Li, Lu, Ls, [(I, some(R,C)) | Lie], Lpt, Li, Lu, Ls):-
    \+ member((I, some(R,C)), Lie).

evolue((I, all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, all(R,C)), Lpt).
evolue((I, all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I, all(R,C)) | Lpt], Li, Lu, Ls):-
    \+ member((I, all(R,C)), Lpt).

evolue((I, and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, and(C1,C2)), Li).
evolue((I, and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Ls) :-
    \+ member((I, and(C1,C2)), Li).

evolue((I, or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    member((I, or(C1, C2)), Lu).
evolue((I, or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls) :-
    \+ member((I, or(C1, C2)), Lu).

evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I,C), Ls).
evolue((I,C), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I,C)|Ls]) :-
    cnamea(C),
    \+ member((I,C), Ls).
    
evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls) :-
    cnamea(C),
    member((I,not(C)), Ls).
evolue((I,not(C)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(I,not(C))|Ls]) :-
    cnamea(C),
    \+ member((I,not(C)), Ls).

evolue_rec([], Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, Ls).
evolue_rec([A|L], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    evolue(A, Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    evolue_rec(L, Lie2, Lpt2, Li2, Lu2, Ls2, Lie1, Lpt1, Li1, Lu1, Ls1).

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Test de clash : Y'a-t-il un clash dans le noeud Ls ?                     │
  └──────────────────────────────────────────────────────────────────────────┘
 */
non_clash([]).
non_clash([(I,C) | Ls]) :-
    nnf(not(C), NC),
    \+ member((I, NC), Ls),
    non_clash(Ls).
	
/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Resolution : Renvoie vrai si on trouve une feuille ouverte                │
  └──────────────────────────────────────────────────────────────────────────┘
 */
resolution(Lie, Lpt, Li, Lu, Ls, Abr) :-
    non_clash(Ls),
    complete_some(Lie, Lpt, Li, Lu, Ls, Abr).
	
resolution([], Lpt, Li, Lu, Ls, Abr) :-
    non_clash(Ls),
    transformation_and([], Lpt, Li, Lu, Ls, Abr).
	
resolution([], Lpt, [], Lu, Ls, Abr) :-
    non_clash(Ls),
    deduction_all([], Lpt, [], Lu, Ls, Abr).
	
resolution([], [], [], Lu, Ls, Abr):-
	non_clash(Ls),
	transformation_or([], [], [], Lu, Ls, Abr).
	
resolution([], [], [], [], Ls, _):-
	non_clash(Ls).

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Affichage                                                                │
  └──────────────────────────────────────────────────────────────────────────┘
 */
affiche_Ls([]).
affiche_concept(some(R, C)) :-
	write(' \u2203 '),
	write(R),
	write('.'),
	affiche_concept(C).
affiche_concept(all(R, C)) :-
	write(' \u2200 '),
	write(R),
	write('.'),
	affiche_concept(C).
affiche_concept(and(C1, C2)) :-
	affiche_concept(C1),
	write(' \u2A05 '),
	affiche_concept(C2).
affiche_concept(or(C1, C2)) :-
	affiche_concept(C1),
	write(' \u2A06 '),
	affiche_concept(C2).
affiche_concept(not(C)) :-
	write(' \u00AC '),
	affiche_concept(C).
affiche_concept(C) :-
	cnamea(C),
	write(C).
	
affiche_Abi([]).
affiche_Abi([(I, C) | L]):-
	write(I), write(' : '), affiche_concept(C),nl,
	affiche_Abi(L).

affiche_Abr([]).
affiche_Abr([(I1, I2, R) | L]) :-
	write('<'), write(I1), write(', '), write(I2), write('> : '),
	write(R),nl,
	affiche_Abr(L).

affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2):-
	write("\033[4;30mEtat de départ :\033[0m"),nl,nl,
	affiche_Abi(Ls1),
	affiche_Abi(Lie1),
	affiche_Abi(Lpt1),
	affiche_Abi(Li1),
	affiche_Abi(Lu1),
	affiche_Abr(Abr1),
	nl,
	write("\033[4;30mEtat d'arrivée :\033[0m"),nl,nl,
	affiche_Abi(Ls2),
	affiche_Abi(Lie2),
	affiche_Abi(Lpt2),
	affiche_Abi(Li2),
	affiche_Abi(Lu2),
	affiche_Abr(Abr2),
	nl,
	(non_clash(Ls2)->write('Pas de clash dans ce noeud');
		write('Clash dans ce noeud')),nl,
	nl, write('============================='), nl, nl.

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Union : ⊔                                                                │
  └──────────────────────────────────────────────────────────────────────────┘
 */
transformation_or(Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls, Abr) :- 
	% Suppression & extraction de la règle devenus inutile par la décomposition en paramètre
	write('Utilisation de la règle \u2A06 sur : '),affiche_Abi([(I, or(C1,C2))]),nl,
	write('Br1 : avec '),affiche_concept(C1),nl,
	% _________________________________
	% Premier split Br1 & Nouveau noeud
	evolue((I, C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
	
	% Print du split
	affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),

	% Appel récursif
	resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).
	
transformation_or(Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Ls, Abr) :- 
	write('Utilisation de la règle \u2A06 sur : '),affiche_Abi([(I, or(C1,C2))]),nl,
	write('Br2 : avec '),affiche_concept(C2),nl,
	% __________________________________
	% Deuxième split Br2 & Nouveau noeud
	evolue((I, C2),Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
	
	% Print du split
	affiche_evolution_Abox(Ls, Lie, Lpt, Li, [(I, or(C1,C2)) | Lu], Abr, Ls2, Lie2, Lpt2, Li2, Lu2, Abr),
	
	% Appel récursif
	resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr).

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Intersection : ⊓                                                         │
  └──────────────────────────────────────────────────────────────────────────┘
 */
transformation_and(Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Ls, Abr) :- 
	write('Utilisation de la règle \u2A05 sur : '), affiche_Abi([(I, and(C1,C2))]),nl,
	% Suppression & extraction de la règle devenus inutile par la décomposition en paramètre

	% Nouveau noeud
	evolue_rec([(I,C1),(I,C2)], Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),

	% Print du split
	affiche_evolution_Abox(Ls, Lie, Lpt, [(I, and(C1,C2)) | Li], Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr),

	% Appel récursif
	resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ % Il existe : ∃                                                          │
  └──────────────────────────────────────────────────────────────────────────┘
 */
complete_some([(I1,some(R,C)) | Lie], Lpt, Li, Lu, Ls, Abr) :-
	write('Utilisation de la règle \u2203 sur : '), affiche_Abi([(I1, some(R,C))]),nl,
	% Nouveau noeud
	genere(I2),
	evolue((I2, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1), 
	
	% Print du noeud
	affiche_evolution_Abox(Ls, [(I1,some(R,C)) | Lie], Lpt, Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, [(I1, I2, R) | Abr]),
	
	% Appel récursif
	resolution(Lie1, Lpt1, Li1, Lu1, Ls1, [(I1, I2, R) | Abr]).

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ % Pour tout :  ∀                                                         │
  └──────────────────────────────────────────────────────────────────────────┘
 */
deduction_all(Lie, [(I1, all(R, C)) | Lpt], Li, Lu, Ls, Abr) :-
	(setof((I2, C),  member((I1, I2, R), Abr), LC2) -> 
		write('Utilisation de la règle \u2200 sur : '), affiche_Abi([(I1, all(R,C))]),
		%writef('et < %w : %w > : %w', [I1, I2, R]), 
		nl ;
		write('Tentative d\'utilisation de la règle \u2200 sur : '), affiche_Abi([(I1, all(R,C))]), writef('car il n\'y a pas de b tel que < %w : b > : %w', [I1, R]),nl
	),

	% Nouveau noeud
	evolue_rec(LC2, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
	
	% Print du noeud
	affiche_evolution_Abox(Ls, Lie, [(I1, all(R, C)) | Lpt], Li, Lu, Abr, Ls1, Lie1, Lpt1, Li1, Lu1, Abr), 
	
	% Appel récursif
	resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr).
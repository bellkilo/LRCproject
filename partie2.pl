saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :- nl,
    write('Entrez le numéro du type de proposition que vous voulez démontrer :'), nl,
    write('1 Une instance donnée appartient a un concept donné.'), nl,
    write('2 Deux concepts qui n\'ont pas d\'éléments en commun (ils ont une intersection vide).'), nl,
    read(R),
    suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox), !.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox), !.
suite(_,Abi,Abi1,Tbox) :- nl, 
    write('Cette réponse est incorrecte.'), nl,
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Acquisition des propositions de type 1 : I : C                           │
  └──────────────────────────────────────────────────────────────────────────┘
 */

input_prop_type1(I, CG) :-
    write('Ajoutons une instance de concept à la ABox :'), nl,
    write('Elle a la forme "I : C"'), nl,
    write('Entrez I :'),nl, 
    read(I), writef('I : %w', [I]), nl, nl,
    write('Entrez C :'),nl, 
    read(CG), writef('C : %w', [C]), nl, nl,
    (instanciationC(I, CG) -> % if 
        writef('%w : %w', [I, CG])
        ; ( % else
        write('\033[1;31m[ERREUR] : I n\'est pas une instance déclarée ou C n\'est pas un concept\033[0m'), nl,
        write('Veuillez recommencer'), nl, fail
    )).

acquisition_prop_type1(Abi, Abi1, Tbox) :- 
    input_prop_type1(I, CG), % User input
    transforme([(I,not(CG))], [(I, CG_dev_nnf)]), % Développement + nnf
    concat(Abi,[(I, CG_dev_nnf)], Abi1), % Ajout de l'input de l'utilisateur dans la ABox
    %write("Abi1"), write(Abi1),
    nl. 

/* 
  ┌──────────────────────────────────────────────────────────────────────────┐
  │ Acquisition des propositions de type 2 : C1 ⊓ C2 ⊑ ⊥                     │
  └──────────────────────────────────────────────────────────────────────────┘
 */

input_prop_type2(C1, C2) :-
    write('Ajoutons une proposition de la forme C1 ⊓ C2 ⊑ ⊥.'), nl,
    write('Entrez C1 :'), nl, 
    read(C1), nl, writef('C1 : %w', [C1]), nl, nl,
    write('Entrez C2 :'), nl, 
    read(C2),nl, writef('C2 : %w', [C2]), nl, nl,
    (concept(and(C1, C2)) -> % if 
        writef("%w ⊓ %w ⊑ ⊥", [C1, C2])
        ; ( % else
        write('\033[1;31m[ERREUR] : C1 ou C2 n\'est pas un concept déclaré\033[0m'), nl,
        write('Veuillez recommencer'), nl, fail
    )).

acquisition_prop_type2(Abi, Abi1, Tbox) :- 
    input_prop_type2(C1, C2), % User input
    % Développement + nnf
    genere(Random_CName),
    transforme([(Random_CName, and(C1, C2))], [(Random_CName, and(C1_dev_nnf, C2_dev_nnf))]),
    concat(Abi, [(Random_CName, and(C1_dev_nnf, C2_dev_nnf))], Abi1), % Ajout de l'input de l'utilisateur dans la ABox
    %write("Abi1"), write(Abi1),
    nl.
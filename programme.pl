premiere_etape(Tbox, Abi, Abr) :-
    setof((CA, CG), equiv(CA, CG), Tboxt),       % Récupération de la TBoxt
    setof((I1, I2), inst(I1, I2), Abit),         % Récupération de Abit
    setof((I1, I2, R), instR(I1, I2, R), Abr),   % Récupération de Abrt
    
    % Vérification de la Tbox
    write('[LOG] Vérification de la TBox ...'), nl,
    (verif_Tbox(Tboxt) ->
        write('[LOG] Vérification de la TBox réussi'), nl;
        write('[ERREUR] Il y a erreur de syntaxe dans la TBox'), nl, halt),
    
    % Vérification de la Abox
    write('[LOG] Vérification de la ABox ...'), nl,
    (verif_Abox(Abit,Abr) ->
        write('[LOG] Vérification de la ABox réussi'), nl;
        write('[ERREUR] Il y a erreur de syntaxe dans la ABox'), nl, halt),
    
    % Vérification des auto-référencements
    setof(X, cnamena(X), Lcc),    % Récupération de la liste des concepts non atomiques
    (verif_Autoref(Lcc) ->
        write('[LOG] Il n\'y a pas auto-référencement dans la TBox'), nl ;
        write('[ERREUR] Il y a auto-référencement dans la TBox'), nl, halt),

    write('[LOG] Transformation des boxs en développant les concepts complexes puis mise sous forme normale négative...'), nl,
    transforme(Abit, Abi),
    %write('abi:'), write(Abi), nl,
    %write('abit:'), write(Abit),nl,
    transforme(Tboxt, Tbox),
    %write('Tbox:'), write(Tbox), nl,
    %write('Tboxt:'), write(Tboxt),nl,
    write('[LOG] Transformation terminée'),nl.

deuxieme_etape(Abi,Abi1,Tbox) :-
    saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

troisieme_etape(Abi,Abr) :-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
    write('====================='),nl,nl,
    resolution(Lie,Lpt,Li,Lu,Ls,Abr).

programme :-
    load_files('helper.pl'),
    load_files('part1.pl'),
    load_files('part2.pl'),
    load_files('part3.pl'),
    load_files('T-A_Box.pl'),
    
    premiere_etape(Tbox, Abi, Abr),             % Call de la première partie
    deuxieme_etape(Abi,Abi1,Tbox),
    (troisieme_etape(Abi1,Abr)->
    	write('Il y a une feuille ouverte : on n\'a pas pu démontré la proposition');
    	write('Toutes les feuilles sont fermées : on a démontré la proposition')),nl,
    write('[LOG] Programme terminé !').
programme.
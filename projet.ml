(*Question 1*)

type alphabet = char list ;;
type etat = int ;; 
type q =etat list ;;
type f = etat list ;; 
type transition = etat * char * etat ;; 
type ftransition = transition list ;;
(** Type représentant un automate fini . *)
type aef = 
{ alphabets : alphabet ;
  etats : q ; 
  q0 : etat ;
  fetats : f;
  delta : ftransition 
}

(**************************Quelques fonctions sur les AEF***************************************)

(**Module Correct : contient les fonctions relatives pour vérifier si un automate est correcte*)
module Correct = struct 
(*Question 2*)
        (** La fonction [est_vide]   verifie si une liste d'états est vide ou non.
            @param liste_etats La liste des etats 
            @return retourne un boolean *)
        let est_vide liste_etats =
          match liste_etats with 
          |[] -> false 
          |h::_ -> true ;;
        (** La fonction [est_present]  verifie si un état est présent dans une liste d'états
            @param etat Un etat dans la list F 
            @param liste_etats Un ensemble fini d’états de l’automate Q
            @return  Un boolean   *)
        let rec est_present etat liste_etats = 
          match liste_etats with
          | [] -> false
          | h :: t when h = etat -> true
          | _ :: t -> est_present etat t ;;

        (** La fonction [est_inclus] verifie si tous les éléments de la liste F sont dans la liste Q 
            @param fetats  un sous-ensemble de Q contenant les états acceptants de l’automate 
            @param liste_etats Un ensemble fini d’états de l’automate Q
            @return Un boolean  *)
        let rec est_inclus fetats liste_etats =
          match fetats with
          | [] -> true
          | h :: t when est_present h liste_etats -> est_inclus t liste_etats
          | _ -> false ;;

        (** La fonction [est_etat_utilise] verifie si un état est utilisé comme état source ou cible dans la fonction de transition δ
            @param etat Un etat dans la liste Q
            @param delta  Une liste de transitions 
            @return Un boolean *)
        let rec est_etat_utilise etat delta =
          match delta with
          | [] -> false
          | (src, _, cbl) :: t when src = etat || cbl = etat -> true 
          | _ :: t -> est_etat_utilise etat t;;

        (** La fonction  [tous_etats_utilises] verifie si tous les états de Q sont utilisés comme états source ou cible
            @param liste_etats Un ensemble fini d’états de l’automate Q 
            @param delta Une liste de transitions 
            @return Un boolean *)
        let rec tous_etats_utilises liste_etats delta =
          match liste_etats with
          | [] -> true
          | h :: t -> if est_etat_utilise h delta then tous_etats_utilises t delta else false ;;

        (** La fonction [est_correct] verifie si un AEF est correct
            @param aef automate 
            @return Un automate correcte  *)
        let est_correct aef =
          let etats_non_vide = est_vide aef.etats  in
          let fetats_inclus_etats = est_inclus aef.fetats aef.etats in
          let tous_etats_dans_delta = tous_etats_utilises aef.etats aef.delta in
          etats_non_vide && fetats_inclus_etats && tous_etats_dans_delta;;
end
(*Question 3*)
(**Module Complet : contient les fonctions relatives pour vérifier si un automate est complet*)
module Complet = struct 
        (** La fonction [transition_definie]  verifie si une transition est définie pour un état donné et un caractère donné
            @param etat Un etat dans Q
            @param car Un caractére dans Alphabets 
            @param delat la liste des transitions 
            @return Un boolean *)
        let rec transition_definie etat car delta =
          match delta with
          | [] -> false
          | (src, c, _) :: t when src = etat && c = car -> true
          | _ :: t -> transition_definie etat car t  ;;

        (** La fonction [toutes_transition_definies] verifie si toutes les transitions sont définies pour chaque état et chaque caractère de l'alphabet
            @param etats  Un ensemble fini d’états de l’automate Q 
            @param alphabets la liste des caractéres
            @param delta la liste des transitions
            @return Un boolean *)
        let rec toutes_transitions_definies etats alphabets delta =
          match etats with
          | [] -> true
          | h :: t ->
              let rec verif_caracteres alphabets =
                match alphabets with
                | [] -> true
                | c :: r -> if transition_definie h c delta then verif_caracteres r else false in
              if verif_caracteres alphabets then  toutes_transitions_definies t alphabets delta  else false ;;

        (** La   fonction [est_complet]  verifie si un AEF est complet
            @param aef Un automate
            @return Un boolean *)

        let est_complet aef =
          toutes_transitions_definies aef.etats aef.alphabets aef.delta;;
end
(*Question 4 *)
(** Module Completer_aef :  contient les fonctions relatives pour compléter un automate  *)
module Completer_aef = struct 
        (** La fonction [etat_maximal]  trouve l'état maximal dans un ensemble d'états
            @param etats la liste des etats Q
            @return le max des etats Q *)
        let rec etat_maximal etats =
          match etats with
          | [] -> -1
          | h :: t -> max h (etat_maximal t) ;;

        (** La  fonction [ generer_transitions_manquantes]  génére des transitions manquantes pour un état donné vers l'état puits
            @param etat Un etat dans Q 
            @param alphabets liste des caractére
            @param delta liste des transitions 
            @param etat_puits 
            @return une liste de transition delta qui contient tous les transitions    *)
        let rec generer_transitions_manquantes etat alphabets delta etat_puits =
          match alphabets with
          | [] -> []
          | c :: r ->
              if not (Complet.transition_definie etat c delta) then 
                (etat, c, etat_puits) :: generer_transitions_manquantes etat r delta etat_puits
              else
                generer_transitions_manquantes etat r delta etat_puits ;;

        (** La fonction [completer] compléte un AEF
            @param aef un automate
            @return un automate complet  *)
        let completer aef =
          let etat_puits = etat_maximal aef.etats + 1 in
          let delta_complet =
            List.fold_left
              (fun delta_acc etat -> delta_acc @ generer_transitions_manquantes etat aef.alphabets aef.delta etat_puits)
              aef.delta aef.etats
          in
          {
            aef with
            etats = etat_puits :: aef.etats;
            delta = delta_complet;
          }
end

(*question 5*)

(** La fonction [langage_vide] vérifie si l'automate reconnait le langage vide
  @param aef un automate 
  @return un boolean  *)
  let langage_vide aef =
    aef.delta = [] || aef.fetats = [] ;;

(*Question 6*)
(**Module Deterministe :contient les fonctions relatives pour vérifier si  un automate est deterministe *)
module Deterministe = struct 
        (** La fonction [compter_transitions] compte les transitions pour un état donné et un caractère donné
            @param etat un etat dans la liste Q
            @param carac un caractére dans la liste d'alphabet
            @param delta la liste des transitions  *)
        let rec compter_transitions etat carac delta =
          match delta with
          | [] -> 0
          | (src, c, _) :: t when src = etat && c = carac -> 1 + compter_transitions etat carac t
          | _ :: t -> compter_transitions etat carac t

        (** La  fonction [est_deterministe] verifie si un AEF est déterministe
            @param aef un automate
            @return  un boolean *)
        let est_deterministe aef =
          let rec verifier_etats etats =
            match etats with
            | [] -> true
            | h :: t ->
                let rec verifier_caracteres caracteres =
                  match caracteres with
                  | [] -> true
                  | c :: r -> if compter_transitions h c aef.delta <= 1 then verifier_caracteres r else false in
                if verifier_caracteres aef.alphabets then verifier_etats t else false in
          verifier_etats aef.etats ;;
 end
(**************************************** TESTER LES FONCTIONS *******************************************)


(* Cet automate reconnaît les mots qui contiennent la séquence "ab" *)
let aef_correct =
  {
    alphabets = ['a'; 'b'];
    etats = [0; 1; 2];
    q0 = 0;
    fetats = [2];
    delta = [
      (0, 'a', 1);
      (0, 'b', 0);
      (1, 'a', 1);
      (1, 'b', 2);
      (2, 'a', 1);
      (2, 'b', 0)
    ]
  }
(*Cet automate n'est pas correct car il existe un état (l'état 3) dans Q qui n'est ni état source ni état cible, car il n'apparaît dans aucune transition. *)
  let aef_incorrect =  
    {
      alphabets = ['a'; 'b'];
      etats = [0; 1; 2; 3];
      q0 = 0;
      fetats = [1; 3];
      delta = [
        (0, 'a', 1);
        (0, 'b', 0);
        (1, 'a', 1);
        (1, 'b', 2)
      ]
    }
    let aef_complet  =
      {
        alphabets = ['a'; 'b'];
        etats = [0; 1; 2];
        q0 = 0;
        fetats = [2];
        delta = [
          (0, 'a', 1);
          (0, 'b', 0);
          (1, 'a', 1);
          (1, 'b', 2);
          (2, 'a', 1);
          (2, 'b', 0)
        ]
      }
    
      let aef_incomplet =
        {
          alphabets = ['a'; 'b'];
          etats = [0; 1; 2];
          q0 = 0;
          fetats = [2];
          delta = [
            (0, 'a', 1);
            (0, 'b', 0);
            (1, 'a', 1);
            (1, 'b', 2);
            (2, 'a', 1)
            (* il manque une transition pour (2, 'b') *)
          ]
        }
        let aef_vide=
          {
            alphabets = ['a'; 'b'];
            etats = [0];
            q0 = 0;
            fetats = []; (* F est vide *)
            delta = [] (* δ est vide *)
          };;
        let aef_deterministe =
          {
            alphabets = ['a'; 'b'];
            etats = [0; 1; 2];
            q0 = 0;
            fetats = [2];
            delta = [
              (0, 'a', 1);
              (0, 'b', 0);
              (1, 'a', 1);
              (1, 'b', 2);
              (2, 'a', 1);
              (2, 'b', 0)
            ]
          }
          ;; 
          
          let aef_non_deterministe=
            {
              alphabets = ['a'; 'b'];
              etats = [0; 1; 2];
              q0 = 0;
              fetats = [2];
              delta = [
                (0, 'a', 1);
                (0, 'b', 0);
                (1, 'a', 1); (**)
                (1, 'b', 2);
                (1, 'a', 2); (* il y a déja une transition qui commence par etat 1 avec le caractére 1 (**) *)
                (2, 'a', 1);
                (2, 'b', 0)
              ]
            } ;;
          

             
 
Correct.est_correct aef_correct;; (*true*)
Correct.est_correct aef_incorrect;;(*false*)
Complet.est_complet aef_complet ;;(*true*)
Complet.est_complet aef_incomplet ;;(*false*)
Completer_aef.completer aef_incomplet ;; (*il va ajouter une transition de 2 avec le caractére b (2,b,3) *)
langage_vide aef_vide;; (*true*)
Deterministe.est_deterministe aef_deterministe;; (*true*)
Deterministe.est_deterministe aef_non_deterministe;; (*false*)





 (********************************* deuxieme partie **************************************)
(** question 7 *)

(**  Type etat_option *) 
(** Module ReconnaissanceMotAef : contient les fonctions  qui manipule la reconnaissance d'un mot par un AEF déterministe *)
module ReconnaissanceMotAef = struct
        type etat_option = None | Some of etat;;


        (**
        La fonction [lecture_car]  permet de lire un caractère donné à partir d’un état donné
        selon une fonction de transition δ.
          @param etat dans q 
          @param carac Le caractère à lire à partir de l'état actuel
          @param delta La liste des transitions de l'automate
          @return Un etat_option qui contient l'état suivant (Some etat) si une
                  transition correspondante est trouvée, sinon None.
        *)
        let lecture_car etat carac delta =
          let rec aux delta_rest =
            match delta_rest with
            | [] -> None
            | (src, c, dst) :: delta_tail when src = etat && c = carac -> Some dst
            | _ :: delta_tail -> aux delta_tail
          in
          aux delta ;;

          (** question 8 *)

          (** La fonction [lecture_mot] qui permet de lire un mot à partir d’un état donné selon une
        fonction de transition δ.
          @param etat_initial L'état initial dans l'automate 
          @param mot Le mot à lire, représenté par une liste
          @param delta La liste des transitions de l'automate
          @return Un etat_option qui contient l'état final (Some etat) après avoir lu le mot, ou None si aucune transition valide n'est trouvée pour un
          caractère. *)
          let lecture_mot etat_initial mot delta =
            let rec aux etat_courant mot_rest =
              match mot_rest with
              | [] -> Some etat_courant
              | carac :: mot_tail ->
                  match lecture_car etat_courant carac delta with
                  | Some etat_suivant -> aux etat_suivant mot_tail
                  | None -> None
            in
            aux etat_initial mot ;; 

            let resultat_lecture_mot = lecture_mot aef_deterministe.q0 ['a'; 'b'; 'a'] aef_deterministe.delta ;;
            resultat_lecture_mot ;; 
          
        (** question 9 *)

        (** La fonction [accepter_mot] qui prend en arguments un mot et un AEF a et qui permet
        de lire le mot à partir de l’état initial de a. accepter_mot retourne true si le mot est accepté
        par a et false sinon.
        @param mot Une liste de mot 
        @param aef Un automate 
        @param return true si le mot est accepté
        par a et false sinon.  *)

        let accepter_mot mot aef =
          let mot_list  = List.of_seq (String.to_seq mot) in 
          let etat_final_option = lecture_mot aef.q0 mot_list aef.delta in
          match etat_final_option with
          | Some etat_final -> List.mem etat_final aef.fetats
          | None-> false ;;
end
   (**********************************TESTER LES FONCTIONS ****************************************)
(* un automate qui peux lire le mot aba*)
   let aef = {
  alphabets = ['a'; 'b'];
  etats = [0; 1; 2; 3];
  q0 = 0;
  fetats = [3];
  delta = [
    (0, 'a', 1);
    (1, 'b', 2);
    (2, 'a', 3)
  ]
};;



ReconnaissanceMotAef.accepter_mot "aba" aef;; (*true*)
ReconnaissanceMotAef.accepter_mot "abb" aef;; (*false*)




  (*******************************************Operation sur les aef*******************************************)
(*  question 10  *)
(** Module OperationAef : contient les fonctions qui manipule les operations sur les automates comme  l'union , la concaténation ...   *)
module OperationAef = struct 
          exception AutomateNonDeterministe;;

        (** 
          La fonction [union_aef] prend en entrée deux automates finis déterministes [a1] et [a2]
          et retourne un  automate qui reconnaît l'union des langages L(a1) ∪ L(a2).
          @param a1 Le premier automate fini déterministe.
          @param a2 Le second automate fini déterministe.
          @return Un  automate fini déterministe reconnaissant l'union des langages L(a1) et L(a2).*)
          let union_aef a1 a2=
          if not (Deterministe.est_deterministe a1 && Deterministe.est_deterministe a2) then
            raise AutomateNonDeterministe;
          (* Combine alphabets *)
          let new_alphabets =  List.sort_uniq compare (a1.alphabets @ a2.alphabets) in
          (*q0 !∈ Q1 et q0 !∈ Q2*)
          let new_q0 = 1 + max (List.fold_left max min_int a1.etats) (List.fold_left max min_int a2.etats) in
          (*• Q = {q0} ∪ Q1 ∪ Q2*)
          let new_q = new_q0 :: (a1.etats @ a2.etats) in
          (*• Si q1 ∈ F1 ou q2 ∈ F2 alors F = F1 ∪ F2 ∪ {q0} sinon F = F1 ∪ F2*)
          let new_f = if List.mem a1.q0 a1.fetats || List.mem a2.q0 a2.fetats then new_q0 :: (a1.fetats @ a2.fetats)
                      else a1.fetats @ a2.fetats 
          in
          (*• δ1 ∪ δ2 ⊆ δ.*)
          let delta = a1.delta @ a2.delta in

          let delta_from_q0 = List.fold_left (fun acc (src, x, dest) ->
            
            (*si δ1(q1, x) = p *)
              if src = a1.q0 then
            (*ajoute à δ toutes les transitions δ(q0, x) = p pour tout p dans Q1*)
                  (new_q0, x, dest) :: acc
            (*si δ2(q2, x) = p *)  
              else if src = a2.q0 then
            (*ajoute à δ toutes les transitions δ(q0, x) = p pour tout p dans Q2*)
                  (new_q0, x, dest) :: acc
              else
                  acc
          ) [] delta in

            let new_delta = delta @ delta_from_q0 

            in 
              
          { alphabets= new_alphabets; etats = new_q; delta = new_delta; q0 = new_q0; fetats = new_f } ;;
          

(*(*  question 11 *)*)
        (** 
          La fonction [concatener_aefs] prend en entrée deux automates finis déterministes [a1] et [a2]
          et retourne un  automate qui reconnaît la concaténation des langages L(a1) et L(a2).

          @param a1 Le premier automate fini déterministe.
          @param a2 Le second automate fini déterministe.
          @return Un  automate fini déterministe reconnaissant la concaténation des langages L(a1) et L(a2).*)
        let concatener_aefs a1 a2 =
          if not (Deterministe.est_deterministe a1 && Deterministe.est_deterministe a2) then
            raise AutomateNonDeterministe;
          let new_alphabets =  List.sort_uniq compare (a1.alphabets @ a2.alphabets) in 
          (*Q = Q1 ∪ Q2*)
          let q = a1.etats @ a2.etats in
          (*Si q2 ∈ F2 alors F = F1 ∪ F2 sinon F = F2*)
          let f = if List.mem a2.q0 a2.fetats then a1.fetats @ a2.fetats else a2.fetats in
        let new_delta = 
          (*δ1 ∪ δ2 ⊆ δ*)
          a1.delta @ a2.delta @
          (*n ajoute à δ toutes les transitions δ(p, x) = q pour tout p dans F1 et tout
        q dans Q2 tel que δ2(q2, x) = q.*)
          List.concat (List.map (fun f1 ->
              List.filter (fun (s, _, _) -> s = a2.q0) a2.delta
              |> List.map (fun (_, car, t) -> (f1, car, t))
            ) a1.fetats)

          
          in
          { alphabets = new_alphabets; etats = q; delta = new_delta; q0 = a1.q0; fetats = f } ;; 


(*(*  question 12 *)*)

        (** 
          La fonction [afficher] prend en entrée un automate fini déterministe [q] et affiche ses
          transitions à l'écran sous la forme "p --> (x) q2", où p est l'état source, x est le
          caractère de la transition et q2 est l'état cible.

          @param q L'automate fini déterministe dont les transitions doivent être affichées.*)
        let afficher q =
          let rec aux = function
            | [] -> ()
            | (p, x, q2) :: tl ->
              Printf.printf "%d --> (%c) %d\n" p x q2;
              aux tl
          in
          aux q.delta ;; 

end
(***********************************************TESTER LES FONCTIONS **************************************)


(* Exemple d'automates *)
(* l'automate a1 reconnaît le mot abc*)
let a1 = {
    alphabets = ['a'; 'b'; 'c'];
    etats = [0; 1; 2; 3];
    delta = [(0, 'a', 1); (1, 'b', 2); (2, 'c', 3)];
    q0 = 0;
    fetats = [3];
}
(* un automate qui peux lire le mot aba*)
let a2 = {
  alphabets = ['a'; 'b'];
  etats = [0; 1; 2; 3];
  q0 = 0;
  fetats = [3];
  delta = [
    (0, 'a', 1);
    (1, 'b', 2);
    (2, 'a', 3)
  ]
}


let aef_union = OperationAef.union_aef a1 a2 ;;
let aef_concat = OperationAef.concatener_aefs a1 a2 ;;

ReconnaissanceMotAef.accepter_mot "abc" aef_union ;; (*true*)
ReconnaissanceMotAef.accepter_mot "ab" aef_union ;; (*false*)
ReconnaissanceMotAef.accepter_mot "aba" aef_union ;; (*true*)
OperationAef.afficher  aef_union ;;
ReconnaissanceMotAef.accepter_mot "abcaba" aef_concat ;; (*true*)
ReconnaissanceMotAef.accepter_mot "abca" aef_concat ;; (*false*)

(*let union_exception = OperationAef.union_aef a1 aef_non_deterministe ;; (*il leve une exception AutomateNonDeterministe *)*)

let afe_brahim = {
  alphabets = ['b'; 'r'; 'a'; 'h'; 'i'; 'm'];
  etats = [0; 1; 2; 3; 4; 5; 6];
  q0 = 0;
  fetats = [6];
  delta = [
    (0, 'b', 1);
    (1, 'r', 2);
    (2, 'a', 3);
    (3, 'h', 4);
    (4, 'i', 5);
    (5, 'm', 6)
  ];
};;

let afe_ahmed = {
  alphabets = ['a'; 'h'; 'm'; 'e'; 'd'];
  etats = [0; 1; 2; 3; 4; 5];
  q0 = 0;
  fetats = [5];
  delta = [
    (0, 'a', 1);
    (1, 'h', 2);
    (2, 'm', 3);
    (3, 'e', 4);
    (4, 'd', 5)
  ];
};;

ReconnaissanceMotAef.accepter_mot "brahim" afe_brahim  ;;(*true*)
ReconnaissanceMotAef.accepter_mot "ahmed" afe_ahmed  ;; (*true*)
let union_brahim_ahmed = OperationAef.union_aef afe_brahim afe_ahmed ;;
ReconnaissanceMotAef.accepter_mot "brahim" union_brahim_ahmed  ;;(*true*)
ReconnaissanceMotAef.accepter_mot "ahmed" union_brahim_ahmed ;;(*true*)
let concat_brahim_ahmed = OperationAef.concatener_aefs afe_brahim afe_ahmed ;; 
ReconnaissanceMotAef.accepter_mot "brahimahmed" concat_brahim_ahmed  ;;(*true*)
























  










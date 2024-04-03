(*Type état défini par son numéro, s'il est initial et s'il est final*)
type etat = Etat of int * bool * bool;;

(*Type transition défini par un état de départ, un état d'arrivé, le mot lu et le mot écrit*)
type transition = Transition of etat * etat * string * string;;

(*Type transducteur défini par un alphabet d'entrée, un alphabet de sortie, un ensemble d'état et un ensemble de transition*)
type transducteur = Transducteur of string list * string list * etat list * transition list;;

(*Fonctions de creation*)

let init_etat num init fin = Etat(num,init,fin);;

let creer_transition dep arr lect ecrit = Transition(dep,arr,lect,ecrit);; 

(*TODO il faudrait vérifier qu'il existe un chemin entre l'état initial et l'état final 
	-> existence initial
	-> existence final
	-> éviter les boucles*)
let creer_transducteur entree sortie etats transi = Transducteur(entree,sortie,etats,transi);;

(*Exemple*)

let e0 = (init_etat 0 true false);;
let e1 = (init_etat 1 false false);;
let e2 = (init_etat 2 false true);;
let e3 = (init_etat 3 false false);;
let e4 = (init_etat 4 false true);;

let t1 = (creer_transition e0 e1 "deux" "2");;
let t2 = (creer_transition e0 e2 "trois" "3");; 


let transducteur0 = (creer_transducteur ["deux";"trois"] ["2";"3"] [e0;e1;e2;e3;e4] [t1;t2]);;

(*Fonction qui retourne l'ensemble des états initiaux d'un transducteur*)
let etat_initial trans = 
  let rec aux transd res =
    match transd with
    | Transducteur(_,_,[],_) -> res
    | Transducteur(entree,sortie,Etat(num,init,fin)::etats,transi) -> 
        if init then (aux (Transducteur(entree,sortie,etats,transi)) (Etat(num,init,fin)::res)) 
        else (aux (Transducteur(entree,sortie,etats,transi)) res)
  in aux trans [];;

(*Exemple*)

etat_initial transducteur0;;

(*Fonction qui retourne l'ensemble des états finaux d'un transducteur*) 
let etat_finaux trans = 
  let rec aux transd res =
    match transd with
    | Transducteur(_,_,[],_) -> res
    | Transducteur(entree,sortie,Etat(num,init,fin)::etats,transi) -> 
        if fin 
        then (aux (Transducteur(entree,sortie,etats,transi)) (Etat(num,init,fin)::res)) 
        else (aux (Transducteur(entree,sortie,etats,transi)) res)
  in aux trans [];;

(*Exemple*)

etat_finaux transducteur0;;

(*Fonction qui afficher un état*) 
let print_etat e =
  match e with
  | Etat(num,false,false) -> print_string "Numéro: "; print_int num; print_newline()
  | Etat(num,false,true) -> print_string "Numéro: "; print_int num; print_string " final"; print_newline()
  | Etat(num,true,false) -> print_string "Numéro: "; print_int num; print_string " initial"; print_newline()
  | Etat(num,true,true) -> print_string "Numéro: "; print_int num; print_string " initial et final"; print_newline();;

List.iter print_etat (etat_initial transducteur0);;
List.iter print_etat (etat_finaux transducteur0);;


(*Fonction qui applique la transition à l'état courant*)
let appliquer transi (Etat(num,_,_)) seq res =
  match transi with
  | Transition(Etat(dep,_,_),Etat(arr,init,fin),lect,ecrit) -> if dep == num
      then if compare seq lect == 0
        then (Etat(arr,init,fin),lect,res^ecrit) 
        else failwith "le mot n'est pas acceptable"
      else failwith "l'état courant n'est pas applicable";;


let (q,r,w) = appliquer t1 e2 "" "";;

let (q,r,w) = appliquer t2 e0 "" "";;

let t2 = creer_transition e0 e2 "trois" "3";;

(*TODO faire une fonction d'affichage, ici on va utilser les tuples (q,r,w) avec q l'etat courant, r le mot à lire et w le mot à écrire*)


let print_etat (q, r, w) = match q with 
  | Etat(num, init, fin) ->
      print_string "État courant: ";
      print_int num;
      if init then print_string " (Initial)";
      if fin then print_string " (Final)";
      print_newline();
      print_string "Lecture: ";  (* Afficher les mots à lire et à écrire *) 
      print_endline r;
      print_string "Écriture: ";
      print_endline w;
      print_newline();;






module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
      *)
    type 'a t = 'a -> 'a list

    (** La stratégie vide : ne renvoie aucune proposition de réduction *)
    val empty : 'a t

    (* TYPES DE BASE *)

    (** Stratégie de réduction sur les entiers
      * @param n entier
      * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
      *)
    val int : int t

    (** Stratégie de réduction sur les entiers positifs
      * @param n entier positif
      * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
      *)
    val int_nonneg : int t

    (** Stratégie de réduction sur les flottants
      * @param x flottant
      * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
      *)
    val float : float t

    (** Stratégie de réduction sur les flottants positifs
      * @param x flottant listlistlistlistllistist
      * @return  liste de flottants positifs plus "simples" entre `0` et `x`
      *)
    val float_nonneg : float t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères plus "simples"
      *)
    val char : char t

    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t

    (* CHAINES DE CARACTERES *)

    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t   
    
    (*string t -> string list t*)

    (* LISTES *)

    (** Stratégie de réduction sur les listes
      * @param red stratégie de réduction à utiliser sur chaque élément
      * @param l   liste
      * @return    liste de listes plus "simples" au pire aussi longues que `l`
      *)
    val list : 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Stratégie de réduction sur les couples
      * @param fst_red stratégie de réduction de la première coordonnée
      * @param snd_red stratégie de réduction de la deuxième coordonnée
      * @return        stratégie de réduction sur les couples correspondants
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un filtre à une stratégie de réduction
      * @param p   filtre à appliquer à chaque réduction
      * @param red stratégie de réduction
      * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t
  end =
  struct
  type 'a t = 'a -> 'a list ;;

  let empty = fun x -> [] ;;
 
  (* les bornes ne sont pas comprises donc on ne renvoie pas n *)
  let int n = if n = 0 then [] else [0; (abs n)-1; (-abs n)+1] ;;

  (* List.init crée une liste d'entiers de 0 à n-1 en prenant en paramètres la taille de la liste à créer et une fonction appliquée à chaque élément (identité) *)
  let int_nonneg n = if n = 0 then [] else List.init (n) (fun i -> i) ;;

  (* on renvoie une liste de flottants en vérifiant que x ne soit pas dans la liste *)
  let float x =
    if x = 0.0 then []
    else
      let lst = [0.0; abs_float x; -.(abs_float x)] in
      if List.mem x lst then List.filter (fun y -> y <> x) lst
      else lst;;

  (* exemple : si x est égal à 1.0, la liste renvoyée sera [0.0; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9]*)
  let float_nonneg x = if x = 0.0 then [] else List.init 10 (fun i -> (float_of_int i) *. (x /. 10.0)) ;;

  (* transforme un caractère en la liste des caractères avant lui dans la table ASCII *)
  let char c = 
    let ascii_value = int_of_char c in
    let tab = 
      if ascii_value >= 97 && ascii_value <= 122 then
        [97; ascii_value - 1]
      else if ascii_value >= 65 && ascii_value <= 90 then
        [65; ascii_value - 1]
      else
        []
    in
    List.map char_of_int tab
  ;;

  (* transforme un caractère / chiffre en la liste de caractères / chiffres avant lui dans la table *)
  let alphanum c = 
    let ascii_value = int_of_char c in
    let tab = 
      if ascii_value >= 97 && ascii_value <= 122 then
        [97; ascii_value - 1]
      else if ascii_value >= 65 && ascii_value <= 90 then
        [65; ascii_value - 1]
      else if ascii_value >= 48 && ascii_value <= 57 then
        [48; ascii_value - 1] 
      else
        []
    in
    List.map char_of_int tab
  ;;

  let rec string red s =
    let l = String.length s in
    let rec aux i =
      if i = l then []
      else List.map (fun c -> (String.make 1 c)^(String.sub s (i+1) (l-i-1))) (red s.[i]) @ (aux (i+1))
    in aux 0

  
    let list red l =
      let rec reduce_list = function
        | [] -> [[]]
        | hd :: tl ->
            let reduced_hd = red hd in
            let reduced_tl = reduce_list tl in
            List.concat (List.map (fun s -> List.map (fun l -> s :: l) reduced_tl) reduced_hd)
      in reduce_list l

  (* Produit toutes les paires (a', b') où a' est une réduction de a selon fst_red, et b' est une réduction de b selon snd_red.
     On itère sur a' selon fst_red avec fold_left,
     puis on applique list_map pour obtenir une liste de paires (a', b') où b' est une réduction de b selon snd_red. 
     Enfin, on concatène toutes les listes de paires ainsi obtenues à l'aide de l'opérateur @ *)
  let combine fst_red snd_red =
    fun (a, b) ->
      List.fold_left (fun acc a' -> acc @ List.map (fun b' -> (a', b')) (snd_red b)) [] (fst_red a)

  let filter p red =
      fun x ->
        List.filter p (red x)

  end ;;

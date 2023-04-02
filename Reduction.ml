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
      * @param x flottant positif
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
    let empty _ = [] ;;

    (* TYPES DE BASE *)
    let int n = 
      let rec aux n =
        if n = 0 then []
        else n :: aux (n / 2)
      in
        aux (abs n) ;;
    
    let int_nonneg n =
      let rec aux n =
        if n = 0 then []
        else n :: aux (n / 2)
      in
        aux n ;;

    let float x = 
      let rec aux x =
        if x = 0. then []
        else x :: aux (x /. 2.)
      in
        aux (abs_float x) ;;
    
    let float_nonneg x =
      let rec aux x =
        if x = 0. then []
        else x :: aux (x /. 2.)
      in
        aux x ;;
    
    let char c =
      let rec aux c =
        if c = 'a' then []
        else c :: aux (char_of_int (int_of_char c - 1))
      in
        aux c ;;
    
    let alphanum c =
      let rec aux c =
        if c = '0' then []
        else c :: aux (char_of_int (int_of_char c - 1))
      in
        aux c ;;

    (* LISTES *)

    let list red l =
      let rec aux acc lst = 
        match lst with
        | [] -> List.rev acc
        | hd :: tl -> aux (red hd :: acc) tl
      in
        aux [] l ;;

    (* TRANSFORMATIONS *)

    let combine red1 red2 (x, y) =
      let red1_list = red1 x in
      let red2_list = red2 y in
      let rec product l1 l2 =
        match l1 with
        | [] -> []
        | hd1 :: tl1 -> List.map (fun e -> (hd1, e)) l2 @ product tl1 l2
      in
        product red1_list red2_list ;;    

    let filter p red =
      fun x ->
        let red_list = red x in
        List.filter p red_list ;;
        
    
    (* CHAINES DE CARACTERES *)

    let string red s =
      List.concat (List.map (fun c -> List.map (String.make 1) (red c)) (List.of_seq (String.to_seq s)))
    ;;
    (* TODO : Implémenter tous les éléments de la signature manquants *)
  end ;;
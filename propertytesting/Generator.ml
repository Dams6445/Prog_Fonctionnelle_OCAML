module Generator :
sig
    (** Type du générateur pseudo-aléatoire de données de type 'a *)
  type 'a t

    (** Renvoie une nouvelle valeur aléeatoire
      * @param gen générateur pseudo-aléatoire
      * @return    nouvelle valeur aléatoire en utilisant `gen`
  *)
  val next : 'a t -> 'a

    (** Générateur constant
      * @param x valeur
      * @return  générateur de l'unique valeur `x`
  *)
  val const : 'a -> 'a t

    (* GENERATEURS DE TYPES DE BASE *)
 
    (** Générateur pseudo-aléatoire de booléens
      * @param prob probabilité de la valeur `true`
      * @return     générateur pseudo-aléatoire de valeurs booléennes
  *)
  val bool : float -> bool t

    (** Générateur pseudo-aléatoire d'entiers
      * @param a borne inférieure
      * @param b borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre `a` et `b` inclus
  *)
  val int : int -> int -> int   t

    (** Générateur pseudo-aléatoire d'entiers positifs ou nuls
      * @param n borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre 0 et `n` inclus
  *)
  val int_nonneg : int -> int   t

    (** Générateur pseudo-aléatoire de flottants
      * @param x borne supérieure
      * @param y borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre `x` et `y` inclus
  *)
  val float : float -> float -> float t

    (** Générateur pseudo-aléatoire de flottants positifs ou nuls
      * @param x borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre 0 et `x` inclus
  *)
  val float_nonneg : float -> float t

    (** Générateur pseudo-aléatoire de caractères *)
  val char : char t

    (** Générateur pseudo-aléatoire de caractères alphanumériques *)
  val alphanum : char t

    (* GENERATEURS DE CHAINE DE CARACTERE *)

    (** Générateur de chaînes de caractères
      * @param n   longueur maximale de la chaîne de caractère
      * @param gen générateur pseudo-aléatoire de caractères
      * @return    générateur pseudo-aléatoire de chaînes de caractères dont chaque caractéré est généré avec `gen`
  *)
  val string : int -> char t -> string t

    (* GENERATEURS DE LISTES *)

    (** Générateur de listes
      * @param n   longueur maximale de la liste
      * @param gen générateur pseudo-aléatoire d'éléments
      * @return    générateur pseudo-aléatoire de listes dont chaque élément est généré avec `gen`
  *)
  val list : int -> 'a t -> ('a list) t
  
    (* TRANSFORMATIONS *)

    (** Générateur pseudo-aléatoire de couples
      * @param fst_gen générateur pseudo-aléatoire de la première coordonnée
      * @param snd_gen générateur pseudo-aléatoire de la deuxième coordonnée
      * @return        générateur pseudo-aléatoire du couple
  *)
  val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un post-traitement à un générateur pseudo-aléatoire
      * @param f   post-traitement à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `f` à chaque valeur générée par `gen`
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t 

    (** Applique un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire ne générant des valeurs de `gen` que si elles vérifient `p`
  *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  
    (** Applique un post-traitement dépendant d'un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param f   couple des post-traitements à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `fst f` pour toute valeur vérifiant `p`
      *                                                          et `snd f` pour toute valeur ne le vérifiant pas
  *)
  val partitioned_map : ('a -> bool) -> (('a -> 'b) * ('a -> 'b)) -> 'a t -> 'b t
end =
struct
  type 'a t = unit -> 'a

  let next gen = gen ()

  let const x = fun () -> x

  let bool prob =
    if Random.float 1.0 < prob then fun () -> true
    else fun () -> false

  let int a b = fun () -> Random.int(b - a + 1) + a

  let int_nonneg n = fun () -> Random.int (n + 1)

  let float x y = fun () -> Random.float (y -. x) +. x

  let float_nonneg x = fun () -> Random.float x

  let char = fun () -> char_of_int(Random.int 256)

  let alphanum = 
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    in fun () -> chars.[Random.int(String.length chars)]

  let string n gen =
    fun () ->
      let buf = Buffer.create n in
      for _i = 1 to n do
        Buffer.add_char buf (next gen)
      done;
      Buffer.contents buf

  let list n gen =
    if n < 0 then invalid_arg "La taille de la liste doit être un nombre supérieur ou égal à é"
    else fun () ->
      let rec aux l i =
        if i = 0 then l
        else aux (next gen :: l) (i - 1)
      in
      aux [] n

  let combine gen1 gen2 = fun () -> (gen1 (), gen2 ())
    
  let map f gen = fun () -> f (gen ())
      
  let filter p gen =
    let rec aux () =
      let x = next gen in
      if p x then x else aux ()
    in
    aux
  
      (*let partitioned_map p (f, g) gen;;*)
end
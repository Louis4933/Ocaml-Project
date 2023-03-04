module Property :
  sig
    (** Type d'une propriété portant sur des éléments de type 'a
      * Une propriété est une fonction booléenne. *)
    type 'a t = 'a -> bool

    (* CONSTANTES *)

    (** Propriété toujours vérifiée *)
    val always_true  : 'a t

    (** Propriété jamais   vérifiée *)
    val always_false : 'a t

    (* FONCTIONS *)

    (** Négation d'une propriété *)
    val not_prop : 'a t -> 'a t

    (** Conjonction de deux propriétés *)
    val and_prop : 'a t -> 'a t -> 'a t

    (** Disjonction de deux propriétés *)
    val or_prop : 'a t -> 'a t -> 'a t

    (** Implication de deux propriétés *)
    val implies_prop : 'a t -> 'a t -> 'a t
  end =
  struct
    type 'a t = 'a -> bool

    let always_true : 'a t = fun _ -> true

    let always_false : 'a t = fun _ -> false

    let not_prop (p : 'a t) : 'a t = fun x -> not (p x)

    let and_prop (p1 : 'a t) (p2 : 'a t) : 'a t = fun x -> p1 x && p2 x

    let or_prop (p1 : 'a t) (p2 : 'a t) : 'a t = fun x -> p1 x || p2 x

    let implies_prop (p1 : 'a t) (p2 : 'a t) : 'a t = fun x -> not (p1 x) || p2 x
  end ;;
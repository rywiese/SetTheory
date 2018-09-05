type 'a set = Empty | Set of 'a * 'a set

let size s =
    let rec sizeHelp s c =
        match s with
        | Empty -> c
        | Set(e,s') -> sizeHelp s' (c+1) in
    sizeHelp s 0

let makeSet e = Set(e, Empty)

let rec contains s e =
    match s with
    | Empty -> false
    | Set(e',s') ->
        if (e=e') then
            true
        else
            contains s' e

let rec union s t =
  match s with
  | Empty -> t
  | Set(e,s') ->
    if (contains t e) then
        union s' t
    else
        union s' (Set(e,t))

let rec listToSet l =
    match l with
    | [] -> Empty
    | h::t -> union (makeSet h) (listToSet t)

let rec intersection s t =
    match s with
    | Empty -> Empty
    | Set(e,s') ->
        if (contains t e) then
            union (makeSet e) (intersection s' t)
        else
            intersection s' t

let rec difference s t =
    match s with
    | Empty -> Empty
    | Set(e,s') ->
        if (contains t e) then
            difference s' t
        else
            union (makeSet e) (difference s' t)

let rec cross s t =
    let rec tuplify e s =
        match s with
        | Empty -> Empty
        | Set(e',s') -> union (makeSet (e,e')) (tuplify e s') in
    match s with
    | Empty -> Empty
    | Set(e,s') -> union (tuplify e t) (cross s' t)

let setToString s =
    let rec setToStringHelp s str =
        match s with
        | Empty -> ""
        | Set (e,t) -> setToStringHelp t (concat [str; ", "; e])
    in setToStringHelp s ""

(* need to get a string module *)

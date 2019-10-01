module MultiDict
    exposing
        ( insert
        , remove
        , get
        , MultiDict
        )

import EverySet exposing (EverySet)
import Dict exposing (Dict)

{- A one-to-many dictionary of keys-to-sets.  A (mostly) straight-up copy-and-paste from the-sett/elm-multi-dict -}

{-| The type of multi dicts from comparable keys to sets of values.
-}
type alias MultiDict comparable value =
    Dict comparable (EverySet value)

{-| Adds a key-value pair into the dictionary. It is added to the set of values
already associated with that key.
-}
insert :
    comparable
    -> value
    -> MultiDict comparable value
    -> MultiDict comparable value
insert k v dict =
    let
        set =
            Dict.get k dict
                |> Maybe.withDefault EverySet.empty
                |> EverySet.insert v
    in
        Dict.insert k set dict


{-| Removes a key-value pair from the dictionary. It is removed from the set of values
associated with the key.
-}
remove :
    comparable
    -> value
    -> MultiDict comparable value
    -> MultiDict comparable value
remove k v dict =
    let
        set =
            Dict.get k dict
                |> Maybe.withDefault EverySet.empty
                |> EverySet.remove v
    in
        if EverySet.isEmpty set then
            Dict.remove k dict
        else
            Dict.insert k set dict


{-| Gets the set of values associated with a key, or `Nothing` if there is no
set of values.
-}
get :
    comparable
    -> MultiDict comparable value
    -> Maybe (EverySet value)
get k dict =
    let
        maybeSet =
            Dict.get k dict
    in
        case maybeSet of
            Nothing ->
                Nothing

            Just set ->
                if EverySet.isEmpty set then
                    Nothing
                else
                    Just set

{- end multi-dict -}

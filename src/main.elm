module Main exposing (Ini(..), KVPair(..), Section(..))


type Ini
    = List KVPair
    | List Section


type KVPair
    = Kv String (Maybe String)


type Section
    = Section String (List KVPair)

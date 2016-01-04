module Game.Board where

type alias Coordinate =
    {
        x : Int
    ,   y : Int
    }


type alias BoardCircle =
    {
        hasPiece : Bool
    ,   coord : Coordinate
    }


coordinate : Int -> Int -> Coordinate
coordinate x y =
    {
        x = x
    ,   y = y
    }


validateCoord : Coordinate -> Bool
validateCoord coord =
    if ( coord.x > 2 && coord.x < 6 ) && ( coord.y > 0 && coord.y < 4 ) then
        True

    else if ( coord.x > 0 && coord.x < 8 ) 
    && ( coord.y > 2 && coord.y < 6 ) then 
        True

    else if ( coord.x > 2 && coord.x < 6 )
    && ( coord.y > 5 && coord.y < 8 ) then
        True

    else
        False


getTuple : Int -> Int -> (Int, Int)
getTuple x y =
    (x, y)


getTuplesRow : Int -> List (Int, Int)
getTuplesRow digit =
    List.map (getTuple digit) [1..7]


getTuplesList : List (Int, Int)
getTuplesList =
    List.concat <| List.map getTuplesRow [1..7]


fromTupleToCoordinate : (Int, Int) -> Coordinate
fromTupleToCoordinate tuple =
    {
        x = fst tuple
    ,   y = snd tuple
    }
module Game.BoardCircle where


type alias Coordinate = (Int, Int)


type alias BoardCircle =
    {
        hasPiece : Bool
    }


validateCoord : Coordinate -> Bool
validateCoord coord =
    if (getX coord > 2 && getX coord < 6) 
    && (getY coord > 0 && getY coord < 4) then
        True

    else if (getX coord > 0 && getX coord < 8) 
    && (getY coord > 2 && getY coord < 6) then 
        True

    else if (getX coord > 2 && getX coord < 6)
    && (getY coord > 5 && getY coord < 8) then
        True

    else
        False


coordinate : Int -> Int -> Coordinate
coordinate x y =
    ( x, y )


boardCircle : Bool -> BoardCircle
boardCircle hpiece =
    {
        hasPiece = hpiece
    }


getX : Coordinate -> Int
getX tuple =
    fst tuple


getY : Coordinate -> Int
getY tuple =
    snd tuple


zipCoordWithBoardCircles : List Coordinate 
                         -> List BoardCircle 
                         -> List ( Coordinate, BoardCircle )
zipCoordWithBoardCircles coords circles =
    List.map2 (,) coords circles


updateBoardCirclePiece : Bool -> Maybe BoardCircle -> Maybe BoardCircle
updateBoardCirclePiece withPiece boardCircle =
    case boardCircle of
        Just boardCircle ->
            Just { boardCircle | hasPiece = withPiece }

        Nothing ->
            boardCircle


setBoardCirclePiece : Bool -> Coordinate -> BoardCircle -> BoardCircle
setBoardCirclePiece withPiece coordinate boardCircle =
    { boardCircle | hasPiece = withPiece }


isBoardCircleOnListAndWithPiece : 
    List Coordinate
    -> Coordinate
    -> BoardCircle
    -> Bool
isBoardCircleOnListAndWithPiece coordinates coordinate boardCircle =
    if List.member coordinate coordinates then
        if boardCircle.hasPiece then
            True

        else
            False

    else
        False


isBoardCircleOnListAndWithoutPiece : 
    List Coordinate
    -> Coordinate
    -> BoardCircle
    -> Bool
isBoardCircleOnListAndWithoutPiece coordinates coordinate boardCircle =
    if List.member coordinate coordinates then
        if not boardCircle.hasPiece then
            True

        else
            False

    else
        False

isBoardCircleOnCoordinateWithPiece : Coordinate -> Coordinate -> BoardCircle -> Bool
isBoardCircleOnCoordinateWithPiece coordinate coordinate' boardCircle =
    if coordinate == coordinate' then
        if boardCircle.hasPiece then
            True

        else
            False
    else
        False
module First where

type Move = (Int, Int, Char)
type Moves = [Move]

winner :: String -> Maybe Char
winner [] = Nothing
winner input =
  let
    moves = parse input
    rw = checkRow moves 2
    cl = checkColumn moves 2
    dg = checkDiagonals moves 2
    winner = if rw /= '-'
      then Just rw
      else if cl /= '-'
        then Just cl
          else if dg /= '-'
            then Just dg
            else Nothing
  in
    winner

--checking rows, columns and diagonals for triples of same player symbol

countScore :: Moves -> Char -> Int
countScore moves p = length $ filter (\(x,y,player) -> player == p) moves

checkRow :: Moves -> Int -> Char
checkRow moves (-1) = '-'
checkRow moves l =
  let
    row = filter (\(x,y,player) -> x == l) moves
    countX = countScore row 'x'
    countO = countScore row 'o'
    winner = if countX > 2
      then 'x'
      else if countO > 2
        then 'o'
        else checkRow moves (l-1)
  in
    winner

checkColumn :: Moves -> Int -> Char
checkColumn moves (-1) = '-'
checkColumn moves l =
  let
    column = filter (\(x,y,player) -> y == l) moves
    countX = countScore column 'x'
    countO = countScore column 'o'
    winner = if countX > 2
      then 'x'
      else if countO > 2
        then 'o'
        else checkColumn moves (l-1)
  in
    winner

checkDiagonals :: Moves -> Int -> Char
checkDiagonals moves l =
  let
    diagonal1 = concat $ filter (not . null) $ getDiagonal1 moves l
    diagonal2 = concat $ filter (not . null) $ getDiagonal2 moves l
    a1 = checkDiagonal diagonal1
    a2 = checkDiagonal diagonal2
    winner = if a1 /= '-'
      then a1
      else if a2 /= '-'
        then a2
        else '-'
  in
    winner

checkDiagonal :: Moves -> Char
checkDiagonal diagonal =
  let
    countX = countScore diagonal 'x'
    countO = countScore diagonal 'o'
    winner = if countX > 2
      then 'x'
      else if countO > 2
        then 'o'
        else '-'
  in
    winner

getDiagonal1 :: Moves -> Int -> [Moves]
getDiagonal1 moves (-1) = []
getDiagonal1 moves l =
  let
    diag = filter (\(x,y,player) -> x == l && y == l) moves
    sm = getDiagonal1 moves (l-1)
  in
    diag:sm

getDiagonal2 :: Moves -> Int -> [Moves]
getDiagonal2 moves (-1) = []
getDiagonal2 moves l =
  let
    diag = filter (\(x,y,player) -> x == l && y == abs(l-2)) moves
    sm = getDiagonal2 moves (l-1)
  in
    diag:sm

--Parsing into tuples

parse :: String -> Moves
parse ('[':rest) = (parseList . trim) rest
parse _ = error "Can't parse, not a list"

parseList :: String -> Moves
parseList "]" = []
parseList str =
  let
    (tuple, rest) = parseToTuple str
    list = readSeparator rest
    tupleBefore = parseList list
  in
    tuple:tupleBefore

parseToTuple :: String -> (Move, String)
parseToTuple ('[':rest) =
  let
    (x, restx) = parseDigit rest
    (y, resty) = parseDigit restx
    prefix = readPrefix resty
    separator = readSeparator prefix
    (p, restp) = readPlayer separator
  in
    case restp of
      (']':t) -> ((x, y, p), t)
      _       -> error "Tuple without closing bracket"

parseDigit :: String -> (Int, String)
parseDigit str =
  let
    prefix = readPrefix str
    separator1 = readSeparator prefix
    (player, rest) = readDigit separator1
    separator2 = readSeparator rest
  in
    (player, separator2)

--Parsing symbols from string

trim :: [Char] -> [Char]
trim [] = []
trim (' ':xs) = trim xs
trim (x:xs) =
  let
    rest = trim xs
  in
    x:rest

readPrefix :: String -> String
readPrefix ('"':coord:'"':rest) = rest
readPrefix _ = error "Prefix expected"

readDigit :: String -> (Int, String)
readDigit ('0':rest) = (0, rest)
readDigit ('1':rest) = (1, rest)
readDigit ('2':rest) = (2, rest)
readDigit _ = error "Digit expected"

readSeparator :: String -> String
readSeparator (',':rest) = rest
readSeparator "]" = "]"
readSeparator _ = error "Separator expected"

readPlayer :: String -> (Char, String)
readPlayer ('"': 'x' : '"': rest) = ('x', rest)
readPlayer ('"': 'o' : '"': rest) = ('o', rest)
readPlayer _ = error "Player expected"

type Shot = String
 
data Square = Square {
  position :: String,
  contains :: SquareContents
} deriving Show
 
data SquareContents = Ship | Empty deriving Show
 
data Board = Board {
  xSize :: Int,
  ySize :: Int,
  squares :: [Square]
} deriving Show
 
data Context = Context {
  hits :: [Shot],
  shots :: [Shot],
  lastShotIsHit :: Bool
} deriving Show
 
class Player p where
  chooseNextShot :: p -> Context -> Shot
  getName :: p -> String
  placeNextShip :: p -> Board -> Shot

data Game = Game {
  p1Board :: Board,
  p2Board :: Board,
  p1Context :: Context,
  p2Context :: Context,
  gameEnded :: Bool
}
 
updateContext :: Shot -> Bool -> Context -> Context
updateContext shot isHit (Context oldHits oldShots _) =
  let newHits = if isHit then shot:oldHits else oldHits
  in Context newHits (shot:oldShots) isHit
 
isHitOnSquare :: Shot -> Square -> Bool
isHitOnSquare _ (Square _ Empty) = False
isHitOnSquare shot (Square pos Ship) = shot == pos
 
shotIsHit :: Shot -> Board -> Bool
shotIsHit s b = any (isHitOnSquare s) (squares b)
 
fireShot :: Context -> Board -> Shot -> Context
fireShot c b s = updateContext s (shotIsHit s b) c

noOfShipSquares :: Int
noOfShipSquares = 1

boardSize = 2

-- TODO: implement this so it's not hard coded 
mkEmptyBoard :: Board
mkEmptyBoard = Board boardSize boardSize [(Square "A1" Empty), (Square "A2" Empty), (Square "B1" Empty), (Square "B2" Empty)]
 
mkEmptyContext :: Context
mkEmptyContext = Context [] [] False
 
-- start Example clients
data Player1 = Player1 deriving Show
 
instance Player Player1 where
    chooseNextShot p context = "A1"
    getName p = "Jim"
    placeNextShip p b = "A1"
 
data Player2 = Player2 deriving Show
 
instance Player Player2 where
    chooseNextShot p context = "A1"
    getName p = "Kate"
    placeNextShip p b = "A2"
 
-- end Example clients
 
playRoundForPlayer :: (Player a) => a -> Context -> Board -> Context
playRoundForPlayer player playerContext opponentBoard =
		let shot = chooseNextShot player playerContext
    in fireShot playerContext opponentBoard shot

 
hasGameEnded :: Context -> Context -> Bool
hasGameEnded p1C p2C = 
    let p1Won = length (hits p1C) == noOfShipSquares
        p2Won = length (hits p2C) == noOfShipSquares
    in p1Won || p2Won

-- TODO: Change this so if player 1 shoots first and wins player 2 cant win
playRound :: (Player a, Player b) => a -> b -> Game -> Game
playRound p1 p2 g = 
		let 
      newP1Context = playRoundForPlayer p1 (p1Context g) (p2Board g)
      newP2Context = playRoundForPlayer p2 (p2Context g) (p1Board g)
      gameEnded = hasGameEnded newP1Context newP2Context
		in Game (p1Board g) (p2Board g) newP1Context newP2Context gameEnded

playGame :: (Player a, Player b) => Game -> a -> b -> Game
playGame wonGame@(Game _ _ _ _ True) _ _ = wonGame
playGame g p1 p2 = 
  let newGame = playRound p1 p2 g
  in playGame newGame p1 p2

squareContainsShip :: Square -> Bool
squareContainsShip (Square _ Ship) = True
squareContainsShip _ = False

 
allShipsPlaced :: Board -> Int -> Bool
allShipsPlaced b noToPlace = 
    let placedCount = length $ (filter squareContainsShip $ squares b)
    in placedCount == noToPlace

addShipToBoard :: Board -> Shot -> Board
addShipToBoard b s = Board (xSize b) (ySize b) $
    map (\(Square pos con) -> Square pos (if pos == s then Ship else con)) (squares b) 

placeShips :: (Player p) => p -> Board -> Board
placeShips p b = 
    if (allShipsPlaced b noOfShipSquares) then b
    else placeShips p (addShipToBoard b $ placeNextShip p b)

initGame :: (Player a, Player b) => a -> b -> Game
initGame p1 p2 = 
    let p1B = placeShips p1 mkEmptyBoard
        p2B = placeShips p2 mkEmptyBoard
    in Game p1B p2B mkEmptyContext mkEmptyContext False 

isWinner :: Context -> Bool
isWinner c = noOfShipSquares == (length $ hits c)

getWinner :: (Player a, Player b) => a -> b -> Game -> Either a b
getWinner p1 p2 g = if isWinner $ p1Context g then Left p1 else Right p2

start :: (Player a, Player b) => a -> b -> Either a b
start p1 p2 = 
    let 
      game = initGame p1 p2
      endGame = playGame game p1 p2
    in getWinner p1 p2 endGame

main :: IO ()
main = do
				putStrLn $ show (start Player1 Player2)

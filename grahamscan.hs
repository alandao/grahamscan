import Data.List

data Direction = LeftTurn | RightTurn | Collinear 
    deriving (Eq, Show)

data Point = Point  { xPos :: Float 
                    , yPos :: Float
                    } deriving (Eq, Show)
                    
angleDiff :: Point -> Point ->  Point -> Float 
angleDiff p0 p1 p2 = (((xPos p1) - (xPos p0)) * ((yPos p2) - (yPos p0)) - ((xPos p2) - (xPos p0)) * ((yPos p1) - (yPos p0)))

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2)    =   sqrt( (x1 - x2)^2 + (y1 - y2)^2) 

direction :: Point -> Point -> Point -> Direction
direction p0 p1 p2  
    | x > 0  = LeftTurn
    | x == 0 = Collinear
    | x < 0  = RightTurn
    where x = angleDiff p0 p1 p2

compareTwoAngles :: Point -> Point -> Point -> Ordering
compareTwoAngles p0 p1 p2
    | ccw > 0 = LT 
    | ccw == 0 = compare (distance p0 p1) (distance p0 p2) 
    | ccw < 0 = GT 
    where ccw = angleDiff p0 p1 p2 

sortedPoints :: [Point] -> [Point]
sortedPoints xs = sortBy (\p1 p2 -> compareTwoAngles basepoint p1 p2) xs
    where basepoint = lowestY xs


lowestY :: [Point] -> Point 
lowestY xs = head leftestBottomPoints 
    where
            leftestBottomPoints = filter (\point -> xPos point == minimum (map xPos bottomPoints)) bottomPoints
            bottomPoints = filter (\point -> yPos point == minimum ( map yPos xs)) xs 

gScan :: [Point] -> [Point]
gScan points
    | length points < 3     = error "Needs 3 points to generate a convex hull!"
    | otherwise             = convex 
    where   p = nub points
            sp = sortedPoints p
            rmp = scanRmv sp sp 
            convex = filter (\x -> not (x `elem` rmp)) sp  


scanRmv :: [Point] -> [Point] -> [Point]

scanRmv (p:c:[]) _ = []
scanRmv (p:c:n:xs) copy =  if (direction p c n) == RightTurn || (direction p c n) == Collinear  
                            then c:scanRmv (filter (/= c) copy) (filter (/= c) copy)
                            else scanRmv (c:n:xs) copy
                         
                        
pointsFromTupleList :: [(Float, Float)] -> [Point]
pointsFromTupleList = map (\(x,y) -> Point x y)


examplePoints :: [Point]
examplePoints = pointsFromTupleList [(0,0), (2,2), (1,1.5), (0.5, 2.2), (-0.4, 1.5), (-4, 4), (-5, 3)] 


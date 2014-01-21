import Data.List

--Call the grahamScan function on a list of points you want to find the convex hull of...

data Direction = LeftTurn | RightTurn | Collinear 
    deriving (Show)

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

grahamScan :: [Point] -> [Point]
grahamScan points
    | length points < 3     = error "Needs 3 points to generate a convex hull!"
    | otherwise             = rmp 
    where   p = nub points
            sp = sortedPoints p
            tl = turnsList (sp ++ [head sp])
            rmp = pointsRemoved tl
            

pointsFromTupleList :: [(Float, Float)] -> [Point]
pointsFromTupleList = map (\(x,y) -> Point x y)

--first two points are always part of convex hull, unless it's collinear
pointsRemoved :: [(Point, Point, Point, Direction)] -> [Point] 
pointsRemoved [] = []
pointsRemoved ((p,c,_,LeftTurn):xs)  = (p:c:(pointsRemoved' xs))  
pointsRemoved ((p,_,_,Collinear):xs) = (p:(pointsRemoved' xs))


pointsRemoved' [] = [] 
pointsRemoved' s@((_,c,_,Collinear):xs) = (pointsRemoved' . turnsList ) (filter (/= c) (pointsList s)) 
pointsRemoved' s@((_,c,_,LeftTurn):xs) = pointsRemoved' xs 
pointsRemoved' s@((_,c,_,RightTurn):xs) = (pointsRemoved' . turnsList ) (filter (/= c) (pointsList s))

isLeftTurn :: (Point, Point, Point, Direction) -> Bool
isLeftTurn (_,_,_,LeftTurn) = True
isLeftTurn _ = False


examplePoints :: [Point]
examplePoints = pointsFromTupleList [(0,0),(2,2),(1,1.5),(0.5,2.2),(-0.4,1.5),(-4,4),(-5,3)] 

turnsList :: [Point] -> [(Point, Point, Point, Direction)]
turnsList (a:b:[]) = []
turnsList (a:b:c:xs) = (a,b,c, direction a b c) : turnsList(b:c:xs)

pointsList :: [(Point, Point, Point, Direction)] -> [Point]
pointsList [] = []
pointsList ((a,b,c,_):xs) = a:b:c:(pointsList' xs)

pointsList' [] = []
pointsList' ((a,b,c,_):xs) = c:(pointsList' xs)


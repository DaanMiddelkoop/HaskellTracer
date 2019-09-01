
module ProcessModel where
import Tracing
import Debug.Trace


data Entity = TV Float Float Float | TF Int Int Int Light | TFN Int Int Int Int Int Int Light | TVN Float Float Float | NoLine

instance Show Entity where
  show (TV x y z) = "TV" ++ " " ++ show x ++ " " ++ show y ++ " " ++ show z
  show (TF x y z l) = "TF" ++ " " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show l

collect :: String -> String
collect ('/':_) = []
collect [] = []
collect (x:xs) = x:(collect xs)

strLen :: String -> Int
strLen [] = 0
strLen (x:xs) = 1 + strLen xs

splitSlash :: String -> [String]
splitSlash [] = []
splitSlash ('/':xs) = splitSlash xs
splitSlash (xs) = (result):(splitSlash $ skipn (strLen result) xs)
  where skipn 0 a = a
        skipn n (x:b) = skipn (n-1) b
        result = collect xs

processTF :: [String] -> [String] -> [String] -> Light -> Entity
processTF (x:a:[]) (y:b:[]) (z:c:[]) l = TFN (read x) (read y) (read z) (read a) (read b) (read c) l
processTF (x:[]) (y:[]) (z:[]) l = TF (read x) (read y) (read z) l

processLine :: [String] -> Entity
processLine ("vn":x:y:z:[]) = TVN (read x) (read y) (read z)
processLine ("v":x:y:z:[]) = TV (read x) (read y) (read z)
processLine ("f":x:y:z:[]) = processTF (splitSlash x) (splitSlash y) (splitSlash z) (L (V 1 1 1) 0)
processLine ("f":x:y:z:cx:cy:cz:l:[]) = processTF (splitSlash x) (splitSlash y) (splitSlash z) (L (V (read cx) (read cy) (read cz)) (read l))
processLine (other) = NoLine

processLines :: [[String]] -> [Entity]
processLines [] = []
processLines ([]:lines) = processLines lines
processLines (line:lines) = (processLine line):processLines lines

rotate :: Vertex -> Vertex -> Float -> Vertex
rotate (V x y z) (V j k l) angle = ((len a_ortagonal_b) |*| ((x1 |*| a_ortagonal_b) |+| (x2 |*| w))) |+| a_follow_b
  where a = V x y z
        b = V j k l
        a_follow_b = ((dotproduct a b) / (dotproduct b b)) |*| b
        a_ortagonal_b = a |-| a_follow_b
        w = crossproduct b a_ortagonal_b
        x1 = (cos angle) / (len a_ortagonal_b)
        x2 = (sin angle) / (len w)

vertices :: [Entity] -> [Entity]
vertices [] = []
vertices ((TV x y z):xs) = TV x y z:vertices xs
vertices (x:xs) = vertices xs

faces :: [Entity] -> [Entity]
faces [] = []
faces ((TF x y z l):xs) = TF x y z l:faces xs
faces ((TFN x y z a b c l):xs) = TFN x y z a b c l:faces xs
faces (x:xs) = faces xs

normals :: [Entity] -> [Entity]
normals [] = []
normals ((TVN x y z):xs) = TVN x y z:normals xs
normals (x:xs) = normals xs

-- elem :: [a] -> Int -> a
-- elem (x:xs) 1 = x
-- elem (x:xs) i = elem xs (i-1)

createVertex :: [Entity] -> Int -> Vertex
createVertex vs index = V x y z
  where (TV x y z) = vs!!(index-1)

createNormal :: [Entity] -> Int -> Vertex
createNormal ns index = V x y z
  where (TVN x y z) = ns!!(index-1)


createTriangle :: [Entity] -> [Entity]-> Entity -> Shape
createTriangle vs _ (TF x y z l) = Triangle (createVertex vs x) (createVertex vs y) (createVertex vs z) (V 0 0 0) (V 0 0 0) (V 0 0 0) l
createTriangle vs ns (TFN x y z a b c l) = Triangle (createVertex vs x) (createVertex vs y) (createVertex vs z) (createNormal ns a) (createNormal ns b) (createNormal ns c) l


convertTriangles :: [Entity] -> [Shape]
convertTriangles entities = map (createTriangle (vertices entities) (normals entities)) $ faces entities

surfaceArea :: Vertex -> Vertex -> Float
surfaceArea a b = area abvec
  where abvec = b |-| a
        area (V x y z) = x * y * z

unionAABB :: Vertex -> Vertex -> Vertex -> Vertex -> [Vertex]
unionAABB (V a1x a1y a1z) (V b1x b1y b1z) (V a2x a2y a2z) (V b2x b2y b2z) = (a:b:[])
  where a = V (min a1x a2x) (min a1y a2y) (min a1z a2z)
        b = V (max b1x b2x) (max b1y b2y) (max b1z b2z)

unionAABBNodeSurface :: Vertex -> Vertex -> Shape -> Float
unionAABBNodeSurface a b (Box x y n1 n2) = surfaceArea (unionbox!!0) (unionbox!!1)
  where unionbox = unionAABB a b x y
unionAABBNodeSurface a b (Triangle x y z g h f l) = surfaceArea (unionbox!!0) (unionbox!!1)
  where trianglebox = createTriangleAABB (Triangle x y z g h f l)
        unionbox = unionAABB a b (trianglebox!!0) (trianglebox!!1)

createTriangleAABB :: Shape -> [Vertex]
createTriangleAABB (Triangle (V v1x v1y v1z) (V v2x v2y v2z) (V v3x v3y v3z) _ _ _ _) = ((V lx ly lz):(V hx hy hz):[])
  where lx = min (min v1x v2x) v3x - 0.01
        ly = min (min v1y v2y) v3y - 0.01
        lz = min (min v1z v2z) v3z - 0.01
        hx = max (max v1x v2x) v3x + 0.01
        hy = max (max v1y v2y) v3y + 0.01
        hz = max (max v1z v2z) v3z + 0.01

insertAABBNode :: Shape -> Shape -> Shape
insertAABBNode (Box a b s1 s2) (Box x y n1 n2) | unionAABBNodeSurface a b n1 < unionAABBNodeSurface a b n2 = Box (new_aabb!!0) (new_aabb!!1) (insertAABBNode (Box a b s1 s2) n1) n2
                                               | otherwise = Box (new_aabb!!0) (new_aabb!!1) n1 (insertAABBNode (Box a b s1 s2) n2)
  where new_aabb = unionAABB a b x y
insertAABBNode (Triangle x y z g h f l) tree = insertAABBNode (Box ((createTriangleAABB (Triangle x y z g h f l))!!0) ((createTriangleAABB (Triangle x y z g h f l))!!1) (Triangle x y z g h f l) NoShape) tree
insertAABBNode (Box a b t _) (Triangle x y z g h f l) = Box ((unionaabb)!!0) (unionaabb!!1) t (Triangle x y z g h f l)
  where unionaabb = unionAABB a b (tbox!!0) (tbox!!1)
        tbox = createTriangleAABB (Triangle x y z g h f l)
insertAABBNode (Box a b t _) NoShape = t

boxtree :: [Shape] -> Shape
boxtree (s:[]) = s
boxtree (s:ss) = insertAABBNode s tree
  where tree = boxtree ss
boxtree (s:ss) = foldr insertAABBNode s ss

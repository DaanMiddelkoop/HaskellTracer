import Tracing
import ProcessModel
import Camera
import Control.Parallel.Strategies
import Debug.Trace

data Screen = S [[Vertex]]

pmap f xs = map f xs `using` parList rdeepseq

show_line :: [Vertex] -> String
show_line [] = "endline\n"
show_line (x:xs) = show x ++ show_line xs

instance Show Screen where
  show (S []) = "endimage"
  show (S (x:xs)) = show_line x ++ show (S xs)

-- final_result :: [Shape] -> [Light] -> [Ray] -> Screen
-- final_result shapes lights rays = S (map (\x -> map (\y -> tracing 1 y triangles lights) x) rays)

init_program :: String -> String
init_program contents = show result
  where rays = map (map (R (V 0 0 (2.5)))) (calculate_directions (V 0 0 (-1)) 800 800)
        triangles = (boxtree $ convertTriangles $ processLines $ map words $ lines contents):[]
        lights = [L (V 20 0 20) (V 255 255 255) 500]
        result = S (map (\x -> parMap rdeepseq  (\y -> tracing 1 y (triangles) lights) x) rays)

main :: IO ()
main = do
  contents <- readFile "sphere.obj"
  putStrLn $ init_program contents
  -- print $ map (map (R (V 0 0 0))) (calculate_directions (V 1 0 0) 10 10)
  -- print $ map (rotate (V 1 0 0) (V 0 0 1)) $ generate_angles 10putStrLn
  -- print $ rotate (V 0.5 0 0.5) (V 0 1 0) 0.2

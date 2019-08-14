module Camera where
import Tracing
import Debug.Trace

normal :: Vertex -> Vertex -> Vertex
normal = crossproduct

rotate :: Vertex -> Vertex -> Float -> Vertex
rotate (V x y z) (V j k l) angle = ((len a_ortagonal_b) |*| ((x1 |*| a_ortagonal_b) |+| (x2 |*| w))) |+| a_follow_b
  where a = V x y z
        b = V j k l
        a_follow_b = ((dotproduct a b) / (dotproduct b b)) |*| b
        a_ortagonal_b = a |-| a_follow_b
        w = crossproduct b a_ortagonal_b
        x1 = (cos angle) / (len a_ortagonal_b)
        x2 = (sin angle) / (len w)

generate_angles :: Int -> [Float]
generate_angles width = map (\y -> y + 0.5) (map (\x -> (fromIntegral x) * (-step_size)) [0..width-1])
  where step_size = 1.0 / (fromIntegral width)

calculate_directions :: Vertex -> Int -> Int -> [[Vertex]]
calculate_directions direction width height = map (\x -> map (rotate x side) horizontal_angles) horizontal_directions
  where up = V 0 1 0
        side = normal direction up
        horizontal_angles = generate_angles width
        vertical_angles = generate_angles height
        horizontal_directions = map (rotate direction up) vertical_angles

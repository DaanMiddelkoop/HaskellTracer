module Camera where
import Tracing
import Debug.Trace

normal :: Vertex -> Vertex -> Vertex
normal = crossproduct



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

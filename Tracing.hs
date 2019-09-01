{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Tracing where

import Debug.Trace

import System.IO.Unsafe
import System.Random

import GHC.Generics (Generic)
import Control.DeepSeq



data Vertex = V Float Float Float deriving (Generic, NFData, Eq)
-- Triangle x y z nx ny nz color light-intensity
data Shape = Triangle Vertex Vertex Vertex Vertex Vertex Vertex Light | Box Vertex Vertex Shape Shape | NoShape
data Ray = R Vertex Vertex
-- Hit Distance Location Normal Shape Depth
data RayTraceResult = Mis | Hit Float Vertex Vertex Shape Int

-- L Position Color Intensity
data Light = L Vertex Float





(|+|) :: Vertex -> Vertex -> Vertex
(V x y z) |+| (V a b c) = V (x + a) (y + b) (z + c)

(|-|) :: Vertex -> Vertex -> Vertex
(V x y z) |-| (V a b c) = V (x - a) (y - b) (z - c)

(|*|) :: Float -> Vertex -> Vertex
k |*| (V x y z) = V (x * k) (y * k) (z * k)

instance Show Light where
  show (L color intensity) = "L(" ++ show color ++ " " ++ show intensity ++ ")"

instance Show Vertex where
  show (V x y z) = show x ++ " " ++ show y ++ " " ++ show z ++ "\n"

instance Show Shape where
  show (Triangle x y z a b c l) = "T(" ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show l ++ ")"
  show (Box a b s1 s2) = "B " ++ show a ++ " " ++ show b ++ " (" ++ show s1 ++ " " ++ show s2 ++ ")"

instance Show RayTraceResult where
  show Mis = "Mis"
  show (Hit dist loc normal shape depth)  = "Hit" ++ " " ++ show dist ++ "," ++ show loc ++ "," ++ show normal ++ "," ++ show shape ++ "," ++ show depth

instance Show Ray where
  show (R x y) = "R(" ++ show x ++ " " ++ show y ++ ")"

instance Eq RayTraceResult where
  Mis == Mis = True
  Mis == _ = False
  _ == Mis = False
  _ == _ = True

rotate :: Vertex -> Vertex -> Float -> Vertex
rotate (V x y z) (V j k l) angle = ((len a_ortagonal_b) |*| ((x1 |*| a_ortagonal_b) |+| (x2 |*| w))) |+| a_follow_b
  where a = V x y z
        b = V j k l
        a_follow_b = ((dotproduct a b) / (dotproduct b b)) |*| b
        a_ortagonal_b = a |-| a_follow_b
        w = crossproduct b a_ortagonal_b
        x1 = (cos angle) / (len a_ortagonal_b)
        x2 = (sin angle) / (len w)

fold :: (a -> a -> a) -> [a] -> a
fold f as = foldr f (head as) (tail as)

crossproduct :: Vertex -> Vertex -> Vertex
crossproduct (V x y z) (V a b c) = V ((y * c) - (z * b)) ((z * a) - (x * c)) ((x * b) - (y * a))

dotproduct :: Vertex -> Vertex -> Float
dotproduct (V x y z) (V a b c) = (x * a) + (y * b) + (z * c)

angle :: Vertex -> Vertex -> Float
angle a b = acos ((dotproduct a b) / (len a * len b))

len :: Vertex -> Float
len (V x y z) = sqrt $ (x*x) + (y*y) + (z*z)

vertex0 (Triangle x _ _ _ _ _ _) = x
vertex1 (Triangle _ x _ _ _ _ _) = x
vertex2 (Triangle _ _ x _ _ _ _) = x

rayvector (R _ x) = x
rayorigin (R x _) = x

h rayvector = crossproduct

test_hit :: RayTraceResult -> Bool
test_hit (Hit _ _ _ _ _) = True
test_hit Mis = False

random_source ::IO Float
random_source = randomIO

random_number :: Float
random_number = unsafePerformIO randomIO

randomize_direction :: Int -> [Float] -> Vertex -> Vertex
randomize_direction 0 _ vertex = vertex
randomize_direction depth randoms vertex = rotate (randomize_direction (depth-1) (takefrom randoms 10) vertex) (V (randoms!!1) (randoms!!2) (randoms!!3)) ((randoms!!4) / 1)
  where (_:_:_:_:randoms2) = trace ("aap" ++ show (randoms!!0)) randoms

takefrom :: [a] -> Int -> [a]
takefrom xs 0 = xs
takefrom (x:xs) n = takefrom xs (n-1)

bounce :: Int -> [Float] -> Ray -> [Shape] -> Vertex
bounce 0 _ _ _ = V 0 0 0
bounce depth randoms (R loc dir) shapes = color
  where bounce_dir 0 = []
        bounce_dir x = randomize_direction 10 (takefrom randoms (83*x)) dir:bounce_dir (x-1)
        colors = map (\x -> tracing (depth) (takefrom randoms 1000)(R loc x) shapes) (bounce_dir (depth * 4))
        color = (0.25 / (fromIntegral depth)) |*| (foldr (|+|) (V 0 0 0) (colors))






angle_intensity :: Float -> Float
angle_intensity x | x < 1.5707964 = calc_angle_intensity x
                  | otherwise = calc_angle_intensity (1.5707964 - (x - 1.5707964))
  where calc_angle_intensity x = 1 - (x / 1.5707964)

-- color -> light -> color
color_intensity_mix :: Vertex -> Float -> Vertex
color_intensity_mix (V x y z) intensity = V (x * intensity) (y * intensity) (z * intensity)

-- calc_color :: RayTraceResult -> Float -> Light -> Vertex
-- calc_color (Hit _ x normal _ _) extra_dist (L pos color intensity) = ((1.0 / (d * d)) * (angle_intensity a)) |*| color_intensity_mix color intensity
--   where dist_vec = pos |-| x
--         d = len dist_vec + extra_dist
--         a = angle normal dist_vec

-- trace bounces ray shapes lights -> Color

light_intensity :: Light -> Vertex
light_intensity (L color intensity) = intensity |*| color

tracing :: Int -> [Float]-> Ray -> [Shape] -> Vertex
tracing 0 _ _ _  = V 0 0 0
tracing depth randoms ray shapes | test_hit result = color--trace ("bounce pos+dir " ++ show loc ++ " " ++ show (rotate dir normal 1.57079632679)) color
                          | otherwise = V 0 0 0
  where result = trace_data ray shapes
        (R ray_loc dir) = ray
        (Hit dist loc normal shape _) = result
        (Triangle _ _ _ _ _ _ light) = shape
        color = light_intensity light |+| (1 |*| ( bounce (depth-1) randoms (R loc (rotate ((-1) |*| dir) normal 1.57079632679)) shapes))

        --direct_colors = map (calc_color result eye_dist) direct_lights
       -- |+| ((fromIntegral (calc_depth result * 1)) |*| (V 0 1 0)))
        --calc_depth (Hit _ _ _ _ depth) = depth

trace_data :: Ray -> [Shape] -> RayTraceResult
trace_data ray [] = Mis
trace_data ray (x:shapes) | test_hit result && distance result < (distance $ trace_data ray shapes)  = result
                        | otherwise = trace_data ray shapes
  where result = trace_ray ray x
        distance (Hit x _ _ _ _) = x
        distance Mis = 99999999999999

trace_ray :: Ray -> Shape -> RayTraceResult
trace_ray ray (Triangle x y z a b c l) = trace_triangle ray (Triangle x y z a b c l)
trace_ray ray (Box a b s1 s2) = trace_box ray (Box a b s1 s2)
trace_ray _ NoShape = Mis

trace_box :: Ray -> Shape -> RayTraceResult
trace_box (R (V ox oy oz) (V vx vy vz)) (Box (V ax ay az) (V bx by bz) s1 s2) | tmax > 0 && tmin < tmax = increase_green ray_result
                                                                              | otherwise = Mis
  where t1 = (ax - ox) / vx
        t2 = (bx - ox) / vx
        t3 = (ay - oy) / vy
        t4 = (by - oy) / vy
        t5 = (az - oz) / vz
        t6 = (bz - oz) / vz

        tmin = max (max(min(t1) (t2)) (min(t3) (t4))) (min(t5) (t6))
        tmax = min (min(max(t1) (t2)) (max(t3) (t4))) (max(t5) (t6))

        ray_result = trace_data (R (V ox oy oz) (V vx vy vz)) (s1:s2:[])
        increase_green (Hit a b c d depth) = (Hit a b c d (depth))
        increase_green Mis = Mis

calcNormal :: Float -> Float -> Shape -> Vertex
calcNormal u v (Triangle _ _ _ a b c _) = (u |*| b) |+| (v |*| c) |+| ((1 - u - v) |*| a)




trace_triangle :: Ray -> Shape -> RayTraceResult
trace_triangle ray shape
        | parallel || utest || vtest || ttest = Mis
        | otherwise = Hit t intersectionPoint triangle_normal shape (round (1000 * (len (v1 |-| intersectionPoint))))
  where v0 = vertex0 shape
        v1 = vertex1 shape
        v2 = vertex2 shape
        e1 = v1 |-| v0
        e2 = v2 |-| v0
        h = crossproduct (rayvector ray) e2
        a = dotproduct e1 h
        parallel = (a < 0.0000001) && (a > -0.0000001)
        f = 1 / a
        s = (rayorigin ray) |-| v0
        u = f * (dotproduct s h)
        utest = u < 0 || u > 1
        q = crossproduct s e1
        v = f * (dotproduct (rayvector ray) q)
        vtest = v < 0 || u + v > 1
        t = (f * (dotproduct e2 q) * 0.99)
        ttest = t < 0.0000001
        intersectionPoint = rayorigin ray |+| (t |*| rayvector ray)
        (Triangle _ _ _ na nb nc _) = shape
        triangle_normal | na == V 0 0 0 && nb == V 0 0 0 && nc == V 0 0 0 = crossproduct e1 e2
                        | otherwise = calcNormal u v shape

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Tracing where

import Debug.Trace

import GHC.Generics (Generic)
import Control.DeepSeq

data Vertex = V Float Float Float deriving (Generic, NFData)
data Shape = Triangle Vertex Vertex Vertex | Box Vertex Vertex Shape Shape | NoShape
data Ray = R Vertex Vertex
-- Hit Distance Location Normal Shape Depth
data RayTraceResult = Mis | Hit Float Vertex Vertex Shape Int

-- L Position Color Intensity
data Light = L Vertex Vertex Float





(|+|) :: Vertex -> Vertex -> Vertex
(V x y z) |+| (V a b c) = V (x + a) (y + b) (z + c)

(|-|) :: Vertex -> Vertex -> Vertex
(V x y z) |-| (V a b c) = V (x - a) (y - b) (z - c)

(|*|) :: Float -> Vertex -> Vertex
k |*| (V x y z) = V (x * k) (y * k) (z * k)

instance Show Light where
  show (L pos color intensity) = "L(" ++ show pos ++ " " ++ show color ++ " " ++ show intensity ++ ")"

instance Show Vertex where
  show (V x y z) = show x ++ " " ++ show y ++ " " ++ show z ++ "\n"

instance Show Shape where
  show (Triangle x y z) = "T(" ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"
  show (Box a b s1 s2) = "B " ++ show a ++ " " ++ show b ++ " (" ++ show s1 ++ " " ++ show s2 ++ ")"

instance Show RayTraceResult where
  show Mis = "Mis"
  show (Hit dist loc normal shape depth)  = "Hit" ++ " " ++ show dist ++ " " ++ show loc ++ " " ++ show normal ++ " " ++ show shape ++ " " ++ show depth

instance Show Ray where
  show (R x y) = "R(" ++ show x ++ " " ++ show y ++ ")"

instance Eq RayTraceResult where
  Mis == Mis = True
  Mis == _ = False
  _ == Mis = False
  _ == _ = True

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

vertex0 (Triangle x _ _) = x
vertex1 (Triangle _ x _) = x
vertex2 (Triangle _ _ x) = x

rayvector (R _ x) = x
rayorigin (R x _) = x

h rayvector = crossproduct

test_hit :: RayTraceResult -> Bool
test_hit (Hit _ _ _ _ _) = True
test_hit Mis = False

trace_light :: Vertex -> [Shape] -> Light -> Bool
trace_light origin shapes light | light_trace == Mis = True
                                | otherwise = False
  where light_loc_func (L x _ _) = x
        light_loc = light_loc_func light
        light_trace = trace_data (R origin (light_loc |-| origin)) shapes

trace_lights :: RayTraceResult -> [Shape] -> [Light] -> [Light]
trace_lights hit shapes lights = filter (trace_light (hitloc hit) shapes) lights
  where hitloc (Hit _ x _ _ _) = x

bounces :: Ray -> RayTraceResult -> Vertex
bounces _ _ = V 0 0 0

angle_intensity :: Float -> Float
angle_intensity x | x < 1.5707964 = calc_angle_intensity x
                  | otherwise = calc_angle_intensity (1.5707964 - (x - 1.5707964))
  where calc_angle_intensity x = 1 - (x / 1.5707964)

-- color -> light -> color
color_intensity_mix :: Vertex -> Float -> Vertex
color_intensity_mix (V x y z) intensity = V (x * intensity) (y * intensity) (z * intensity)

calc_color :: RayTraceResult -> Light -> Vertex
calc_color (Hit _ x normal _ _) (L pos color intensity) = ((1.0 / (d * d)) * (angle_intensity a)) |*| color_intensity_mix color intensity
  where dist_vec = pos |-| x
        d = len dist_vec
        a = angle normal dist_vec

limit_color :: Vertex -> Vertex
limit_color (V x y z) = V (min x 255) (min y 255) (min z 255)
-- trace bounces ray shapes lights -> Color
tracing :: Int -> Ray -> [Shape] -> [Light] -> Vertex
tracing 0 _ _ _ = V 0 0 0
tracing x ray shapes lights | test_hit result = color
                          | otherwise = V 0 0 0
  where result = trace_data ray shapes
        direct_lights = trace_lights result shapes lights
        bounce_light = bounces ray result
        direct_colors = map (calc_color result) direct_lights
        color = limit_color $ (foldr (|+|) (V 0 0 0) direct_colors) -- |+| ((fromIntegral (calc_depth result * 1)) |*| (V 0 1 0)))
        calc_depth (Hit _ _ _ _ depth) = depth

trace_data :: Ray -> [Shape] -> RayTraceResult
trace_data ray [] = Mis
trace_data ray (x:shapes) | test_hit result && distance result < (distance $ trace_data ray shapes)  = result
                        | otherwise = trace_data ray shapes
  where result = trace_ray ray x
        distance (Hit x _ _ _ _) = x
        distance Mis = 99999999999999

trace_ray :: Ray -> Shape -> RayTraceResult
trace_ray ray (Triangle x y z) = trace_triangle ray (Triangle x y z)
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
        increase_green (Hit a b c d depth) = (Hit a b c d (depth+1))
        increase_green Mis = Mis


trace_triangle :: Ray -> Shape -> RayTraceResult
trace_triangle ray shape
        | parallel || utest || vtest || ttest = Mis
        | otherwise = Hit t intersectionPoint (crossproduct e1 e2) shape 0
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

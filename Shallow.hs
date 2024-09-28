{-# LANGUAGE TupleSections #-}
module Shallow where

import Data.List hiding (union)
import qualified Data.Set as S
import Debug.Trace

{-
    Copyright 2024 Toma-Ioan Dumitrescu
-}

{-
    Punct bidimensional, reprezentat ca pereche de coordonate reale (x, y).
    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Point = (Float, Float)

{-
    Tip de funcție care primește un punct, și este parametrizat în raport
    cu tipul rezultatului.
-}
type Pointed a = Point -> a

{-
    Regiune bidimensională, reprezentată ca o funcție caracteristică
    (Point -> Bool). Pentru un punct care aparține regiunii, se întoarce True;
    altfel, False.
-}
type Region = Pointed Bool

{-
    Transformare bidimensională, reprezentată ca o funcție peste puncte.
-}
type Transformation = Point -> Point

{-
    Funcția inside verifică dacă un punct aparține unei
    regiuni (ea însăși reprezentată ca o funcție caracteristică).

    Exemple:

    > inside (0, 0) (== (0, 0))
    True

    > inside (1, 1) (== (0, 0))
    False
-}
inside :: Point -> Region -> Bool
inside = flip ($)

{-
    Funcția fromPoints construiește o regiune pe baza unei liste de puncte.

    Exemple:

    > fromPoints [(0, 0), (1, 1)] (0, 0)
    True

    > inside (0, 0) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    True

    > fromPoints [(0, 0), (1, 1)] (0, 1)
    False

    > inside (0, 1) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    False
-}
fromPoints :: [Point] -> Region
fromPoints = flip elem

{-
    Funcția rectangle generează o regiune aferentă
    unui dreptunghi, cu lățime și înălțime date, simetric față de originea
    (0, 0). De exemplu, un dreptunghi cu lățimea 2 și înălțimea 2 va avea
    punctul din stânga-sus (-1, 1), iar din dreapta-jos, (1, -1).

    Exemple:

    > rectangle 2 2 (0, 0)
    True

    > rectangle 2 2 (-1, 1)
    True

    > rectangle 2 2 (1, -1)
    True

    > rectangle 2 2 (2, 2)  
    False
-}
rectangle :: Float -> Float -> Region
rectangle width height = \point -> ((fst point >= -width / 2)
    && (fst point <= width / 2) && (snd point >= -height / 2)
    && (snd point <= height / 2))

{-
    Funcția circle generează o regiune aferentă unui cerc,
    cu rază dată și centrul în originea (0, 0).

    Exemple:

    > circle 1 (0, 0)
    True

    > circle 1 (1, 0)
    True
    
    > circle 1 (0, 1)
    True
    
    > circle 1 (1, 1)
    False
-}
circle :: Float -> Region
circle radius = \point -> (fst point) ** 2 + (snd point) ** 2 <= radius ** 2

{-
    Funcția plot generează diagrama unei regiuni,
    pe o suprafață de desenare de dimensiuni fixate. Punctul (0, 0)
    se află în centrul suprafeței de desenare, iar lățimea și înălțimea
    unui cadran (dintre cele 4) sunt primite ca parametri. De exemplu, dacă
    lățimea este 2 și înălțimea este 1, punctul din stânga-sus al suprafeței
    este (-2, 1), iar cel din dreapta-jos, (2, -1). Pentru fiecare punct
    cu coordonate întregi de pe suprafața de desenare, se introduce caracterul
    '*', dacă punctul aparține regiunii de desenat, sau '.', altfel.

    Exemple:

    > printPlot 2 1 $ fromPoints [(0, 0), (1, 1)]
    ...*.
    ..*..
    .....

    > printPlot 2 2 $ rectangle 2 2
    .....
    .***.
    .***.
    .***.
    .....

    Deși dimensiunile dreptunghiului sunt 2 și 2, apariția a câte 3 caractere
    '*' pe orizontală și pe verticală poate fi înțeleasă dacă vă gândiți
    la coordonatele vizate, -1, 0 și 1, în toate combinațiile (x, y).

    > printPlot 2 2 $ circle 2     
    ..*..
    .***.
    *****
    .***.
    ..*..
-}
plot :: Int -> Int -> Region -> String
plot width height region = let fpoint x y = (fromIntegral x, fromIntegral y)
                               draw x y = if not $ region $ fpoint x y then '.' else '*'
                               row y = [draw x y | x <- [-width..width]]
                               rows = [row y | y <- reverse [-height..height]]
                               in intercalate "\n" rows

{-
    Funcție pentru vizualizarea diagramelor.
-}
printPlot :: Int -> Int -> Region -> IO ()
printPlot width height region = putStrLn $ plot width height region

{-
    Funcțiile promoteUnary și promoteBinary primesc
    o funcție unară (a -> b), respectiv binară (a -> b -> c), și o promovează
    pentru a opera pe rezultatul(-ele) unor funcții (Point -> a) etc.

    Exemple:

    > promoteUnary (+ 1) (\(x, _) -> x) (3, 2)
    4.0

    > promoteBinary (+) (\(x, _) -> x) (\(_, y) -> y) (3, 2)
    5.0
-}
promoteUnary :: (a -> b) -> Pointed a -> Pointed b
promoteUnary = (.)

promoteBinary :: (a -> b -> c) -> Pointed a -> Pointed b -> Pointed c
promoteBinary f pointed1 pointed2 point = f (pointed1 point) (pointed2 point)

{-
    Funcțiile complement, union și intersection determină
    complementul, reuniunea, respectiv intersecția a două regiuni.

    Exemple:

    > printPlot 2 2 $ complement $ circle 2
    **.**
    *...*
    .....
    *...*
    **.**

    > printPlot 2 2 $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    *....
    ..*..
    .***.
    ..*..
    ....*

    > printPlot 2 2 $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    .....
    .....
    ..*..
    .....
    .....
-}
complement :: Region -> Region
complement = promoteUnary not

union :: Region -> Region -> Region
union = promoteBinary (||)

intersection :: Region -> Region -> Region
intersection = promoteBinary (&&)

{-
    Funcția translation generează o translație
    cu deplasamente primite ca parametri. Deși contraintuitiv, deplasamentele
    trebuie scăzute, nu adunate, din coordonatele punctului transformat.
    De exemplu, dacă punctul (0, 0) aparține unei regiuni de interes, atunci
    punctul (1, 2) va trebui să aparțină regiunii în urma translației
    cu deplasamentele 1 și 2. Din moment ce funcția caracteristică a regiunii
    întoarce True pentru (0, 0), nu pentru (1, 2), cele două deplasamente
    trebuie scăzute.

    Exemple:

    > translation 1 2 (1, 2)
    (0.0,0.0)
-}
translation :: Float -> Float -> Transformation
translation tx ty = \(x, y) -> (x - tx, y - ty)

{-
    Funcția scaling generează o scalare cu un factor primit
    ca parametru. Similar cu observația de la funcția translate, factorul
    contribuie prin împărțire, nu prin înmulțire.

    Exemple:

    > scaling 2 (2, 2)
    (1.0,1.0)
-}
scaling :: Float -> Transformation
scaling factor = \(x, y) -> (x / factor, y / factor)

{-
    Funcția applyTransformation aplică o transformare asupra
    unei regiuni.

    Exemple:

    > printPlot 2 2 $ applyTransformation (translation 1 0) (circle 2)
    ...*.
    ..***
    .****
    ..***
    ...*.

    > printPlot 2 2 $ applyTransformation (scaling 0.5) (circle 2)    
    .....
    ..*..
    .***.
    ..*..
    .....
-}
applyTransformation :: Transformation -> Region -> Region
applyTransformation = flip (.)

{-
    Funcția combineTransformations combină transformările
    dintr-o listă într-o singură transformare. Ordinea de aplicare
    a transformărilor este dată de ordinea lor în listă.

    Exemple:

    > printPlot 2 2 $ applyTransformation
        (combineTransformations [translation 1 0, scaling 0.5]) (circle 2)
    .....
    ...*.
    ..***
    ...*.
    .....
-}
combineTransformations :: [Transformation] -> Transformation
combineTransformations = foldr (flip (.)) id

{-
    Funcția circles de mai jos generează o regiune formată din n cercuri de rază
    2, translatate succesiv cu 6 unități pe orizontală.

    Explicați la prezentare utilitatea evaluării leneșe pentru determinarea
    eficientă a apartenenței unui punct la regiunea construită prin reuniune.

    Exemple:

    > printPlot 15 3 $ circles 3
    ...............................
    ...............*.....*.....*...
    ..............***...***...***..
    .............*****.*****.*****.
    ..............***...***...***..
    ...............*.....*.....*...
    ...............................
-}

circles :: Int -> Region
circles n
    | n <= 0    = const False
    | otherwise = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             (circles (n - 1)))

{-
    Reuniune infinita de cercuri
-}
infiniteCircles :: Region
infiniteCircles = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             infiniteCircles)

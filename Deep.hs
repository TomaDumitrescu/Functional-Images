module Deep where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

{-
    Copyright 2024 Toma-Ioan Dumitrescu
-}

{-
    Deep embeddings pentru regiuni și transformări. Fiecare regiune
    și transformare este reprezentată sub forma unui arbore sintactic
    (abstract syntax tree, AST) ce descrie secvența de operații care contribuie
    la construcția acelei regiuni sau transformări. De exemplu, expresia
    (circles 2), unde circles a fost definită în etapa 1, ar produce acum
    un rezultat similar cu

    Union (Circle 2.0) (Transform (Translation 6.0 0.0) (Circle 2.0)).

    Pentru a obține acest efect, toate funcțiile din etapa 1 sunt reimplementate
    astfel încât să utilizeze direct constructorul de date potrivit al unui
    tip de date. De exemplu, funcția fromPoints *este* acum constructorul
    FromPoints.

    Primul avantaj major al reprezentării bazate pe AST-uri este posibilitatea
    interpretării acesteia în diverse moduri pentru a reconstitui semnificații
    concrete variate ale regiunilor și transformărilor, e.g. regiuni ca funcții
    caracteristice, și transformări ca funcții pe puncte, ca în etapa 1.
    Vom vedea și alte semnificații concrete în etapa 3.

    Al doilea mare avantaj îl constituie posibilitatea simplificării AST-ului
    înaintea interpretării lui într-o manieră specifică. Observați deja cum
    funcțiile combineTransformations și applyTransformation de mai jos, văzute
    ca smart constructors, recunosc anumite cazuri particulare și simplifică
    AST-ul încă de la construcție.
-}
data RegionAST
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement RegionAST
    | Union RegionAST RegionAST
    | Intersection RegionAST RegionAST
    | Transform TransformationAST RegionAST
    deriving (Show, Eq)

data TransformationAST
    = Translation Float Float
    | Scaling Float
    | Combine [TransformationAST]
    deriving (Show, Eq)

fromPoints :: [Point] -> RegionAST
fromPoints = FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle = Rectangle

circle :: Float -> RegionAST
circle = Circle

complement :: RegionAST -> RegionAST
complement = Complement

union :: RegionAST -> RegionAST -> RegionAST
union = Union

intersection :: RegionAST -> RegionAST -> RegionAST
intersection = Intersection

translation :: Float -> Float -> TransformationAST
translation = Translation

scaling :: Float -> TransformationAST
scaling = Scaling

{-
    Smart constructor: dacă lista de transformări este singleton, înlocuiește
    lista cu unica transformare din listă; altfel, utilizează constructorul
    de date Combine.
-}
combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [transformation] = transformation
combineTransformations transformations = Combine transformations

{-
    Smart constructor: dacă se dorește aplicarea unei liste vide de transformări
    asupra unei regiuni, întoarce regiunea ca atare; altfel, utilizează
    constructorul de date Transform.
-}
applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (Combine []) region = region
applyTransformation transformation region = Transform transformation region

{-
    Funcția toTransformation constituie o interpretare
    a AST-ului unei transformări (TransformationAST), în vederea recuperării
    reprezentării concrete din etapa 1, sub forma unei funcții cu tipul
    Transformation = (Point -> Point).
-}
listTransformations :: [TransformationAST] -> [Transformation]
listTransformations [] = []
listTransformations astTransformations = if (length astTransformations) < 2 then [(toTransformation (head astTransformations))]
    else (toTransformation (head astTransformations)) : (listTransformations (tail astTransformations))

toTransformation :: TransformationAST -> Transformation
toTransformation (Translation x y) = S.translation x y
toTransformation (Scaling x) = S.scaling x
toTransformation (Combine astTransformations) = S.combineTransformations (listTransformations astTransformations)


{-
    Funcția toRegion constituie o interpretare a AST-ului
    unei regiuni (RegionAST), în vederea recuperării reprezentării concrete
    din etapa 1, sub forma unei funcții caracteristice cu tipul
    Region = (Point -> Bool).
-}

toRegion :: RegionAST -> Region
toRegion region = case region of
    (FromPoints points) -> S.fromPoints points
    (Rectangle x y) -> S.rectangle x y
    (Circle r) -> S.circle r
    (Complement region) -> S.complement (toRegion region)
    (Union region1 region2) -> S.union (toRegion region1) (toRegion region2)
    (Intersection region1 region2) -> S.intersection (toRegion region1) (toRegion region2)
    (Transform astTransformation region) -> S.applyTransformation (toTransformation astTransformation) (toRegion region)

{-
    Varianta actualizată a a funcției inside.
-}
inside :: Point -> RegionAST -> Bool
inside = flip toRegion

{-
    Funcția decomposeTransformation descompune o transformare
    oricât de complexă într-o listă de transformări elementare (translații
    și scalări), conservând bineînțeles ordinea acestora.

    Exemple:

    > decomposeTransformation $ Translation 1 2
    [Translation 1.0 2.0]

    > decomposeTransformation $ Scaling 2
    [Scaling 2.0]

    > decomposeTransformation $
        Combine [ Translation 1 2
                , Combine [ Translation 3 4
                          , Scaling 2
                          ]
                , Scaling 3
                ]
    [Translation 1.0 2.0,Translation 3.0 4.0,Scaling 2.0,Scaling 3.0]
-}
decomposeTransformation :: TransformationAST -> [TransformationAST]
decomposeTransformation transformation = case transformation of
    (Translation x y) -> [Translation x y]
    (Scaling x) -> [Scaling x]
    (Combine transformations) -> foldl (\res transformation -> res ++ (decomposeTransformation transformation)) [] transformations

{-
    Funcția fuseTransformations alipește transformările
    adiacente de același fel (translații cu translații și scalări cu scalări)
    dintr-o listă de transformări elementare (translații și scalări),
    și întoarce lista transformărilor rezultante.

    > fuseTransformations [Translation 1 2]
    [Translation 1.0 2.0]

    > fuseTransformations [Scaling 2, Scaling 3]             
    [Scaling 6.0]

    > fuseTransformations [ Translation 1 2, Translation 3 4
                          , Scaling 2, Scaling 3
                          , Translation 5 6
                          ]
    [Translation 4.0 6.0,Scaling 6.0,Translation 5.0 6.0]
-}
fuseTwo :: [TransformationAST] -> TransformationAST -> [TransformationAST]
fuseTwo [] transformation = [transformation]
fuseTwo transformations transformation = let len = (length transformations) - 1
    in case (last transformations, transformation) of
        (Translation x y, Translation a b) -> (take len transformations) ++ [Translation (x + a) (y + b)]
        (Scaling x, Scaling y) -> (take len transformations) ++ [Scaling (x * y)]
        _ -> transformations ++ [transformation]

fuseTransformations :: [TransformationAST] -> [TransformationAST]
fuseTransformations transformations = case transformations of
    [Translation x y] -> [Translation x y]
    [Scaling x] -> [Scaling x]
    _ -> foldl (\res transformation -> (fuseTwo res transformation)) [] transformations

{-
    Funcția optimizeTransformations simplifică toate
    transformările din AST-ul unei regiuni. Principiile sunt următoarele:

    * Transformările succesive trebuie descompuse și alipite.
    * Pentru a evidenția lanțuri cât mai lungi de transformări succesive,
      se urmărește deplasarea în sus a transformărilor din AST, astfel:
      * Complementul unei transformări este transformarea complementului.
      * O ramificare (reuniune sau intersecție) de transformări de regiuni
        presupune determinarea celui mai lung prefix de transformări comune
        de pe cele două ramuri și deplasarea acestuia deasupra ramificării,
        păstrând sub ea sufixele de transformări necomune.
    * O regiune elementară (din puncte, dreptunghi sau cerc) se consideră
      optimizată.
    * Toate cosmetizările de mai sus se realizează după optimizarea recursivă
      a subregiunilor.

    Exemple:

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Circle 5)
    Transform (Combine [Translation 4.0 6.0,Scaling 6.0]) (Circle 5.0)

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Transform (Scaling 4)
                             (Transform (Scaling 2) (Circle 5)))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Circle 5.0)

    > optimizeTransformations $
        Complement (Transform (Scaling 4)
                              (Transform (Scaling 2) (Circle 5)))
    Transform (Scaling 8.0) (Complement (Circle 5.0))

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Complement (Transform (Scaling 4)
                                         (Transform (Scaling 2) (Circle 5))))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Complement (Circle 5.0))

    > optimizeTransformations $
        Union (Complement (Transform (Scaling 4)
                                     (Transform (Scaling 2) (Circle 5))))
              (Rectangle 6 7)
    Union (Transform (Scaling 8.0) (Complement (Circle 5.0))) (Rectangle 6.0 7.0)

    > optimizeTransformations $
        Union (Transform (Combine [ Translation 1 2
                                  , Combine [ Translation 3 4
                                            , Scaling 2
                                            ]  
                                  , Scaling 3
                                  ])
                         (Complement (Transform (Scaling 4)
                                                (Transform (Scaling 2) (Circle 5)))))
              (Transform (Translation 4 6) (Rectangle 6 7))
    Transform (Translation 4.0 6.0)
              (Union (Transform (Scaling 48.0) (Complement (Circle 5.0)))
                     (Rectangle 6.0 7.0))
-}

commonPrefix :: RegionAST -> RegionAST
commonPrefix region = region

deepMerge :: [TransformationAST] -> [TransformationAST] -> [TransformationAST]
deepMerge transformations result = case transformations of
    [] -> result
    notNullTransfList -> case (head notNullTransfList) of
        (Combine transfList) -> result ++ (deepMerge transfList []) ++ (deepMerge (tail notNullTransfList) [])
        (Translation x y) -> result ++ [Translation x y] ++ (deepMerge (tail notNullTransfList) [])
        (Scaling x) -> result ++ [Scaling x] ++ (deepMerge (tail notNullTransfList) [])

deepFuse :: [TransformationAST] -> [TransformationAST]
deepFuse transformations = fuseTransformations (deepMerge transformations [])

reduceTransformations :: TransformationAST -> RegionAST -> RegionAST
reduceTransformations transf1 region = case region of
    (Transform transf2 reg) -> applyTransformation (combineTransformations (deepFuse ((decomposeTransformation transf1) ++ (decomposeTransformation transf2)))) reg
    _ -> applyTransformation (combineTransformations (deepFuse (decomposeTransformation transf1))) region

optimizeTransformations :: RegionAST -> RegionAST
optimizeTransformations region = case region of
    (Complement reg) -> case reg of
        (Transform transf reg) -> reduceTransformations transf (optimizeTransformations (Complement reg))
        reg2 -> (Complement reg2)
    (Union reg1 reg2) -> commonPrefix (Union (optimizeTransformations reg1) (optimizeTransformations reg2))
    (Intersection reg1 reg2) -> commonPrefix (Intersection (optimizeTransformations reg1) (optimizeTransformations reg2))
    (Transform transf reg) -> reduceTransformations transf (optimizeTransformations reg)
    elementary_region -> elementary_region

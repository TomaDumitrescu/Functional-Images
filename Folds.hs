{-# OPTIONS_GHC -Wno-missing-methods #-}
module Folds where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

{-
    Copyright 2024 Toma-Ioan Dumitrescu
-}

{-
    Contructor de tip care surprinde regiunile elementare și modalitățile
    generice de construcție a regiunilor complexe (Complement etc.). Spre
    deosebire de etapa 2, în care constructorul de tip RegionAST nu era
    parametrizat, constructorul RegionShape este acum parametrizat cu un tip
    generic de informație, a, care înlocuiește referirile recursive la tipul
    RegionAST. De exemplu, (Complement RegionAST) din etapa 2 devine acum
    (Complement a). Cuvântul „shape” din numele tipului vizează formele generice
    (Complement, Union etc.) pe care le pot lua valorile acestui tip, fără
    a fixa tipul câmpurilor. Așa cum vom vedea în continuare, acest artificiu
    permite atât recuperarea tipului recursiv RegionAST, cât și implementarea
    unui mecanism generic de folding (reducere) al valorilor acestui tip.
-}
data RegionShape a
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement a
    | Union a a
    | Intersection a a
    | Transform TransformationAST a
    deriving (Show, Eq)

{-
    Contructor de tip care surprinde transformările elementare și o variantă
    generică de construcție a transformărilor complexe (Combine). Explicațiile
    referitoare la RegionShape sunt valabile și aici.
-}
data TransformationShape a
    = Translation Float Float
    | Scaling Float
    | Combine [a]
    deriving (Show, Eq)

{-
    Tipul recursiv RegionAST din etapa 2 se poate recupera particularizând
    tipul a din definiția constructorului RegionShape la... RegionAST însuși.
    Cu alte cuvinte, RegionAST este un punct fix al constructorului de tip
    RegionShape, adică RegionAST = RegionShape RegionAST (egalul ar trebui citit
    mai degrabă ca „izomorf cu” în loc de „identic cu”, din perspectiva
    limbajului). Vă puteți convinge de relația de mai sus substituind textual
    aparițiile lui a din definiția lui RegionShape cu RegionAST; de exemplu,
    (Complement a) devine (Complement RegionAST).

    newtype este similar cu data, în sensul că definește un tip nou, dar poate
    fi folosit doar când precizăm un unic constructor de date (R de mai jos),
    care posedă un unic câmp (având mai jos tipul (RegionShape RegionAST)).
    Există și alte diferențe subtile pe care nu le vom detalia, dar îl puteți
    folosi în continuare ca pe un data obișnuit.
-}
newtype RegionAST
    = R (RegionShape RegionAST)
    deriving (Eq)

{-
    Similar se poate recupera și tipul TransformationAST din etapa 2.
-}
newtype TransformationAST
    = T (TransformationShape TransformationAST)
    deriving (Eq)

{-
    Funcțiile pe regiuni și transformări au implementări foarte similare celor
    din etapa 2, singura diferență constând în apariția constructorilor de date
    R și T de mai sus, pentru a reflecta noile definiții ale tipurilor RegionAST
    și TransformationAST în termenii constructorilor de tip RegionShape,
    respectiv TransformationShape.
-}
fromPoints :: [Point] -> RegionAST
fromPoints = R . FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle width height = R $ Rectangle width height

circle :: Float -> RegionAST
circle = R . Circle

complement :: RegionAST -> RegionAST
complement = R . Complement

union :: RegionAST -> RegionAST -> RegionAST
union region1 region2 = R $ Union region1 region2

intersection :: RegionAST -> RegionAST -> RegionAST
intersection region1 region2 = R $ Intersection region1 region2

translation :: Float -> Float -> TransformationAST
translation tx ty = T $ Translation tx ty

scaling :: Float -> TransformationAST
scaling = T . Scaling

{-
    Smart constructor: dacă lista de transformări este singleton, înlocuiește
    lista cu unica transformare din listă; altfel, utilizează constructorul
    de date Combine.
-}
combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [transformation] = transformation
combineTransformations transformations = T $ Combine transformations

{-
    Smart constructor: dacă se dorește aplicarea unei liste vide de transformări
    asupra unei regiuni, întoarce regiunea ca atare; altfel, utilizează
    constructorul de date Transform.
-}
applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (T (Combine [])) region = region
applyTransformation transformation region = R $ Transform transformation region

{-
    Clasa Show cu tipul TransformationAST, pentru a reprezenta
    transformările sub formă de șir de caractere, astfel:

    * o translație cu parametrii tx și ty devine "+(tx,ty)"
    * o scalare cu factorul f devine "*<f>"
    * o transformare compusă este reprezentată ca lista Haskell
      a transformărilor constitutive.

    Exemple:

    > combineTransformations [ translation 1 2
                             , scaling 3
                             , combineTransformations [ scaling 4
                                                      , translation 5 6
                                                      ]
                             ]
    [+(1.0,2.0),*<3.0>,[*<4.0>,+(5.0,6.0)]]
-}

toStringTransformations :: [TransformationAST] -> String
toStringTransformations transformations = case transformations of
    [] -> ""
    [transformation] -> show transformation ++ "]"
    notNullList -> show (head notNullList) ++ "," ++ toStringTransformations (tail notNullList)


instance Show TransformationAST where
    show (T transformation) = case transformation of
        (Translation tx ty) -> "+(" ++ show tx ++ "," ++ show ty ++ ")"
        (Scaling f) -> "*<" ++ show f ++ ">"
        (Combine transformations) -> "[" ++ toStringTransformations transformations

{-
    Clasa Show cu tipul RegionAST, pentru a reprezenta regiunile
    sub formă de șir de caractere, astfel:

    * fiecare nod din AST apare pe propriul rând
    * un nod copil este indentat cu două spații față de părintele său
    * un nod frunză FromPoints, Rectangle sau Circle este afișat ca în etapa 2
    * un nod Transform este reprezentat prin transformarea aferentă
    * un nod Complement este reprezentat prin caracterul '~'
    * un nod Union este reprezentat prin caracterul '+'
    * un nod Intersection este reprezentat prin caracterul '*'.

    Exemple:

    > union (complement (fromPoints [(1, 1)]))
            (intersection (applyTransformation (translation 1 2) (circle 3))
                          (rectangle 4 5))
    +
      ~
        FromPoints [(1.0,1.0)]
      *
        +(1.0,2.0)
          Circle 3.0
        Rectangle 4.0 5.0
-}

heightIndent :: String -> Int -> String
heightIndent initSpaces level = if level == 0 then initSpaces else (heightIndent (initSpaces ++ " ") (level - 1))

toStringRegion :: RegionAST -> Int -> String
toStringRegion (R region) level = 
        let space = (heightIndent "" level)
        in case region of
            (FromPoints points) -> space ++ "FromPoints " ++ show points
            (Rectangle x y) -> space ++ "Rectangle " ++ show x ++ " " ++ show y
            (Circle r) -> space ++ "Circle " ++ show r
            (Complement reg) -> space ++ "~\n" ++ toStringRegion reg (level + 2)
            (Union reg1 reg2) -> space ++ "+\n" ++ toStringRegion reg1 (level + 2) ++ "\n" ++ toStringRegion reg2 (level + 2)
            (Intersection reg1 reg2) -> space ++ "*\n" ++ toStringRegion reg1 (level + 2) ++ "\n" ++ toStringRegion reg2 (level + 2)
            (Transform transf reg) -> space ++ show transf ++ "\n" ++ toStringRegion reg (level + 2)

instance Show RegionAST where
    show (R region) = toStringRegion (R region) 0

{-
    Clasa Num are tipul RegionAST privit ca pe un tip numeric, astfel:

    * interpretarea unui întreg n este regiunea care include doar punctul (n, n)
    * negația (minus unar) unei regiuni corespunde complementului
    * suma a două regiuni corespunde reuniunii
    * produsul a două regiuni corespunde intersecției.

    Deși nu am definit până acum constructori de date sau funcții pentru
    reprezentarea diferenței a două regiuni (operatorul (-) de mai jos),
    ea poate fi implementată ca intersecție cu complementul scăzătorului.

    Exemple:

    > fromInteger 1 :: RegionAST   
    FromPoints [(1.0,1.0)]

    > 1 :: RegionAST  -- aici se invocă implicit fromInteger pe 1
    FromPoints [(1.0,1.0)]

    > fromInteger (-1) :: RegionAST
    FromPoints [(-1.0,-1.0)]

    > -1 :: RegionAST  -- aici se invocă implicit fromInteger pe 1
    ~
      FromPoints [(1.0,1.0)]

    > circle 3 - rectangle 4 5        
    *
      Circle 3.0
      ~
        Rectangle 4.0 5.0

    > -1 + applyTransformation (translation 1 2) (circle 3) * rectangle 4 5
    +
      ~
        FromPoints [(1.0,1.0)]
      *
        +(1.0,2.0)
          Circle 3.0
        Rectangle 4.0 5.0
-}
instance Num RegionAST where
    fromInteger n = let x = (fromIntegral n) in (fromPoints [(x, x)])

    negate = complement

    (+) = union

    (*) = intersection

    {-
        Diferența regiunilor trebuie implementată exclusiv prin alți operatori
        aritmetici pe regiuni, definiți în instanța curentă.
    -}
    region1 - region2 = region1 * (negate region2)

{-
    Clasa Functor cu constructorul TransformationShape. Funcția f,
    cu tipul (a -> b), pe care fmap o ia ca parametru, poate fi aplicată numai
    unde există un câmp de tipul a. Celelalte valori își păstrează forma
    originală.

    Exemple:

    > fmap (+ 1) $ Translation 1 2  -- am particularizat a la un tip numeric
    Translation 1.0 2.0

    > (+ 1) <$> Translation 1 2  -- (<$>) este sinonim pentru fmap
    Translation 1.0 2.0

    > fmap (+ 1) $ Scaling 3
    Scaling 3.0

    > fmap (+ 1) $ Combine [1, 2, 3]
    Combine [2,3,4]
-}
instance Functor TransformationShape where
    -- fmap :: (a -> b) -> TransformationShape a -> TransformationShape b
    fmap f transformation = case transformation of
        (Combine transformations) -> Combine (map f transformations)
        (Translation x y) -> (Translation x y)
        (Scaling x) -> (Scaling x)

{-
    Clasa Functor cu constructorul RegionShape. Funcția f,
    cu tipul (a -> b), pe care fmap o ia ca parametru, poate fi aplicată numai
    unde există un câmp de tipul a. Celelalte valori își păstrează forma
    originală.

    Exemple:

    > fmap (+ 1) $ Circle 1  -- am particularizat a la un tip numeric
    Circle 1.0

    > fmap (+ 1) $ Union 1 2
    Union 2 3
-}
instance Functor RegionShape where
    -- fmap :: (a -> b) -> RegionShape a -> RegionShape b
    fmap f region = case region of
        (FromPoints points) -> (FromPoints points)
        (Rectangle x y) -> (Rectangle x y)
        (Circle r) -> (Circle r)
        (Complement reg) -> (Complement (f reg))
        (Union reg1 reg2) -> (Union (f reg1) (f reg2))
        (Intersection reg1 reg2) -> (Intersection (f reg1) (f reg2))
        (Transform transf reg) -> (Transform transf (f reg))

{-
    Tipul (TransformationCombiner a) include funcții care pot combina câmpurile
    de tipul a din interiorul unei valori de tipul (TransformationShape a) la o
    singură valoare de tipul a.

    Tipul (RegionCombiner a) include funcții care pot combina câmpurile de tipul
    a din interiorul unei valori de tipul (RegionShape a) la o singură valoare
    de tipul a.

    În continuare, ne vom referi la aceste funcții prin „combiner-e”.
-}
type TransformationCombiner a = TransformationShape a -> a
type RegionCombiner a = RegionShape a -> a

{-
    Funcționalele foldTransformationAST și foldRegionAST au
    definiții aproape identice, pentru a putea reduce (fold) AST-uri de tipul
    TransformationAST, respectiv RegionAST, la o singură valoare de tipul a,
    pornind de la combiner-e cu tipurile (TransformationCombiner a), respectiv
    (RegionCombiner a).

    Pentru definirea unei funcționale fold...AST, se urmează doi pași:

    1. Se reduc mai întâi subarborii folosind recursiv funcționala fold...AST,
       pentru a înlocui subarborii de tipul ...AST cu acumulatori de tipul a.
    2. Se aplică combiner-ul pe rezultat, pentru a combina acumulatorii interni
       de tipul a într-o singură valoare de tipul a.

    Cele două funcționale vor avea câte o unică definiție (nu este necesar
    să faceți pattern matching pe forma parametrilor, e.g. translație,
    complement etc.), întrucât forma acestora este gesionată de combiner-ul
    primit ca parametru.

    Exemple:

    labelCombiner :: TransformationCombiner String
    labelCombiner (Translation _ _) = "t"
    labelCombiner (Scaling _) = "s"
    labelCombiner (Combine labels) = concat labels

    > foldTransformationAST labelCombiner $
        combineTransformations [ translation 1 2
                               , scaling 3
                               , combineTransformations [ scaling 4
                                                        , translation 5 6
                                                        ]
                               ]
    "tsst"

    Observați că, în ultima definiție a lui labelCombiner, pentru consructorul
    Combine, asumăm că acel câmp cu tipul [TransformationAST] a fost deja redus
    la (labels :: [String]), și singura preocupare este de a combina șirurile
    intermediare într-un nou șir.
-}
foldTransformationAST :: TransformationCombiner a -> TransformationAST -> a
foldTransformationAST f (T transformation) = case transformation of
        (Translation x y) -> f (Translation x y)
        (Scaling a) -> f (Scaling a)
        (Combine transformations) -> f (Combine (fmap (foldTransformationAST f) transformations))


foldRegionAST :: RegionCombiner a -> RegionAST -> a
foldRegionAST f (R region) = case region of
        (FromPoints points) -> f (FromPoints points)
        (Rectangle x y) -> f (Rectangle x y)
        (Circle r) -> f (Circle r)
        (Complement reg) -> f (Complement (foldRegionAST f reg))
        (Union reg1 reg2) -> f (Union (foldRegionAST f reg1) (foldRegionAST f reg2))
        (Intersection reg1 reg2) -> f (Intersection (foldRegionAST f reg1) (foldRegionAST f reg2))
        (Transform transf reg) -> f (Transform transf (foldRegionAST f reg))

{-
    Funcția toTransformation din etapa 2, ca o reducere a unui
    TransformationAST.
-}

toTransformation :: TransformationAST -> Transformation
toTransformation = foldTransformationAST combiner
  where
    combiner :: TransformationCombiner Transformation
    combiner transformation = case transformation of
        (Translation x y) -> S.translation x y
        (Scaling x) -> S.scaling x
        (Combine astTransformations) -> S.combineTransformations astTransformations

{-
    Funcția basicTransformationCount, ca o reducere a unui
    TransformationAST, care numără transformările elementare (translații
    și scalări) din cadrul unei alte transformări.

    Exemple:

    > basicTransformationCount (translation 1 2)
    1

    > basicTransformationCount (scaling 3)
    1

    > basicTransformationCount $
        combineTransformations [ translation 1 2
                               , combineTransformations [ translation 3 4
                                                        , scaling 2
                                                        ]
                               , scaling 3
                               ]
    4
-}
basicTransformationCount :: TransformationAST -> Int
basicTransformationCount = foldTransformationAST combiner
  where
    combiner :: TransformationCombiner Int
    combiner transformation = case transformation of
        (Translation x y) -> 1
        (Scaling x) -> 1
        (Combine transformations) -> foldl (+) 0 transformations

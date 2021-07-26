module Puut where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

{-
OMA LISÄYS
Tiedosto pelin puulle. Mainissa puut luodaan listaan.
Puun tietotyyppi, osumisen tarkastelu,
piirto ja nurkkapisteiden määritys.
-}

data Puu = Puu {puu_korkeus :: Float, puu_leveys :: Float
                 ,puu_sijainti :: Float }

osuukoPuuhun :: Float -> Puu -> Float
osuukoPuuhun kohta puu
    | abs (puu_sijainti puu - kohta) < (puu_leveys puu / 2) = puu_korkeus
      puu
    | otherwise = 0

piirräPuu :: Puu -> Picture
piirräPuu puu = runko <> lehdet
    where
        lehdet = translate (puu_sijainti puu) (puu_korkeus puu) (color (dark green)
                            (circleSolid (puu_leveys puu)))
        runko = translate (puu_sijainti puu) (puu_korkeus puu / 2) (color (makeColorI 150 100 0 255)
                            (rectangleSolid (puu_korkeus puu / 4) (puu_korkeus puu)))

-- type Point = (Float,Float)
puunNurkkaPisteet :: Puu -> (Point,Point)
puunNurkkaPisteet puu = 
    let
        vasenAla = (((puu_sijainti puu) - (puu_leveys puu / 2) + 20) , 0)
        oikeaYlä = (((puu_sijainti puu) + (puu_leveys puu / 2) - 20) , puu_korkeus puu) 
    in (vasenAla,oikeaYlä)
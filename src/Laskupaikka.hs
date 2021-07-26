module Laskupaikka where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

{-
OMA LISÄYS
Tiedosto laskupaikalle, eli helikopterikentälle.
Helikopterikentän tietotyyppi, osumisen tarkastelu,
piirto ja nurkkapisteiden määritys.
-}

data Laskupaikka = Laskupaikka {lp_korkeus :: Float, lp_leveys :: Float
                 ,lp_sijainti :: Float }

osuukoLaskupaikkaan :: Float -> Laskupaikka -> Float
osuukoLaskupaikkaan kohta laskupaikka
    | abs (lp_sijainti laskupaikka - kohta) < (lp_leveys laskupaikka / 2) = lp_korkeus
      laskupaikka
    | otherwise = 0

piirräLaskupaikka :: Laskupaikka -> Picture
piirräLaskupaikka laskupaikka = translate (lp_sijainti laskupaikka) ((lp_korkeus laskupaikka )/ 2 - 50) (color (greyN 0.2) 
                                (rectangleSolid (lp_leveys laskupaikka) (lp_korkeus laskupaikka)))

-- type Point = (Float,Float)
laskupaikanNurkkaPisteet :: Laskupaikka -> (Point,Point)
laskupaikanNurkkaPisteet laskupaikka = 
    let
        vasenAla = (lp_sijainti laskupaikka - (lp_leveys laskupaikka / 2) , 0)
        oikeaYlä = (lp_sijainti laskupaikka + (lp_leveys laskupaikka / 2)  , (lp_korkeus laskupaikka -50)) 
    in (vasenAla,oikeaYlä)
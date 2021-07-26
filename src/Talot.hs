module Talot where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

data Talo = Talo {talo_korkeus :: Float, talo_leveys :: Float
                 ,talo_sijainti :: Float }

osuukoTaloon :: Float -> Talo -> Float
osuukoTaloon kohta talo
    | abs (talo_sijainti talo - kohta) < (talo_leveys talo / 2) = talo_korkeus
      talo
    | otherwise = 0

{-
OMA LISÄYS
Lisätty ikkunat ja ikkunoihin liekit.
-}
piirräTalo :: Float -> Talo -> Picture
piirräTalo aika talo = let
                   paikoillaan = talonKuva <> ikkuna1 <> ikkuna2 <> ikkuna3 <> ikkuna4

                   talonKuva = translate (talo_sijainti talo) (talo_korkeus talo / 2) (color (greyN 0.5) 
                                (rectangleSolid (talo_leveys talo) (talo_korkeus talo)))
                   ikkunaKuva = color (greyN 0.8) (rectangleSolid 100 100)
                                    <> color orange (polygon [((-40),(-20)), liekkiPiste, (40,(-20))
                                        ,(30,(-40)), (0,(-50)), ((-30),(-40)), ((-40),(-20))])
                   liekkiPiste = (0,55 + sin (10*aika) * 18)

                   ikkuna1 = translate (talo_sijainti talo - 100) (talo_korkeus talo - 200) ikkunaKuva
                   ikkuna2 = translate (talo_sijainti talo + 100) (talo_korkeus talo -200) ikkunaKuva
                   ikkuna3 = translate (talo_sijainti talo - 100) (talo_korkeus talo -400) ikkunaKuva
                   ikkuna4 = translate (talo_sijainti talo + 100) (talo_korkeus talo -400) ikkunaKuva
                  in paikoillaan

-- type Point = (Float,Float)
talonNurkkaPisteet :: Talo -> (Point,Point)
talonNurkkaPisteet talo = 
    let
        vasenAla = (talo_sijainti talo - (talo_leveys talo / 2) , 0)
        oikeaYlä = (talo_sijainti talo + (talo_leveys talo / 2)      , talo_korkeus talo) 
    in (vasenAla,oikeaYlä)
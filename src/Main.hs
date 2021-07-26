module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)

import Aritmetiikka
import Hemmot
import Kopteri
import Talot
import Puut
import Laskupaikka

{-
TIIVISTELMÄ OMISTA LISÄYKSISTÄ

Toiminnallisia lisäyksiä:
Laajennettu lisäämällä taloja ja hemmoja.
Lisätty puita, joihin voi törmätä ja helikopterikenttä, johon laskeudutaan.
Lisätty pelin voittaminen.

Visuaalisia lisäyksiä:
Lisätty taloihin ikkunat ja liikkuvat liekit.
Lisätty kopteriin pyörivä pyrstöroottori ja muutettu ulkonäköä.
Lisätty hemmoille liikettä ja muutettu ulkonäköä.
Lisätty peliin ohjeteksti, joka kertoo pelastettavien hemmojen määrän ja
ohjaa laskeutumaan helikopterikentälle.
-}


{-
OMA LISÄYS
Lisätty enemmän taloja ja hemmoja.
Lisätty puita ja helikopterikenttä.
-}
alkutilanne :: PeliTilanne 
alkutilanne =
    GameOn 
      (Peli 
       0 
       (luoKopteri (0,0))
       [Talo 700 500 (-700), Talo 800 500 700, Talo 900 600 (-1700)]
       [Hemmo (700, 800), Hemmo (900, 800), Hemmo ((-700), 700), Hemmo ((-900),700)
       , Hemmo ((-1600), 900), Hemmo ((-1800), 900), Hemmo ((-1500), 900)]
       [Puu 200 80 200, Puu 300 90 (-1200), Puu 250 85 1100]
       (Laskupaikka 50 300 (-50))
      )

main :: IO ()
main = play 
        (InWindow "Choplifter" (1000,600) (300,300))
        (light blue)
        24
        alkutilanne 
        piirräPeliTilanne
        reagoiPeliTilanne
        päivitäPelitilanne

{-
OMA LISÄYS
Lisätty pelin voittotilanteen käsittely.
-}
reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne 
    = case pelitilanne of
        GameWin cl -> GameWin cl
        GameOver cl -> GameOver cl
        GameOn cl -> GameOn (reagoi tapahtuma cl)

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli 
    = case tapahtuma of
        EventKey (Char 'w') Down _ _ -> kopterille (muutaTehoa 2.5) peli
        EventKey (Char 's') Down _ _ -> kopterille (muutaTehoa (-2.5)) peli
        EventKey (Char 'a') Down _ _ -> kopterille (kallista (-8)) peli
        EventKey (Char 'd') Down _ _ -> kopterille (kallista (8)) peli
        _ -> peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli{cl_kopteri = f (cl_kopteri peli)} 

{-
OMA LISÄYS
Lisätty puihin törmääminen.
Lisätty pelin voittaminen keräämällä kaikki hemmot ja palaamalla helikopterikentälle.
-}
päivitäPelitilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPelitilanne aikaEdellisestä pelitilanne 
    = case pelitilanne of
        GameWin cl -> GameWin cl
        GameOver cl -> GameOver cl
        GameOn cl   -> case törmääköPuuhun (kopteriTörmäysviivat (cl_kopteri cl)) (cl_puut cl) of
                                Just Roottori -> GameOver cl
                                _ -> case  törmääköTaloon (kopteriTörmäysviivat (cl_kopteri cl)) (cl_talot cl)  of
                                        Nothing -> case length (cl_hemmot cl) of
                                                     0 -> case osuukoLaskeutuminen (kopteriTörmäysviivat (cl_kopteri cl)) (cl_laskupaikka cl) of
                                                            Just Laskuteline -> GameWin cl
                                                            _ -> GameOn (päivitäPeliä aikaEdellisestä cl)
                                                     _ -> GameOn (päivitäPeliä aikaEdellisestä cl)
                                        Just Roottori -> GameOver cl
                                        Just Laskuteline 
                                            | onkoHyväLaskeutuminen (cl_kopteri cl)
                                                -> GameOn (päivitäPeliä aikaEdellisestä 
                                                            (kopterille laskeudu cl))
                                            | otherwise -> GameOver cl

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
     Peli aika kopteri talot hemmot puut laskupaikka
        -> let
            nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- kop_paikka kopteri) < 50
            (hemmotKopteriin,hemmotUlkona) = partition nouseekoKyytiin hemmot
           in Peli (aika + aikaEdellisestä) 
                   (noukiHemmot hemmotKopteriin . päivitäKopteri aikaEdellisestä $ kopteri)
                   talot
                   (map (päivitäHemmoa (flip korkeusKohdassa edellinenTila) (kop_paikka kopteri)) 
                        hemmotUlkona)
                    puut
                    laskupaikka

data TörmäysKohta = Laskuteline | Roottori 
        deriving (Eq,Ord,Show)

törmääköTaloon :: ((Point,Point),(Point,Point)) -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
    where
     törmääköYhteen talo 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
            (va,oy)   = talonNurkkaPisteet talo 
          in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
                (True,False) -> Just Laskuteline
                (False,False) -> Nothing
                _ -> Just Roottori

{-
OMA LISÄYS
Luotu puihin törmäämisen tarkastelu.
-}
törmääköPuuhun :: ((Point,Point),(Point,Point)) -> [Puu] -> Maybe TörmäysKohta
törmääköPuuhun törmäysviivat puut = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen puut))
    where
     törmääköYhteen puu 
        = let 
            ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
            (va,oy)   = puunNurkkaPisteet puu 
          in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
                (False,False) -> Nothing
                _ -> Just Roottori

{-
OMA LISÄYS
Luotu helikopterikentälle laskeutumisen (törmäämisen) tarkastelu.
-}
osuukoLaskeutuminen :: ((Point,Point),(Point,Point)) -> Laskupaikka -> Maybe TörmäysKohta
osuukoLaskeutuminen törmäysviivat laskupaikka
    = let 
      ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
      (va,oy)   = laskupaikanNurkkaPisteet laskupaikka
     in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
          (True,False) -> Just Laskuteline
          (False,False) -> Nothing
          _ -> Just Roottori

{-
OMA LISÄYS
Lisätty pelin voittotilanteen teksti.
-}
piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne 
    = case pelitilanne of
        GameWin cl  -> piirräPeli cl <> translate (-300) 0 (color yellow (text "YOU WIN!"))
        GameOver cl -> piirräPeli cl <> translate (-300) 0 (color yellow (text "GAME OVER"))
        GameOn cl   -> piirräPeli cl

{-
OMA LISÄYS
Lisätty peliin ohjeteksti (peliviesti).
Lisätty puiden ja helikopterikentän piirto peliin.
-}
piirräPeli :: Choplifter -> Picture
piirräPeli peli = let
                   kopteriKuva = piirräKopteri (cl_aika peli) (cl_kopteri peli)
                   laskupaikkaKuva = piirräLaskupaikka (cl_laskupaikka peli)
                   peliViesti = if length (cl_hemmot peli) > 0
                                  then text ("Pelasta " <> show (length (cl_hemmot peli)) <> " hemmoa")
                                else text ("Palaa takaisin kopterikentalle")

                   hemmoKuvat = map (piirräHemmo (cl_aika peli))  (cl_hemmot peli)
                   taloKuvat  = map (piirräTalo (cl_aika peli))  (cl_talot peli)
                   puuKuvat = map piirräPuu (cl_puut peli)

                   peliKuva = maa  
                              <> pictures taloKuvat
                              <> pictures hemmoKuvat
                              <> pictures puuKuvat
                              <> laskupaikkaKuva
                              <> kopteriKuva
                                        
                  in scale 0.2 0.2 (translate (-700) 1100 peliViesti)
                        <> scale 0.25 0.25 (translate 0 (-180) peliKuva)

{-
OMA LISÄYS
Lisätty voitto yhdeksi pelitilanteeksi.
-}
data PeliTilanne = GameWin Choplifter | GameOver Choplifter | GameOn Choplifter

{-
OMA LISÄYS
Lisätty pelille kentät puille ja laskupaikalle.
-}
data Choplifter 
 = Peli 
   {
     cl_aika   :: Float            -- Aika pelin alusta
    ,cl_kopteri :: Kopteri         -- kopterin tiedot 
    ,cl_talot  :: [Talo]           -- Esteet pelissä
    ,cl_hemmot :: [Hemmo]          -- Pelihahmot
    ,cl_puut :: [Puu]              -- Puut pelissä
    ,cl_laskupaikka :: Laskupaikka -- Kopterin laskupaikka
   }

korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
  maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))
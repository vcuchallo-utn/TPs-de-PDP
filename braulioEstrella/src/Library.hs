module Library where
import PdePreludat
import GHC.Integer (Integer)
import Foreign (Int)

data Personaje = UnPersonaje {
    nombre :: String,
    poderBasico :: (Personaje->Personaje),
    superPoder :: (Personaje->Personaje),
    estaSuperActivo :: Bool,
    vida :: Number
} deriving Show

modificarVida :: (Number->Number)->Personaje->Personaje
modificarVida unaFuncion personaje = personaje {
    vida = max (unaFuncion $ vida personaje) 0
    }

bolaEspinosa :: Personaje->Personaje
lluviaDeTuercas :: String->Personaje->Personaje
granadaDeEspinas :: Number->Personaje->Personaje
torretaCurativa :: Personaje->Personaje

bolaEspinosa contrincante = modificarVida (subtract 1000) contrincante

lluviaDeTuercas "Daninas" personaje = modificarVida (/2) personaje 
lluviaDeTuercas "Sanadoras" personaje = modificarVida (+800) personaje 
lluviaDeTuercas _ personaje = personaje

granadaDeEspinas radio personaje
    | radio>3 && (vida personaje) < 800 = bolaEspinosa personaje {
        nombre = (nombre personaje) ++" Espina estuvo aqui",
        estaSuperActivo = False
        } 
    | radio>3 = bolaEspinosa personaje {
        nombre = (nombre personaje) ++" Espina estuvo aqui"
        }
    |otherwise = bolaEspinosa personaje

torretaCurativa aliado = modificarVida (*2) aliado {
    estaSuperActivo = True
 }

atacarConPoderEspecial :: Personaje->Personaje->Personaje
atacarConPoderEspecial personaje contrincante 
    | estaSuperActivo personaje == True =
        (superPoder personaje).(poderBasico personaje) $ contrincante
    | otherwise = contrincante

brawlers :: [Personaje]
brawlers = [personajeEspina, personajePamela, personajeEnLasUltimas]

vidaComparada :: (Number->Bool)->Personaje->Bool
vidaComparada unaFuncion unPersonaje = (unaFuncion).vida $ unPersonaje 

quienEstaEnLasUltimas :: [Personaje] -> [String] 
quienEstaEnLasUltimas brawlers = map nombre . filter (vidaComparada (<800)) $ brawlers  

personajeEspina :: Personaje
personajePamela :: Personaje
personajeEnLasUltimas :: Personaje

personajeEspina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 4800  

personajeEnLasUltimas = UnPersonaje "Negro" bolaEspinosa torretaCurativa False 500

personajePamela = UnPersonaje "Pamela" (lluviaDeTuercas "Sanadoras") torretaCurativa False 9600
module Library where
import PdePreludat
import GHC.Num (Num)


--Definicion de tipos ----------------------------------------------------------------------------------------

type Nombre = String
type Autor = String
type CantidadDePaginas = Number
type Libro = (Nombre, Autor, CantidadDePaginas)


--Definicion de datos ----------------------------------------------------------------------------------------

capituloShingeki, capituloSandman :: Number -> Libro

capituloShingeki numero = ("Shingeki no Kyojin - Capitulo" ++ show numero, "Hajime Isayama", 40)
capituloSandman numero = ("Sandman - Capitulo "++ show numero, "Neil Gaiman", 35)

visitante :: Libro
visitante = ("El visitante", "Stephen King", 592)

shingeki1, shingeki3, shingeki127 :: Libro
shingeki1 = capituloShingeki 1
shingeki3 = capituloShingeki 3
shingeki127 = capituloShingeki 127

sagaShingeki :: [Libro]
sagaShingeki = [shingeki1,shingeki3,shingeki127]

sandman5, sandman10, sandman12 :: Libro

sandman5 = capituloSandman 5
sandman10 = capituloSandman 10
sandman12 = capituloSandman 12

fundacion :: Libro
fundacion = ("Fundacion","Isaac Asimov", 230)

eragon, eldest, brisignr, legado :: Libro
eragon = ("Eragon","Christopher Paolini",544)
eldest = ("Eldest","Christopher Paolini",704)
brisignr = ("Brisignr","Christopher Paolini",700)
legado = ("Legado","Christopher Paolini",811)

bibliotecaPdp :: [Libro]
bibliotecaPdp = [visitante, shingeki1, shingeki3, shingeki127, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]


cantidadDePaginasMostrar :: Libro -> Number
autorMostrar, nombreMostrar :: Libro -> String
cantidadDePaginasMostrar (_,_,paginas) = paginas
autorMostrar (_,nombreAutor,_) = nombreAutor
nombreMostrar (nombreLibro,_,_) = nombreLibro


--Funcion de promedio de hojas ----------------------------------------------------------------------------------------

promedioDeHojas :: [Libro] -> Number
promedioDeHojas biblioteca =  sum (map cantidadDePaginasMostrar biblioteca) / length biblioteca


--Funcion de lectura obligatoria ----------------------------------------------------------------------------------------

sagaEragon :: [Libro]
sagaEragon = [eragon, eldest, brisignr, legado]

esFundacion, esDeSagaEragon, esLecturaObligatoria :: Libro -> Bool
esDeAutor :: String -> Libro -> Bool
esFundacion libro = libro == fundacion
esDeSagaEragon libro = elem libro sagaEragon
esDeAutor nombreAutor libro = nombreAutor == autorMostrar libro 
esLecturaObligatoria libro = esFundacion libro || esDeSagaEragon libro || esDeAutor "Stephen King" libro


--Funcion de es fantasiosa ----------------------------------------------------------------------------------------

tieneAutor :: String -> [Libro] -> Bool
-- any Recibe una condición (una función que recibe un parámetro y devuelve Bool), una lista y retorna True si al menos un elemento de la lista cumple la condición
tieneAutor nombreAutor biblioteca =  any (esDeAutor nombreAutor) biblioteca


esFantasiosa :: [Libro] -> Bool
esFantasiosa unaBiblioteca = tieneAutor "Neil Gaiman" unaBiblioteca || tieneAutor "Christopher Paolini" unaBiblioteca 

--Funcion de nombre de la biblioteca ----------------------------------------------------------------------------------------

nombreDeLaBiblioteca :: [Libro] -> String
quitarVocales :: String -> String
esVocal, esVocalOEspacioOGuion :: Char -> Bool
esVocalOEspacioOGuion letra = esVocal letra || elem letra " -1234567890"
esVocal letra = elem letra "aeiouAEIOUáéíóúÁÉÍÓÚ"
quitarVocales palabra = filter (not.esVocalOEspacioOGuion) palabra 
nombreDeLaBiblioteca biblio = concat (map quitarVocales (map nombreMostrar biblio))

--Funcion de si es biblioteca ligera ----------------------------------------------------------------------------------------

esBibliotecaLigera::[Libro]->Bool
esBibliotecaLigera biblio = all (<=40) (map cantidadDePaginasMostrar biblio)


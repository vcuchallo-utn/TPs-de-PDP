module Library where
import PdePreludat


type Nombre = String
type Precio = Number
type Dia = String -- dd de mes de yyyy
type Producto = (Nombre, Precio)
type Entrega = (Producto, Dia)

productoLeche = ("Leche", 200)
productoElite = ("Parxeroloco", 1500)

nombreProducto :: Producto -> Nombre
precioProducto :: Producto -> Precio
nombreProducto (unNombre, _) = unNombre
precioProducto (_, unPrecio) = unPrecio

diaEntrega :: Entrega -> Dia
diaEntrega (_,unDia) = unDia

esXoY :: Char -> Bool
esVocal :: Char -> Bool
esXoY letra = elem letra "xzXZ"
esVocal letra = elem letra "aeiouAEIOUáéíóú"

esProductoDeLujo :: Producto -> Bool
esProductoCodiciado :: Producto -> Bool
esProductoCorriente :: Producto -> Bool
esProductoDeElite :: Producto -> Bool

esProductoDeLujo unProducto = any esXoY $ nombreProducto unProducto 
esProductoCodiciado unProducto = length (nombreProducto unProducto) > 10
esProductoCorriente unProducto = esVocal $ head (nombreProducto unProducto)
esProductoDeElite unProducto = esProductoDeLujo unProducto && esProductoCodiciado unProducto && not (esProductoCorriente unProducto)

aplicarCostoEnvio :: Number -> Number -> Number
aplicarDescuento :: Number -> Number -> Number  
precioTotal :: Number -> Number -> Number -> Number -> Number
aplicarCostoEnvio precio costoEnvio = precio + costoEnvio
aplicarDescuento precio descuento = precio - precio * descuento/100
precioTotal precioUnitario cantidad descuento costoEnvio = aplicarCostoEnvio ((aplicarDescuento precioUnitario descuento) * cantidad) costoEnvio

entregaSencilla :: Entrega -> Bool 
entregaSencilla unaEntrega = even $ length $ filter esVocal $ diaEntrega unaEntrega

descodiciarProducto :: Producto -> Producto 
descodiciarProducto unProducto = (take 10 $ nombreProducto unProducto, precioProducto unProducto)

productoXL :: Producto -> Producto
productoXL unProducto = (nombreProducto unProducto ++ "XL" , precioProducto unProducto)


{- Aca capaz hice mal, deberia quizar la funcion de descodiciarProducto aceptar un string y devolver 
string, o aceptar producto y devolver un string -}
invertirNombreProducto :: Producto -> Producto
invertirNombreProducto unProducto = (reverse $ nombreProducto unProducto, precioProducto unProducto)
versionBarata :: Producto -> Producto
versionBarata unProducto = invertirNombreProducto $ descodiciarProducto unProducto

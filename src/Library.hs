module Library where
import PdePreludat
import GHC.OldList (isInfixOf)

-- PARTE 1 - ¿Que es un alfajor?

data Alfajor = UnAlfajor {
    relleno :: [String],
    peso :: Number,
    dulzor :: Number,
    nombre :: String
} deriving (Show, Eq)

--data Relleno = UnRelleno {
--    cantidadCapas :: Number,
--    tipoRelleno :: TipoRelleno
--} deriving(Show, Eq)

--data TipoRelleno = DulceDeLeche | Mousse | Fruta deriving(Show, Eq)

-- a) Dadas estas características, queremos representar los siguientes alfajores:
-- i) Jorgito, que es un alfajor de una capa de dulce de leche, pesa 80g, tiene un dulzor de 8 y su nombre es “Jorgito”,
-- ii) Havanna, que es un alfajor de dos capas de mousse, pesa 60g, tiene un dulzor de 12 y su nombre es “Havanna”,
-- iii) Capitán del espacio, que es un alfajor de una capa de dulce de leche, pesa 40g, tiene un dulzor de 12 y su nombre es “Capitán del espacio”.

jorgito :: Alfajor
jorgito = UnAlfajor ["dulce de leche"] 80 8 "Jorgito"

havanna :: Alfajor
havanna = UnAlfajor ["mousse","mousse"] 60 12 "Havanna"

capitanDelEspacio :: Alfajor
capitanDelEspacio = UnAlfajor ["dulce de leche"] 40 12 "Capitan del espacio" 

-- b) También, queremos poder calcular las siguientes propiedades de un alfajor:

--i) el coeficiente de dulzor de un alfajor: indica cuánto dulzor por gramo tiene el alfajor; 
-- se calcula dividiendo su dulzor sobre su peso.

coeficienteDeDulzor :: Alfajor -> Number
coeficienteDeDulzor alfajor = dulzor alfajor / peso alfajor

-- ii) el precio de un alfajor: se calcula como el doble de su peso sumado a la sumatoria de los precios de sus rellenos. 
-- Una capa de relleno de dulce de leche cuesta $12; una de mousse, $15; una de fruta, $10.

precio :: Alfajor -> Number
precio alfajor = 2 * peso alfajor + precioRelleno alfajor 

precioRelleno :: Alfajor -> Number
precioRelleno = sum . map precioCapa. relleno 

precioCapa :: String -> Number
precioCapa capa 
    | capa == "dulce de leche" = 12
    | capa == "mousse"         = 15
    | capa == "fruta"          = 10
    | otherwise                = 1

-- iii) si un alfajor es potable: lo es si tiene al menos una capa de relleno (¿dónde se ha visto un alfajor sin relleno?),
-- todas sus capas son del mismo sabor, y su coeficiente de dulzor es mayor o igual que 0,1.

esPotable :: Alfajor -> Bool
esPotable alfajor = rellenoDelMismoSabor (relleno alfajor) && coeficienteDeDulzor alfajor >= 0.1

-- Este no es necesario porque lo implemente de forma dentro del rellenoDelMismoSabor
--tieneRelleno :: Alfajor -> Bool
--tieneRelleno = not . null . relleno

-- MODO RECURSIVO 
rellenoDelMismoSabor :: [String] -> Bool
rellenoDelMismoSabor [] = False -- el caso de que no tenga relleno
rellenoDelMismoSabor [x] = True
rellenoDelMismoSabor (x:y:ys) = x == y && rellenoDelMismoSabor (y:ys) 

-- MODO SIN RECURSIVIDAD (CON FUNCIONES YA HECHAS)
rellenoDelMismoSabor' :: [String] -> Bool
rellenoDelMismoSabor' [] = False -- el caso de que no tenga relleno
rellenoDelMismoSabor' relleno = all (== head relleno) relleno

-- PARTE 2 - escalabilidad vertical

-- Nuestro software debería poder reflejar las siguientes modificaciones:
-- a) abaratar un alfajor: reduce su peso en 10g y su dulzor en 7. 

abaratar :: Alfajor -> Alfajor
abaratar alfajor = alfajor {peso = peso alfajor - 10, dulzor = dulzor alfajor - 7}    

-- b) renombrar un alfajor, que cambia su packaging dándole un nombre completamente nuevo.

renombrar :: String -> Alfajor -> Alfajor
renombrar nuevoNombre alfajor = alfajor {nombre = nuevoNombre}

-- c) agregar una capa de cierto relleno a un alfajor.

agregarCapa :: String -> Alfajor -> Alfajor
agregarCapa nuevaCapa alfajor = alfajor {relleno = nuevaCapa : relleno alfajor}

-- d) hacer premium a un alfajor: dado un alfajor, le agrega una capa de relleno (del mismo tipo de relleno que ya tiene), 
-- y lo renombra a su nombre original + la palabra "premium" al final. 
-- Sólo los alfajores potables pueden hacerse premium; si se intenta hacer premium un alfajor no potable, 
-- el alfajor queda exactamente igual que como estaba.

-- Ejemplo: dado un alfajor Havanna, hacerlo premium lo convertiría en un alfajor con tres capas de mousse 
-- llamado “Havanna premium”.

intentarHacerPremium :: Alfajor -> Alfajor
intentarHacerPremium alfajor 
    | esPotable alfajor = hacerPremium alfajor
    | otherwise         = alfajor

hacerPremium :: Alfajor -> Alfajor
hacerPremium alfajor = (renombrar (nombre alfajor ++ " " ++ "premium") . flip agregarCapa alfajor . head . relleno) alfajor

hacerPremium' :: Alfajor -> Alfajor
hacerPremium' = nombrePremium . agregarCapaPremiun

nombrePremium :: Alfajor -> Alfajor 
nombrePremium alfajor = renombrar (nombre alfajor ++ " " ++ "premium") alfajor

agregarCapaPremiun :: Alfajor -> Alfajor 
agregarCapaPremiun alfajor = (flip agregarCapa alfajor . head . relleno) alfajor 

-- e) hacer premium de cierto grado a un alfajor: consiste en hacerlo premium varias veces. 
-- Este punto puede ser resuelto usando recursividad.

type Modificacion = Alfajor -> Alfajor

aplicarNVeces :: Number -> Modificacion -> Alfajor -> Alfajor
aplicarNVeces grado modificacion alfajor 
    | grado > 0 = aplicarNVeces (grado - 1) modificacion (modificacion alfajor) -- para que se vaya disminuyendo la cantidad (caso recursivo)
    | otherwise = alfajor -- (caso base)

aplicarNIntentarPremium :: Number -> Alfajor -> Alfajor
aplicarNIntentarPremium numero = aplicarNVeces numero intentarHacerPremium 

-- > aplicarNIntentarPremiun 2 intentarPremiun havanna

-- f) Modelar los siguientes alfajores:
-- i) Jorgitito, que es un Jorgito abaratado, y cuyo nombre es “Jorgitito”.

jorgitito :: Alfajor
jorgitito = renombrar "Jorgitito" (abaratar jorgito)

jorgitito' :: Alfajor
jorgitito' = (renombrar "Jorgitito" . abaratar) jorgito

-- ii) Jorgelín, que es un Jorgito pero con una capa extra de dulce de leche, y cuyo nombre es “Jorgelín”.

jorgelin :: Alfajor
jorgelin = (renombrar "Jorgelin" . agregarCapa "dulce de leche") jorgito

-- iii) Capitán del espacio de costa a costa: es un capitán del espacio abaratado, luego hecho premium de grado 4, 
--y luego renombrado a “Capitán del espacio de costa a costa”

capitanDelEspacioDeCostaACosta :: Alfajor
capitanDelEspacioDeCostaACosta = (renombrar "Capitan del espacio de costa a costa" . aplicarNIntentarPremium 4 . abaratar) capitanDelEspacio 

-- PARTE 3 - clientes del kiosco

-- Queremos también representar a los clientes de nuestro kiosco y registrar cuánto dinero tienen para gastar y 
-- qué alfajores nos compraron.
-- Sobre gustos no hay nada escrito. Cada cliente tiene diferentes criterios respecto a los alfajores. 
-- A un cliente le gusta un alfajor si cumple con todos sus criterios.

-- a) Queremos modelar los siguientes clientes (todos comienzan sin alfajores comprados):

data Cliente = UnCliente {
    nombreCliente :: String,
    dinero :: Number,
    alfajoresComprados :: [Alfajor],
    criterios :: [Criterio]
} deriving(Show, Eq)

type Criterio = Alfajor -> Bool

-- i) Emi: tiene $120; es busca marca de los capitanes del espacio, lo que significa que solo le 
-- gustan alfajores que contengan en su nombre “Capitán del espacio”.

emi :: Cliente
emi = UnCliente "Emi" 120 [] [soloMarca "Capitan del espacio"] 

--buscaCapitanesDelEspacio :: Criterio
--buscaCapitanesDelEspacio = soloMarca "Capitan del espacio"  

soloMarca :: String -> Alfajor -> Bool
soloMarca marca alfajor = contieneSubCadena' marca (nombre alfajor)

-- Ojo solo funcionar con las alfajores que comienzan con el nombre de la marca por ejemplo "Havanna" de "Havanna triple chocolate"
contieneSubCadena :: String -> String -> Bool
contieneSubCadena subcadena cadena 
    | length subcadena > length cadena            = False
    | take (length subcadena) cadena == subcadena = True
    | otherwise                                   = False

-- Este si funciona bien!!
contieneSubCadena' :: String -> String -> Bool
contieneSubCadena' = isInfixOf 

-- ii) Tomi: tiene $100; es pretencioso (solo le gustan los alfajores que contienen “premium” en su nombre*), 
-- y dulcero (le gustan los alfajores cuyo coeficiente de dulzor es mayor a 0,15).

tomi :: Cliente 
tomi = UnCliente "Tomi" 100 [] [pretencioso, dulcero]

pretencioso :: Criterio
pretencioso alfajor = contieneSubCadena' "premium" (nombre alfajor)

dulcero :: Criterio
dulcero = (> 0.15) . coeficienteDeDulzor

-- Ejemplo: no le gusta el Jorgelín premium porque a pesar de ser premium, su índice de dulzor es menor a 0,15, 
-- así que sólo cumple 1 de sus criterios, no todos. En cambio, el Havanna premium sí cumple ambos, así que ese alfajor le gusta.
-- *ojo: premium no es una marca

-- iii) Dante: tiene $200; es anti-dulce de leche, por lo que los alfajores que le gustan no deben tener 
-- ninguna capa de dulce de leche, y además es extraño, lo que significa que solo le gustan alfajores que no son potables.

dante :: Cliente
dante = UnCliente "Dante" 200 [] [antiAlgo "dulce de leche", extranio]

antiAlgo :: String -> Criterio
antiAlgo algo = notElem algo . relleno

--antiDulceDeLeche :: Criterio
--antiDulceDeLeche = notElem "dulce de leche" . relleno

extranio :: Criterio
extranio = not . esPotable

-- iv) Juan: tiene $500; es dulcero, busca marca de Jorgito, pretencioso y anti-mousse

juan :: Cliente 
juan = UnCliente "Juan" 500 [] [dulcero, soloMarca "Jorgito", pretencioso, antiAlgo "mousse"]

--buscaJorgitos :: Criterio
--buscaJorgitos = soloMarca "Jorgito"

-- b) indicar, dada una lista de alfajores, cuáles le gustan a cierto cliente.
-- RECORDAR: A un cliente le gusta un alfajor si cumple con todos sus criterios.

leGustan :: Cliente -> [Alfajor] -> [Alfajor]
leGustan cliente = filter (leGusta cliente)

leGusta :: Cliente -> Alfajor -> Bool
leGusta cliente alfajor = all (\criterio -> criterio alfajor) (criterios cliente)
-- 1ero. Me meto a los criterios de los clientes 
-- 2dos. Hago un todos cumplen (para cada criterio), el alfajor cumple con cada uno de los criterios del cliente

-- c) que un cliente pueda comprar un alfajor: esto lo agrega a su lista de alfajores actuales, y gasta el dinero 
-- correspondiente al precio del alfajor. Si no tiene suficiente plata, no lo compra y queda como está.

intentarComprar :: Alfajor -> Cliente -> Cliente
intentarComprar alfajor cliente 
    | puedeComprarlo cliente alfajor = comprar alfajor cliente
    | otherwise                      = cliente

puedeComprarlo :: Cliente -> Alfajor -> Bool
puedeComprarlo cliente = (< dinero cliente) . precio  

comprar :: Alfajor -> Cliente -> Cliente
comprar alfajor = gastarDinero (precio alfajor) . agregarAlfajor alfajor

agregarAlfajor :: Alfajor -> Cliente -> Cliente
agregarAlfajor alfajor cliente = cliente {alfajoresComprados = alfajor : alfajoresComprados cliente}

gastarDinero :: Number -> Cliente -> Cliente
gastarDinero dineroGastado cliente = cliente {dinero = dinero cliente - dineroGastado}

-- d) que un cliente compre, de una lista de alfajores, todos aquellos que le gustan.

comprarLosQueLeGustan :: Cliente -> [Alfajor]  -> Cliente
comprarLosQueLeGustan cliente = foldr comprar cliente . leGustan cliente
-- 1ero. Filtro los alfajores que le gustan
-- 2dos. Compro, realiza una reduccion, compro todos los alfajores que le gustan impactando en el mismo cliente una y otra vez 
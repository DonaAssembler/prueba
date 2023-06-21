module Library where
import PdePreludat

data Filmacion = Filmacion{
    titulo :: String,
    puntaje :: Number,
    anioDeFilmacion :: Number,
    duracion :: Number,
    actores :: [String],
    genero :: Genero
} deriving(Show)

nuevereinas = Filmacion{
    titulo = "9 reinas",
    puntaje = 8,
    anioDeFilmacion = 2000,
    duracion = 114,
    actores = ["Gaston Pauls", "Ricardo Darin", "Leticia Bredice", "Pochi Ducasse"],
    genero = generoDrama 5
}

armamortal = Filmacion{
    titulo = "Arma Mortal",
    puntaje = 7,
    anioDeFilmacion = 1987,
    duracion = 109,
    actores = ["Mel Gibson", "Danny Glover", "Gary Busey"],
    genero = generoAccion
}

odiseagiles = Filmacion{
    titulo = "La Odisea de los Giles",
    puntaje = 8,
    anioDeFilmacion = 2019,
    duracion = 116,
    actores = ["Ricardo Darin", "Luis Brandoni", "Veronica Llinas", "Daniel Araoz", "Rita Cortese"],
    genero = generoComedia
}

laflor = Filmacion{
    titulo = "La flor",
    puntaje = 7,
    anioDeFilmacion = 2018,
    duracion = 840,
    actores = ["Pilar Gamboa"],
    genero = generoTragicomico
}

speed = Filmacion{
    titulo = "Speed",
    puntaje = 7,
    anioDeFilmacion = 1994,
    duracion = 116,
    actores = ["Keanu Reeves", "Sandra Bullock", "Dennis Hopper","Jeff Daniels", "Alan Ruck"],
    genero = generoAccion
}

indiana = Filmacion{
    titulo = "Indiana Jones",
    puntaje = 8,
    anioDeFilmacion = 1981,
    duracion = 115,
    actores = ["Harrison Ford"],
    genero = generoAventura "IV"
}

indianacuatro = Filmacion{
    titulo = "Indiana Jones IV",
    puntaje = 6,
    anioDeFilmacion = 2007,
    duracion = 125,
    actores = ["Harrison Ford"],
    genero = generoAventura "IV"
}

secretodesusojos = Filmacion{
    titulo = "El Secreto de sus Ojos",
    puntaje = 9,
    anioDeFilmacion = 2009,
    duracion = 129,
    actores = ["Ricardo Darin","Soledad Villamil"],
    genero = generoDrama 3
}

-- Punto 1, Pelicula Darinesca (Lara Galvan)

peliDarinesca :: Filmacion -> Bool 
peliDarinesca =  (== "Ricardo Darin").head.actores 


-- Punto 1, Pinta Buena (Juan Molina)

cantActores :: Number
cantActores = 5

pintaBuena :: Filmacion -> Bool
pintaBuena = ( >= cantActores).length.actores

-- Punto 1, Minutos Excedentes (Camila Fernández)

minutosTope :: Number
minutosTope = 115

minutosExcedentes :: Filmacion -> Number
minutosExcedentes = abs .(-) minutosTope . duracion


-- Punto 2, Precio Base (Lara Galvan)

topeActores :: Number
topeActores = 4

anioTope :: Number
anioTope = 1990

--pintaGrosa :: Filmacion -> Bool
--pintaGrosa = (>topeActores).length.actores 

esVieja :: Filmacion -> Bool
esVieja = (<anioTope).anioDeFilmacion 

precioBase :: Filmacion -> Number
precioBase filmacion 
    | pintaBuena filmacion = 200
    | esVieja filmacion = ((*2).length.titulo)filmacion        --2 * length (titulo pelicula)
    | otherwise = ((+100).(*3).puntaje) filmacion


-- Punto 2, Precio Extra (Juan Molina)

esLarga :: Filmacion -> Bool
esLarga = (>minutosTope).duracion


precioExtra :: Filmacion -> Number
precioExtra filmacion 
    | esLarga filmacion = (min 100.(10*).minutosExcedentes) filmacion
    | (not . esVieja) filmacion = 50
    | otherwise = 0

-- Punto 2, Precio Total (Camila Fernández)

sumatoriaDePrecio :: Filmacion -> Number
sumatoriaDePrecio filmacion = precioBase filmacion + precioExtra filmacion

precioTotal :: Filmacion -> Number
precioTotal filmacion
    | sumatoriaDePrecio filmacion > 200 = sumatoriaDePrecio filmacion * 0.9
    | otherwise = sumatoriaDePrecio filmacion

-- Punto 3, Introduccion (Camila Fernandez - Lara Galvan)
data Persona = Persona {
    nombre :: String,
    satisfaccion :: Number,
    edad :: Number,
    cantPelisVistas :: Number,
    plataDisponible :: Number
}deriving(Show)

joaquin = Persona {
    nombre = "Joaquin",
    satisfaccion = 5,
    edad = 22,
    cantPelisVistas = 3,
    plataDisponible = 1500
}

maria = Persona {
    nombre = "Maria",
    satisfaccion = 20,
    edad = 19,
    cantPelisVistas = 5,
    plataDisponible = 10
}

pepe = Persona {
    nombre = "Pepe",
    satisfaccion = 20,
    edad = 30,
    cantPelisVistas = 3,
    plataDisponible = 1500
}

pedro = Persona {
    nombre = "Pedro",
    satisfaccion = 10,
    edad = 30,
    cantPelisVistas = 15,
    plataDisponible = 2300
}

moni = Persona{
    nombre = "Moni",
    satisfaccion = 50,
    edad = 31,
    cantPelisVistas = 1,
    plataDisponible = 5600
}

coky = Persona{
    nombre = "Coky",
    satisfaccion = 120,
    edad = 20,
    cantPelisVistas = 40,
    plataDisponible = 50
}

type Genero = Filmacion -> Persona -> Persona

efectoComun :: Genero
efectoComun filmacion persona = persona{
    cantPelisVistas = cantPelisVistas persona + 1,
    plataDisponible =  max (plataDisponible persona - precioTotal filmacion) 0
} 

-- Punto 3 Genero Terror y Comedia (Lara Galvan)

generoTerror :: Number -> Genero
generoTerror litrosSangre filmacion unaPersona = efectoComun filmacion unaPersona{satisfaccion = satisfaccion unaPersona - litrosSangre}

generoComedia :: Genero
generoComedia filmacion unaPersona = efectoComun filmacion unaPersona{satisfaccion = 2*satisfaccion unaPersona, nombre = nombre unaPersona ++ " muy alegre"}

-- Punto 3, Genero Drama y Accion (Juan Molina)

generoDrama :: Number -> Genero
generoDrama escenasFelices filmacion unaPersona = efectoComun filmacion unaPersona{
    edad = edad unaPersona + 1,
    satisfaccion = satisfaccion unaPersona + min escenasFelices 3
    }

generoAccion :: Genero
generoAccion filmacion unaPersona  
    | pintaBuena filmacion  = efectoComun filmacion unaPersona {satisfaccion = satisfaccion unaPersona + 100}
    | otherwise = efectoComun filmacion unaPersona


-- Punto 3, Genero Tragicómico y Aventura (Camila Fernández)
generoTragicomico :: Genero
generoTragicomico filmacion = generoDrama 4 filmacion. generoComedia filmacion


generoAventura :: String -> Genero
generoAventura versionPeliculaMala filmacion unaPersona
    | ((== versionPeliculaMala).last.words.titulo) filmacion = unaPersona
    | otherwise = generoComedia filmacion unaPersona 

{- otra forma de pensarlo
generoAventuraBis :: Persona -> Filmacion -> String -> Persona
generoAventuraBis unaPersona filmacion1 tituloPeliculaMala
    | titulo filmacion1 == tituloPeliculaMala = unaPersona -- es mala
    | otherwise = generoComedia unaPersona -- es buena -}


-- ----------------------------------  ENTREGA 2  ----------------------------------
-- Punto 4 Luz, Camara... (Todos los integrantes)
verFilmacion :: Genero
verFilmacion filmacion  = genero filmacion filmacion 

verFilmaciones :: Persona -> [Filmacion] -> Persona
verFilmaciones = foldr verFilmacion

-- Punto 4 Never Pony (Lara Galván)

neverPony :: [Persona] -> Bool
neverPony televidentes = not(null(filter ((> 100) . satisfaccion) televidentes))

televidentes :: Filmacion -> [Persona] -> [Persona]
televidentes personas = map (verFilmacion personas)

-- Punto 4 Deme dos (Camila Fernandez)

lasDosPrimerasDarinescas :: [Filmacion] -> [String]
lasDosPrimerasDarinescas filmaciones = take 2 $ map titulo $ filter peliDarinesca filmaciones

-- Punto 4, Combo Vendible (Juan Molina) a terminar

comboVendible :: [Filmacion] -> [Persona] -> Bool
comboVendible filmaciones personas = all (verificarCompra filmaciones) personas

verificarCompra :: [Filmacion] -> Persona -> Bool
verificarCompra filmaciones persona = any (puedePagarFilmacion persona) filmaciones

-- Punto 5 Hasta donde de la billetera (Lara Galván)

hastaDondeDeLaBilletera :: Persona -> [Filmacion] -> Persona
hastaDondeDeLaBilletera unaPersona [] = unaPersona
hastaDondeDeLaBilletera unaPersona (filmacion:filmaciones) 
    | puedePagarFilmacion unaPersona filmacion = hastaDondeDeLaBilletera (verFilmacion filmacion unaPersona) filmaciones
    | otherwise = hastaDondeDeLaBilletera unaPersona filmaciones


puedePagarFilmacion :: Persona -> Filmacion -> Bool
puedePagarFilmacion unaPersona filmacion = plataDisponible unaPersona >= precioTotal filmacion

-- Punto 5, I can´t get no… (Juan Molina)

iCantGetNo :: Filmacion -> [Persona] -> Number -> [Persona]
iCantGetNo _ [] _ = []
iCantGetNo filmacion (persona:personas) satisfaccionMinima
    | satisfaccion (verFilmacion filmacion persona) > satisfaccionMinima = verFilmacion filmacion persona : personas
    | otherwise = persona : iCantGetNo filmacion personas satisfaccionMinima

-- Punto 5 "Reee manija" (Camila Fernandez)
reManija :: Persona -> [Filmacion] -> Bool
reManija persona [] = error "No matching"
reManija persona [filmacion] = True
reManija persona (filmacion1:filmacion2:filmaciones)
    | pareceManija persona [filmacion1,filmacion2] = reManija persona (filmacion2:filmaciones)
    | otherwise = False

pareceManija :: Persona -> [Filmacion] -> Bool
pareceManija unaPersona [filmacion1,filmacion2] = satisfaccion (verFilmacion filmacion1 unaPersona) < satisfaccion (verFilmacion filmacion2 unaPersona) 

-- Punto 6

meQuedoConLosPrimeros :: (a -> Bool) -> [a] -> [a]
meQuedoConLosPrimeros _ [] = []
meQuedoConLosPrimeros criterio (x:xs)
    |  not (criterio x) = []
    | otherwise = x : meQuedoConLosPrimeros criterio xs

-- Punto 6 La que le gusta a los Stones (Lara Galván)
laPulenta :: Persona -> Number -> Filmacion -> Bool
laPulenta unaPersona nivelSatisfaccion filmacion  = ((>nivelSatisfaccion).satisfaccion.verFilmacion filmacion) unaPersona

{-
Ejemplo de implementación en meQuedoConLosPrimeros
ghci > meQuedoConLosPrimeros (laPulenta coky 200) [nuevereinas,speed,indiana]
-}


-- Punto 6, Caprichito (Juan Molina)

tengoCaprichitoCon :: Filmacion -> [Number] -> Bool
tengoCaprichitoCon filmacion  = (elem . puntaje) filmacion

{-
Ejemplo de implementación en meQuedoConLosPrimeros
ghci > meQuedoConLosPrimeros (flip tengoCaprichitoCon [6,7,8]) [odiseagiles,speed]
-}

-- Punto 6, Show me The Money (Camila Fernandez)

tienenLaTeca :: Persona -> (Filmacion -> Bool)
tienenLaTeca = puedePagarFilmacion 

{-
Ejemplo de implementación en meQuedoConLosPrimeros
ghci > meQuedoConLosPrimeros (tienenLaTeca coky) [odiseagiles,nuevereinas,speed,indiana]
-}


{- Punto 7 Evaluación (todos los integrantes)

Defina un contingente infinito de filmaciones. ¿Qué ocurre si invocamos lasDosPrimerasDarinescas  o comboVendible con una lista infinita de Filmaciones? Justifique su respuesta.

filmacionesInfinitas :: [Filmacion]
filmacionesInfinitas = cycle(Filmacion "Pelicula 1" 7 2022 120 ["Ricardo Darin", "Actor 2"] Accion,
    Filmacion "Pelicula 2" 8 2019 110 ["Ricardo Darin", "Actor 4"] Comedia,
    Filmacion "Pelicula 3" 6 2021 105 ["Actor 5", "Actor 6"] Drama)

Respuesta: Si invocamos la funcion lasDosPrimerasDarinescas con la lista infinita de filmaciones funciona. Ya que gracias a la evaluación diferida que utiliza Haskell, al utilizar take 2 en la función agarra las 2 primeras peliculas y las devuelve. De esta manera no tiene que que seguir evaluando la lista infinita.
-}

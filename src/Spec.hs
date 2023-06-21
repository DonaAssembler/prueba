module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do

-- Test Punto 1, Pelicula Darinesca (Lara Galvan)

    describe "Pelicula Darinesca" $ do
        it "pelicula encabezada por Ricardo Darin es Darinesca" $ do
            odiseagiles `shouldSatisfy` peliDarinesca
        it "pelicula con Darin pero encabezada por otro actor no es Darinesca" $ do
            nuevereinas `shouldNotSatisfy` peliDarinesca
        it "pelicula encabezada por otro actor no es Darinesca" $ do
            peliDarinesca armamortal `shouldBe` False

-- Test Punto 1, Pinta Buena (Juan Molina)

    describe "Pinta Buena" $ do 
        it "La pelicula pinta buena" $ do
            odiseagiles `shouldSatisfy` pintaBuena
        it "La pelicula no pinta buena" $ do
            armamortal `shouldNotSatisfy` pintaBuena

-- Test Punto 1, Minutos Excedentes (Camila Fernández)

    describe "Minutos Excedentes" $ do 
        it "La pelicula tiene minutos excedentes" $ do
            minutosExcedentes odiseagiles `shouldBe` 1
        it "La pelicula tiene minutos excedentes" $ do
            minutosExcedentes armamortal `shouldBe` 6



-- Test Punto 2, Precio Base (Lara Galvan)

    describe "Test Precio Base" $ do
        it "Precio base para pelicula que pinta grosa" $ do
            precioBase odiseagiles `shouldBe` 200
        it "Precio base para pelicula vieja" $ do
            precioBase armamortal `shouldBe` 22
        it "Precio base para pelicula que no pinta grosa y no es vieja" $ do
            precioBase nuevereinas `shouldBe` 124


--Test Punto 2, Precio Extra (Juan Molina)

    describe "Precio Extra" $ do
        it "La pelicula tiene un precio extra por excederse de tiempo" $ do
            precioExtra odiseagiles `shouldBe` 10
        it "La pelicula tiene un precio extra por excederse de tiempo" $ do
            precioExtra laflor `shouldBe` 100
        it "La pelicula tiene un precio extra por no ser vieja" $ do
            precioExtra nuevereinas `shouldBe` 50
        it "La pelicula no tiene precio extra" $ do
            precioExtra armamortal `shouldBe` 0


--Test Punto 2, Precio Total (Camila Fernandez)

    describe "Precio Total" $ do 
        it "La pelicula tiene un descuento por tener un precio base mayor a 200" $ do
            precioTotal odiseagiles `shouldBe` 189
        it "La pelicula tiene un precio sin descuento" $ do
            precioTotal armamortal `shouldBe` 22


--Test Punto 3 Introduccion 

    describe "Test Persona" $ do
        it "Persona con filmaciones vistas, ve algun genero" $ do
            cantPelisVistas(efectoComun armamortal joaquin) `shouldBe` 4
        it "Se descuenta precio total de una pelicula a una persona con saldo disponible" $ do
            plataDisponible(efectoComun odiseagiles joaquin) `shouldBe` 1311
        it "Se le descuenta precio total de una pelicula a una persona hasta un tope minimo" $ do
            plataDisponible(efectoComun odiseagiles maria) `shouldBe` 0

-- Test Punto 3 Genero Terror y Comedia (Lara Galvan)
    describe "Test Genero Terror y Comedia" $ do
        it "Se decrementa nivel de satisfaccion de una persona al aplicarse genero terror" $ do
            satisfaccion(generoTerror 5 odiseagiles maria) `shouldBe` 15
        it "Se aumenta nivel de satisfaccion de una persona al aplicarse genero comedia" $ do
            satisfaccion(generoComedia nuevereinas maria) `shouldBe` 40
        it "Se modifica el nombre de una persona al aplicarse una comedia" $ do
            nombre(generoComedia nuevereinas pepe) `shouldBe` "Pepe muy alegre"

-- Test Punto 3 Genero Drama y Accion (Juan Molina)
    describe "Test Genero Drama y Accion" $ do
        it "Se aumenta la edad de una persona al aplicarse genero drama" $ do
            edad(generoDrama 2 odiseagiles pedro) `shouldBe` 31
        it "Se modifica el nivel de satisfaccion de una persona al aplicarse genero drama" $ do
            satisfaccion(generoDrama 2 odiseagiles maria) `shouldBe` 22
        it "Se modifica el nivel de satisfaccion de una persona al aplicarse genero drama" $ do
            satisfaccion(generoDrama 5 odiseagiles maria) `shouldBe` 23
        it "Se modifica el nivel de satisfaccion de una persona al aplicarse genero Accion" $ do
            satisfaccion(generoAccion speed maria) `shouldBe` 120
        it "Se mantiene el nivel de satisfaccion de una persona al aplicarse genero Accion" $ do 
            satisfaccion(generoAccion armamortal maria) `shouldBe` 20

-- Test Punto 3 Genero Tragicomico y Aventura (Camila Fernández)
    describe "Test Genero Tragicomico y Aventura" $ do
        it "Se aumenta la edad de una persona al aplicarse genero Tragicomico" $ do
            edad(generoTragicomico odiseagiles pedro) `shouldBe` 31
        it "Se modifica nivel de satisfaccion de una persona al aplicarse genero Tragicomico" $ do
            satisfaccion(generoTragicomico odiseagiles pepe) `shouldBe` 43
        it "Se agrega ' muy alegre' al nombre de una persona al aplicarse genero Tragicomico" $ do
            nombre(generoTragicomico odiseagiles pepe) `shouldBe` "Pepe muy alegre"
        it "Se mantiene el nivel de satisfaccion de una persona al aplicarse genero Aventura, ya que opina que indianajones IV es la mala de la saga" $ do
            satisfaccion(generoAventura "IV" indianacuatro maria) `shouldBe` 20
        it "Se aumenta el nivel de satisfaccion de una persona al aplicarse genero Aventura, ya que opina que indianajones I es la buena de la saga" $ do
            satisfaccion(generoAventura "IV" indiana maria) `shouldBe` 40


-- Tests Punto 4 "Luz, Camara ..." 
    describe "Test VerFilmacion y verFilmaciones" $ do
        it "Una persona ve una comedia y se le descuenta lo correspondiente" $ do
            plataDisponible(verFilmacion odiseagiles pepe) `shouldBe` 1311
        it "Una persona ve una comedia y se modifica la cantidad de peliculas vistas" $ do
            cantPelisVistas(verFilmacion odiseagiles pepe) `shouldBe` 4
        it "Una persona ve una comedia y se le incrementa el nivel de satisfaccion" $ do
            satisfaccion(verFilmacion odiseagiles pepe) `shouldBe` 40
        it "Una persona ve una serie de filmaciones y se le descuenta el dinero correspondiente" $ do
            plataDisponible(verFilmaciones pepe [odiseagiles,speed]) `shouldBe` 1122
        it "Una persona ve una serie de filmaciones y se modifica la cantidad de peliculas vistas" $ do
            cantPelisVistas(verFilmaciones pepe [odiseagiles,speed]) `shouldBe` 5
        it "Una persona ve una serie de filmaciones y se modifica su nivel de satisfaccion" $ do
            satisfaccion(verFilmaciones pepe [speed,odiseagiles]) `shouldBe` 140        

-- Test Punto 4 "Never Pony" (Lara Galvan)
    describe "Test Never Pony" $ do
        it "Never Pony con televidentes que ven una filmacion motivadora" $ do
            neverPony (televidentes speed [pepe,moni]) `shouldBe` True
        it "Never Pony con televidentes que ven una filmacion no motivadora" $ do
            neverPony (televidentes armamortal [pepe,moni]) `shouldBe` False

-- Test Punto 4 "Combo Vendible" (Juan Molina)
    describe "Test Combo Vendible" $ do
        it "Combo vendible de las filmaciones speed y nueve reinas para Pepe y Moni" $ do 
            comboVendible [speed,nuevereinas] [pepe,moni] `shouldBe` True
        it "Combo vendible de las filmaciones speed y nueve reinas para Pepe y Moni" $ do 
            comboVendible [speed,nuevereinas] [pepe,moni,coky] `shouldBe` False

-- Test Punto 4 "¡Deme dos!" (Camila Fernandez)
    describe "Test ¡Deme dos!" $ do
        it "¡Deme dos! con dos películas darinescas" $ do
            lasDosPrimerasDarinescas [nuevereinas,odiseagiles,secretodesusojos] `shouldBe` ["La Odisea de los Giles","El Secreto de sus Ojos"]
        it "¡Deme dos! con una película darinesca" $ do
            lasDosPrimerasDarinescas [nuevereinas,secretodesusojos] `shouldBe` ["El Secreto de sus Ojos"]
        it "¡Deme dos! sin películas darinescas" $ do
            lasDosPrimerasDarinescas [nuevereinas,laflor] `shouldBe` []

-- Test Punto 5 "Dame mas info" (Lara Galvan)
    describe "Test Hasta donde de la billetera" $ do
        it "Persona ve las peliculas de una serie de filmaciones que puede abonar" $ do
            plataDisponible(hastaDondeDeLaBilletera coky [indiana,laflor,armamortal,speed]) `shouldBe` 2
        it "Persona no ve las filmaciones ya que no puede abonarlas" $ do
            plataDisponible(hastaDondeDeLaBilletera coky [nuevereinas,laflor]) `shouldBe` 50

-- Test Punto 5 "I Cant Get No.." (Juan Molina)
    describe "Test iCantGetNo" $ do
        it "I cant get no para 'La Odisea de los giles' con el grupo de televidentes Moni y Coky con un nivel de 150 de satisfacción" $ do
            (cantPelisVistas . (!! 1)) (iCantGetNo odiseagiles [moni,coky] 150) `shouldBe` 41
        it "Satisfacción de Coky" $ do
            (satisfaccion . (!! 1)) (iCantGetNo odiseagiles [moni,coky] 150) `shouldBe` 240
        it "Cantidad de peliculas vistas por Moni" $ do 
            (cantPelisVistas . (!! 0)) (iCantGetNo odiseagiles [moni,coky] 150) `shouldBe` 1

-- Test Punto 5 "Reeeee manija" (Camila Fernandez)
    describe "Test Reeeee manija" $ do
        it "Coky con las filmaciones Arma Mortal, 9 Reinas y Speed." $ do
            reManija coky [armamortal,nuevereinas,speed] `shouldBe` True
        it "Coky con las filmaciones 9 Reinas Arma Mortal y Speed." $ do
            reManija coky [nuevereinas,armamortal,speed] `shouldBe` False
        -- it "Coky sin filmaciones." $ do
        --     reManija coky [] `shouldThrow` ExitFailure 1

-- Test Punto 6 "Me quedo con los primeros" 
    describe "Test meQuedoConLosPrimeros" $ do
        it "Me quedo con los primeros menores a 3 de una lista que va del 1 al 100" $ do
            meQuedoConLosPrimeros (<3) [1..100] `shouldBe` [1,2]
        it "Me quedo con los primeros menores a 3 de una lista de números" $ do
            meQuedoConLosPrimeros (<3) [5,1,2,3] `shouldBe` []

--Test Punto 6 "La Pulenta" (Lara Galván)
    describe "La que le gusta a los Stones" $ do
        it "Persona que supera cierto nivel de satisfaccion al mirar una pelicula" $ do
            laPulenta coky 200 odiseagiles `shouldBe` True
        it "Persona que no supera cierto nivel de satisfaccion al ver una pelicula" $ do
            laPulenta coky 300 odiseagiles `shouldBe` False

-- Test Punto 6, "Caprichito" (Juan Molina)
    describe "Test tencgoCaprichito con" $ do
        it "Tengo caprichito con una serie de calificaciones para una filmacion" $ do
            tengoCaprichitoCon odiseagiles [6,8,9] `shouldBe` True
        it "Tengo caprichito con unas serie de calificaciones para una filmacion" $ do
            tengoCaprichitoCon odiseagiles [6,7,9] `shouldBe` False

-- Test Punto 6.c "Show me the money" (Camila Fernandez)
    describe "Test Show me the money" $ do
        it "Moni con la filmación 'La odisea de los giles'" $ do
            tienenLaTeca moni odiseagiles `shouldBe` True
        it "Coky con la filmación 'La odisea de los giles'" $ do
            tienenLaTeca coky odiseagiles  `shouldBe` False

    
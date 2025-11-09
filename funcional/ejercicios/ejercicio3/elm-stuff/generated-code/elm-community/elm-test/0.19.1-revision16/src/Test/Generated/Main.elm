module Test.Generated.Main exposing (main)

import Clase3Test

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 178240767707548
        , processes = 8
        , globs =
            []
        , paths =
            [ "C:\\Users\\Samu\\Documents\\2do Año\\Paradigmas de Programación\\paradigmaUAP2025\\funcional\\ejercicios\\ejercicio3\\tests\\Clase3Test.elm"
            ]
        }
        [ ( "Clase3Test"
          , [ Test.Runner.Node.check Clase3Test.testsMiMap
            , Test.Runner.Node.check Clase3Test.testsMiFiltro
            , Test.Runner.Node.check Clase3Test.testsMiFoldl
            , Test.Runner.Node.check Clase3Test.testsDuplicar
            , Test.Runner.Node.check Clase3Test.testsLongitudes
            , Test.Runner.Node.check Clase3Test.testsIncrementarTodos
            , Test.Runner.Node.check Clase3Test.testsTodasMayusculas
            , Test.Runner.Node.check Clase3Test.testsNegarTodos
            , Test.Runner.Node.check Clase3Test.testsPares
            , Test.Runner.Node.check Clase3Test.testsPositivos
            , Test.Runner.Node.check Clase3Test.testsStringsLargos
            , Test.Runner.Node.check Clase3Test.testsSoloVerdaderos
            , Test.Runner.Node.check Clase3Test.testsMayoresQue
            , Test.Runner.Node.check Clase3Test.testsSumaFold
            , Test.Runner.Node.check Clase3Test.testsProducto
            , Test.Runner.Node.check Clase3Test.testsContarFold
            , Test.Runner.Node.check Clase3Test.testsConcatenar
            , Test.Runner.Node.check Clase3Test.testsMaximo
            , Test.Runner.Node.check Clase3Test.testsInvertirFold
            , Test.Runner.Node.check Clase3Test.testsTodos
            , Test.Runner.Node.check Clase3Test.testsAlguno
            , Test.Runner.Node.check Clase3Test.testsSumaDeCuadrados
            , Test.Runner.Node.check Clase3Test.testsContarPares
            , Test.Runner.Node.check Clase3Test.testsPromedio
            , Test.Runner.Node.check Clase3Test.testsLongitudesPalabras
            , Test.Runner.Node.check Clase3Test.testsPalabrasLargas
            , Test.Runner.Node.check Clase3Test.testsSumarPositivos
            , Test.Runner.Node.check Clase3Test.testsDuplicarPares
            , Test.Runner.Node.check Clase3Test.testsAplanar
            , Test.Runner.Node.check Clase3Test.testsAgruparPor
            , Test.Runner.Node.check Clase3Test.testsParticionar
            , Test.Runner.Node.check Clase3Test.testsSumaAcumulada
            , Test.Runner.Node.check Clase3Test.testsSubSets
            , Test.Runner.Node.check Clase3Test.testsCortar
            , Test.Runner.Node.check Clase3Test.suite
            ]
          )
        ]
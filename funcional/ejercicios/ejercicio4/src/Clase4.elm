module Clase4 exposing (..)

{-| Ejercicios de Programación Funcional - Clase 4
Este módulo contiene ejercicios para practicar pattern matching y mónadas en Elm
usando árboles binarios como estructura de datos principal.

Temas:
- Pattern Matching con tipos algebraicos
- Mónada Maybe para operaciones opcionales
- Mónada Result para manejo de errores
- Composición monádica con andThen
-}
import Dict exposing (values)


-- ============================================================================
-- DEFINICIÓN DEL ÁRBOL BINARIO
-- ============================================================================

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


-- ============================================================================
-- PARTE 0: CONSTRUCCIÓN DE ÁRBOLES
-- ============================================================================


-- 1. Crear Árboles de Ejemplo


arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- 2. Es Vacío


esVacio : Tree a -> Bool
esVacio arbol =
    case arbol of
        Empty ->
            True

        Node _ _ _ ->
            False


-- 3. Es Hoja


esHoja : Tree a -> Bool
esHoja arbol =
    case arbol of
        Node _ Empty Empty ->
            True
        _ -> False



-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- esHoja : Tree a -> Bool
-- esHoja arbol =
--     case arbol of
--         Node _ Empty Empty -> True
--         _ -> False


-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- 4. Tamaño del Árbol


tamaño: Tree a -> Int
tamaño arbol =
    case arbol of
        Empty ->
            0

        Node _ left right ->
            (tamaño left) + (tamaño right) + 1


-- 5. Altura del Árbol


altura : Tree a -> Int
altura arbol =
    case arbol of
        Empty -> 0
        Node _ izq der -> (max (altura izq) (altura der)) + 1


-- 6. Suma de Valores


sumarArbol : Tree Int -> Int
sumarArbol arbol =
    case arbol of
        Empty -> 0
        Node v izq der -> v + (sumarArbol izq) + (sumarArbol der)


-- 7. Contiene Valor


contiene : comparable -> Tree comparable -> Bool
contiene valor arbol =
    case arbol of
        Empty -> False
        -- Cortocircuito: si encuentro el valor, retorno True
        Node v izq der -> (v == valor) || (contiene valor izq) || (contiene valor der)


-- 8. Contar Hojas


contarHojas : Tree a -> Int
contarHojas arbol =
    case arbol of
        Empty -> 0
        Node _ Empty Empty -> 1
        Node _ izq der -> (contarHojas izq) + (contarHojas der)


-- 9. Valor Mínimo (sin Maybe)


minimo : Tree Int -> Int
minimo arbol =
    case arbol of
        Empty -> 0
        Node v Empty Empty -> v
        Node v Empty der -> min v (minimo der)
        Node v izq Empty -> min v (minimo izq)
        Node v izq der -> (min v (min (minimo izq) (minimo der)))


-- 10. Valor Máximo (sin Maybe)


maximo : Tree Int -> Int
maximo arbol =
    case arbol of
        Empty -> 0
        Node v Empty Empty -> v
        Node v Empty der -> max v (maximo der)
        Node v izq Empty -> max v (maximo izq)
        Node v izq der -> (max v (max (maximo izq) (maximo der)))


-- ============================================================================
-- PARTE 2: INTRODUCCIÓN A MAYBE
-- ============================================================================


-- 11. Buscar Valor

buscarEnLista: comparable -> List comparable -> Maybe comparable
buscarEnLista valor lista =
    case lista of
        [] -> Nothing
        h::t ->
            if h == valor then
                Just h
            else buscarEnLista valor t

buscar : comparable -> Tree comparable -> Maybe comparable
buscar valor arbol =
    case arbol of
        Empty ->
            Nothing

        Node v izq der ->
            if v == valor then
                Just v
            else
                case buscar valor izq of
                    Just encontrado ->
                        Just encontrado

                    Nothing ->
                        buscar valor der


-- 12. Encontrar Mínimo (con Maybe)


encontrarMinimo : Tree comparable -> Maybe comparable
encontrarMinimo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v izq der -> case ((encontrarMinimo izq), (encontrarMinimo der)) of
            (Nothing, Nothing) -> Just v
            (Just minIzq, Nothing) -> Just (min v minIzq)
            (Nothing, Just minDer) -> Just (min v minDer)
            (Just minIzq, Just minDer) -> Just (min v (min minIzq minDer))


-- 13. Encontrar Máximo (con Maybe)


encontrarMaximo : Tree comparable -> Maybe comparable -- comparable: tipo que se puede comparar (<, >, =)
encontrarMaximo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v izq der -> case ((encontrarMaximo izq), (encontrarMaximo der)) of
            (Nothing, Nothing) -> Just v
            (Just maxIzq, Nothing) -> Just (max v maxIzq)
            (Nothing, Just maxDer) -> Just (max v maxDer)
            (Just maxIzq, Just maxDer) -> Just (max v (max maxIzq maxDer))


-- 14. Buscar Por Predicado


buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor predicado arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> if (predicado v ) then Just v else Nothing
        Node v izq der ->
            case buscarPor predicado izq of
                Just encontrado -> Just encontrado
                Nothing ->
                    if (predicado v) then
                        Just v
                    else
                        buscarPor predicado der

-- 15. Obtener Valor de Raíz


raiz : Tree a -> Maybe a
raiz arbol =
    case arbol of
        Empty -> Nothing
        Node v _ _ -> Just v


-- 16. Obtener Hijo Izquierdo


hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo arbol =
    case arbol of
        Empty -> Nothing
        Node _ Empty _ -> Nothing
        Node _ izq _ -> Just izq



hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho arbol =
    case arbol of
        Empty -> Nothing
        Node _ _ Empty -> Nothing
        Node _ _ der -> Just der


-- 17. Obtener Nieto


nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo arbol =
    -- case hijoIzquierdo arbol of
    --     Nothing -> Nothing
    --     Just hijo -> hijoIzquierdo hijo
    
    Maybe.andThen hijoIzquierdo (hijoIzquierdo arbol)


-- 18. Buscar en Profundidad


obtenerSubarbol : comparable -> Tree comparable -> Maybe (Tree comparable)
obtenerSubarbol valor arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der ->
            if v == valor then
                Just (Node v izq der)
            else
                case obtenerSubarbol valor izq of
                    Just sub -> Just sub
                    Nothing -> obtenerSubarbol valor der

        


buscarEnSubarbol : comparable -> comparable -> Tree comparable -> Maybe comparable
buscarEnSubarbol valor1 valor2 arbol =
     Maybe.andThen
         (\subarbol -> buscar valor2 subarbol)
         (obtenerSubarbol valor1 arbol)



-- ============================================================================
-- PARTE 3: RESULT PARA VALIDACIONES
-- ============================================================================


-- 19. Validar No Vacío


validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio arbol =
    case arbol of
        Empty ->
            Err "El árbol está vacío"

        Node _ _ _ ->
            Ok arbol


-- 20. Obtener Raíz con Error


obtenerRaiz : Tree a -> Result String a
obtenerRaiz arbol =
    case arbol of
        Empty ->
            Err "No se puede obtener la raíz de un árbol vacío"

        Node v _ _ ->
            Ok v


-- 21. Dividir en Valor Raíz y Subárboles


dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir arbol =
    case arbol of
        Empty ->
            Err "No se puede dividir un árbol vacío"

        Node v izq der ->
            Ok ( v, izq, der )


-- 22. Obtener Mínimo con Error


obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo arbol =
    case arbol of
        Empty ->
            Err "No hay mínimo en un árbol vacío"

        _ ->
            case encontrarMinimo arbol of
                Just valorMinimo ->
                    Ok valorMinimo

                Nothing ->
                    Err "El mínimo no se pudo encontrar"


-- 23. Verificar si es BST (Árbol de Búsqueda Binario)


esBST : Tree comparable -> Bool
esBST arbol =
    let
        esConLimites : Maybe comparable -> Maybe comparable -> Tree comparable -> Bool
        esConLimites lower upper tree =
            case tree of
                Empty ->
                    True

                Node v left right ->
                    let
                        cumpleLower =
                            case lower of
                                Nothing -> True
                                Just low -> low < v   -- usar <= si permites duplicados en la derecha/izquierda

                        cumpleUpper =
                            case upper of
                                Nothing -> True
                                Just up -> v < up
                    in
                    cumpleLower && cumpleUpper
                        && esConLimites lower (Just v) left
                        && esConLimites (Just v) upper right
    in
    esConLimites Nothing Nothing arbol

-- 24. Insertar en BST


insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST valor arbol =
    case arbol of
        Empty ->
            Ok (Node valor Empty Empty)

        Node v izq der ->
            if valor < v then
                case insertarBST valor izq of
                    Ok nuevoIzq ->
                        Ok (Node v nuevoIzq der)

                    Err mensajeError ->
                        Err mensajeError
            else if valor > v then
                case insertarBST valor der of
                    Ok nuevoDer ->
                        Ok (Node v izq nuevoDer)

                    Err mensajeError ->
                        Err mensajeError
            else
                Err "El valor ya existe en el árbol"


-- 25. Buscar en BST


buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST valor arbol =
    case arbol of
        Empty ->
            Err "El valor no se encuentra en el árbol"

        Node v izq der ->
            if valor == v then
                Ok v
            else if valor < v then
                buscarEnBST valor izq
            else
                buscarEnBST valor der


-- 26. Validar BST con Result


validarBST : Tree comparable -> Result String (Tree comparable)
validarBST arbol =
    if esBST arbol then
        Ok arbol
    else
        Err "El árbol no es un BST válido"


-- ============================================================================
-- PARTE 4: COMBINANDO MAYBE Y RESULT
-- ============================================================================


-- 27. Maybe a Result


maybeAResult : String -> Maybe a -> Result String a
maybeAResult mensajeError maybe =
    case maybe of
        Just valor ->
            Ok valor

        Nothing ->
            Err mensajeError


-- 28. Result a Maybe


resultAMaybe : Result error value -> Maybe value
resultAMaybe result =
    case result of
        Ok valor ->
            Just valor

        Err _ ->
            Nothing


-- 29. Buscar y Validar


buscarPositivo : Int -> Tree Int -> Result String Int
buscarPositivo valor arbol =
    case buscar valor arbol of
        Just encontrado ->
            if encontrado > 0 then
                Ok encontrado
            else
                Err "El valor encontrado no es positivo"

        Nothing ->
            Err "El valor no se encuentra en el árbol"


-- 30. Pipeline de Validaciones


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol arbol =
    validarNoVacio arbol
        |> Result.andThen validarBST


-- 31. Encadenar Búsquedas


buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    buscarEnBST valor arbol1
        |> Result.andThen (\_ -> buscarEnBST valor arbol2)


-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 32. Recorrido Inorder


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> (inorder izq) ++ [v] ++ (inorder der)


-- 33. Recorrido Preorder


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> [v] ++ (preorder izq) ++ (preorder der)


-- 34. Recorrido Postorder


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> (postorder izq) ++ (postorder der) ++ [v]


-- 35. Map sobre Árbol


mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol funcion arbol =
    case arbol of
        Empty -> Empty
        Node v izq der -> Node (funcion v) (mapArbol funcion izq) (mapArbol funcion der)

-- 36. Filter sobre Árbol

-- Función auxiliar para fusionar dos árboles
mergeTrees : Tree a -> Tree a -> Tree a
mergeTrees arbol1 arbol2 =
    case arbol1 of
        Empty -> arbol2
        Node v izq der -> Node v izq (mergeTrees der arbol2)

filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol predicado arbol =
    case arbol of
        Empty -> Empty
        Node v izq der ->
            let
                izqFiltrado = filterArbol predicado izq
                derFiltrado = filterArbol predicado der
            in
            if predicado v then
                Node v izqFiltrado derFiltrado
            else
                mergeTrees izqFiltrado derFiltrado

-- 37. Fold sobre Árbol


foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol funcion acumulador arbol =
    case arbol of
        Empty -> acumulador
        Node v izq der ->
            let
                acumuladorIzq = foldArbol funcion acumulador izq
                acumuladorDer = foldArbol funcion acumuladorIzq der
            in
            funcion v acumuladorDer


-- 38. Eliminar de BST


eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST valor arbol =
    case arbol of
        Empty ->
            Err "El valor no se encuentra en el árbol"

        Node v izq der ->
            if valor < v then
                case eliminarBST valor izq of
                    Ok nuevoIzq ->
                        Ok (Node v nuevoIzq der)

                    Err mensajeError ->
                        Err mensajeError
            else if valor > v then
                case eliminarBST valor der of
                    Ok nuevoDer ->
                        Ok (Node v izq nuevoDer)

                    Err mensajeError ->
                        Err mensajeError
            else
                -- Valor encontrado, proceder a eliminar
                case (izq, der) of
                    (Empty, Empty) ->
                        Ok Empty

                    (Empty, _) ->
                        Ok der

                    (_, Empty) ->
                        Ok izq

                    _ ->
                        case encontrarMinimo der of
                            Just minDer ->
                                case eliminarBST minDer der of
                                    Ok nuevoDer ->
                                        Ok (Node minDer izq nuevoDer)

                                    Err mensajeError ->
                                        Err mensajeError

                            Nothing ->
                                Err "Error al encontrar el mínimo del subárbol derecho"


-- 39. Construir BST desde Lista


desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST lista =
    case lista of
        [] ->
            Ok Empty

        h::t ->
            case desdeListaBST t of
                Err mensajeError ->
                    Err mensajeError

                Ok arbolParcial ->
                    insertarBST h arbolParcial


-- 40. Verificar Balance


estaBalanceado : Tree a -> Bool
estaBalanceado arbol =
    case arbol of
        Empty -> True
        Node _ izq der ->
            let
                alturaIzq = altura izq
                alturaDer = altura der
            in
            abs (alturaIzq - alturaDer) <= 1
                && estaBalanceado izq
                && estaBalanceado der


-- 41. Balancear BST


balancear : Tree comparable -> Tree comparable
balancear arbol =
    let
        lista = inorder arbol
        n = List.length lista

        build : Int -> List comparable -> ( Tree comparable, List comparable )
        build count lst =
            if count == 0 then
                ( Empty, lst )
            else
                let
                    leftCount = count // 2
                    ( leftTree, restAfterLeft ) =
                        build leftCount lst
                in
                case restAfterLeft of
                    [] ->
                        ( Empty, [] )

                    rootVal :: restAfterLeftTail ->
                        let
                            rightCount = count - leftCount - 1
                            ( rightTree, restAfterRight ) =
                                build rightCount restAfterLeftTail
                        in
                        ( Node rootVal leftTree rightTree, restAfterRight )
    in
    case build n lista of
        ( arbolBalanceado, _ ) ->
            arbolBalanceado


-- 42. Camino a un Valor


type Direccion
    = Izquierda
    | Derecha


encontrarCamino : comparable -> Tree comparable -> Result String (List Direccion)
encontrarCamino valor arbol =
    let
        aux : Tree comparable -> Result String (List Direccion)
        aux tree =
            case tree of
                Empty ->
                    Err "El valor no existe en el árbol"

                Node v izq der ->
                    if v == valor then
                        Ok []
                    else
                        case aux izq of
                            Ok caminoIzq ->
                                Ok (Izquierda :: caminoIzq)

                            Err _ ->
                                case aux der of
                                    Ok caminoDer ->
                                        Ok (Derecha :: caminoDer)

                                    Err _ ->
                                        Err "El valor no existe en el árbol"
    in
    aux arbol

-- 43. Seguir Camino


seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino arbol =
    case arbol of
        Empty ->
            Err "Camino inválido"

        Node v izq der ->
            case camino of
                [] ->
                    Ok v

                Izquierda :: resto ->
                    seguirCamino resto izq

                Derecha :: resto ->
                    seguirCamino resto der


-- 44. Ancestro Común Más Cercano


ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun valor1 valor2 arbol =
    let
        aux : Tree comparable -> Result String (List comparable)
        aux tree =
            case tree of
                Empty ->
                    Err "Uno o ambos valores no existen en el árbol"

                Node v izq der ->
                    if v == valor1 || v == valor2 then
                        Ok [ v ]
                    else
                        case aux izq of
                            Ok caminoIzq ->
                                case aux der of
                                    Ok caminoDer ->
                                        Ok (v :: (caminoIzq ++ caminoDer))

                                    Err _ ->
                                        Ok caminoIzq

                            Err _ ->
                                case aux der of
                                    Ok caminoDer ->
                                        Ok caminoDer

                                    Err _ ->
                                        Err "Uno o ambos valores no existen en el árbol"
    in
    case aux arbol of
        Ok caminos ->
            let
                comunes =
                    List.filter (\x -> List.member x caminos) caminos
            in
            case comunes of
                [] ->
                    Err "No hay ancestro común"

                h::_ ->
                    Ok h

        Err mensajeError ->
            Err mensajeError


-- ============================================================================
-- PARTE 6: DESAFÍO FINAL - SISTEMA COMPLETO
-- ============================================================================


-- 45. Sistema Completo de BST
-- (Las funciones individuales ya están definidas arriba)


-- Operaciones que retornan Bool
esBSTValido : Tree comparable -> Bool
esBSTValido arbol =
    esBST arbol


estaBalanceadoCompleto : Tree comparable -> Bool
estaBalanceadoCompleto arbol =
    estaBalanceado arbol


contieneValor : comparable -> Tree comparable -> Bool
contieneValor valor arbol =
    contiene valor arbol


-- Operaciones que retornan Maybe
buscarMaybe : comparable -> Tree comparable -> Maybe comparable
buscarMaybe valor arbol =
    buscar valor arbol


encontrarMinimoMaybe : Tree comparable -> Maybe comparable
encontrarMinimoMaybe arbol =
    encontrarMinimo arbol


encontrarMaximoMaybe : Tree comparable -> Maybe comparable
encontrarMaximoMaybe arbol =
    encontrarMaximo arbol


-- Operaciones que retornan Result
insertarResult : comparable -> Tree comparable -> Result String (Tree comparable)
insertarResult valor arbol =
    insertarBST valor arbol


eliminarResult : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarResult valor arbol =
    eliminarBST valor arbol


validarResult : Tree comparable -> Result String (Tree comparable)
validarResult arbol =
    validarBST arbol


obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
obtenerEnPosicion posicion arbol =
    if posicion < 0 then Err "Posición inválida"
    else
        let lista = inorder arbol
        in case List.drop posicion lista |> List.head of
            Just v -> Ok v
            Nothing -> Err "Posición inválida"



-- Operaciones de transformación
map : (a -> b) -> Tree a -> Tree b
map funcion arbol =
    mapArbol funcion arbol


filter : (a -> Bool) -> Tree a -> Tree a
filter predicado arbol =
    filterArbol predicado arbol


fold : (a -> b -> b) -> b -> Tree a -> b
fold funcion acumulador arbol =
    foldArbol funcion acumulador arbol


-- Conversiones
aLista : Tree a -> List a
aLista arbol =
    inorder arbol


desdeListaBalanceada : List comparable -> Tree comparable
desdeListaBalanceada lista =
    let
        sorted = List.sortBy identity lista
        -- opcional: eliminar duplicados
        uniq lst =
            case lst of
                [] -> []
                x::xs -> x :: List.filter (\y -> y /= x) xs |> uniq  -- simple, no eficiente
        cleaned = List.foldr (\x acc -> if List.head acc == Just x then acc else x::acc) [] (List.reverse sorted)
        build count items =
            if count == 0 then ( Empty, items )
            else
                let leftCount = count // 2
                    ( leftTree, restAfterLeft ) = build leftCount items
                in
                case restAfterLeft of
                    [] -> ( Empty, [] )
                    rHead :: rTail ->
                        let rightCount = count - leftCount - 1
                            ( rightTree, restAfterRight ) = build rightCount rTail
                        in ( Node rHead leftTree rightTree, restAfterRight )
        n = List.length cleaned
    in
    case build n lista of
        ( arbolBalanceado, _ ) -> arbolBalanceado

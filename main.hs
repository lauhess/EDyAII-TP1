{- 
 -  Trabajo Practico 1 - EDyA II
 -  Alumnos: Laureano Hess, Lautaro Peralta Aguilera
 -}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module TreeDic where


data TTree k v = E | Leaf k v | Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) deriving Show


search :: Ord k => [k] -> TTree k v -> Maybe v
search [] _ = Nothing
search _ E  = Nothing
search [x] (Leaf k v) | x == k = Just v
                      | otherwise = Nothing
search _ (Leaf k v) = Nothing
search key@[x] (Node k v lt ct rt)| x == k = v
                                  | x > k  = search key rt
                                  | x < k  = search key lt
search key@(x:xs) (Node k v lt ct rt) | x == k = search xs ct
                                      | x > k  = search key rt
                                      | x < k  = search key lt
-- Para evitar advertencias respecto a casos no contemplados puesto que no pueden 
-- darse mediante un arbol construido con las funciones insert y delete.
search _ _ = Nothing 


insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] _ t = t
insert [x] v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E
insert [x] v (Leaf k u) | x == k = Leaf k v
                        | x >  k = Node k (Just u) E E (Leaf x v)
                        | x <  k = Node k (Just u) (Leaf x v) E E
insert key@(x:xs) v (Leaf k u) | x == k = Node k (Just u) E (insert xs v E) E
                               | x >  k = Node k (Just u) E E (insert key v E)
                               | x <  k = Node k (Just u) (insert key v E) E E
insert key@[x] v (Node k u lt ct rt) | x == k = Node k (Just v) lt ct rt
                                     | x >  k = Node k u lt ct (insert key v rt)
                                     | x <  k = Node k u (insert key v lt) ct rt
insert key@(x:xs) v (Node k u lt ct rt) | x == k = Node k u lt (insert xs v ct) rt
                                        | x >  k = Node k u lt ct (insert key v rt)
                                        | x <  k = Node k u (insert key v lt) ct rt
-- Igual que el útlimo pattern matching en search
insert _ _ _ = E


{-
 - NodePostProcessing
 - Función que acomoda ciertos casos particulares
 - como vacios o rotaciones necesarias al finalizar
 - la eliminación.
 -}
nodePostP :: Ord k => TTree k v -> TTree k v
nodePostP (Node k Nothing E E E) = E
nodePostP (Node k Nothing E E rt) = rt
nodePostP (Node k Nothing lt E E) = lt
nodePostP (Node k Nothing lt E rt@(Leaf j u)) = Node j (Just u) lt E E
nodePostP (Node k Nothing lt@(Leaf j u) E rt) = Node j (Just u) E E rt
nodePostP (Node k (Just v) E E E) = Leaf k v
nodePostP nodo@(Node k Nothing lt E rt) = nodo
nodePostP nodo@(Node k v lt ct rt) = nodo
nodePostP _ = E -- Casos restantes eran E y Leaf

delete :: Ord k => [k] -> TTree k v -> TTree k v
delete [] t = t
delete _ E = E
delete [x] hoja@(Leaf k v) | x == k = E
                           | otherwise = hoja
delete (x:xs) hoja@(Leaf k v)  = hoja 
delete key@[x] (Node k v lt ct rt) | x > k = nodePostP (Node k v lt ct (delete key rt))
                                   | x < k = nodePostP (Node k v (delete key lt) ct rt)
                                   | otherwise = nodePostP (Node k Nothing lt ct rt)
delete key@(x:xs) (Node k v lt ct rt) | x > k = nodePostP (Node k v lt ct (delete key rt))
                                      | x < k = nodePostP (Node k v (delete key lt) ct rt)
                                      | otherwise = nodePostP (Node k v lt (delete xs ct) rt)


-- Función para obtener el valor dentro de un Just
unJust :: Maybe a -> a
unJust (Just v) = v
unJust Nothing = error "Esto no debería pasar..."

-- Si tenemos una tupla clave-valor, 
-- podemos agregar extender la clave
appendToKey :: k -> ([k], v) -> ([k], v)
appendToKey k (ks, v) = (k:ks, v)

-- Función para devolver una lista con
-- todas las claves y sus valores para
-- usar en la instancia de Dic
keyVal :: Ord k => TTree k v -> [([k], v)]
keyVal E = []
keyVal (Leaf k v) = [([k], v)]
keyVal (Node k Nothing lt ct rt) = keyVal lt 
  ++ map (appendToKey k) (keyVal ct) 
  ++ keyVal rt
keyVal (Node k v lt ct rt) = keyVal lt 
  ++ [([k], unJust v)] 
  ++ map (appendToKey k) (keyVal ct) 
  ++ keyVal rt

--  Puesto que keyVal ya nos devuelve las llaves
--  para no repetir código, la llamamos y tomamos 
--  solo el primer elemento de cada tupla.
keys :: Ord k => TTree k v -> [[k]]
keys t = map fst (keyVal t)


class Dic k v d | d -> k v where
  vacio     :: d
  insertar  :: Ord k => k -> v -> d -> d
  buscar    :: Ord k => k -> d -> Maybe v
  eliminar  :: Ord k => k -> d -> d
  claves    :: Ord k => d -> [(k , v )]

-- Hacemos de TTree una instancia de la clase Dic
instance Ord k => Dic [k] v (TTree k v) where
  vacio     = E 
  insertar  = insert  
  buscar    = search 
  eliminar  = delete 
  claves    = keyVal 

m = Node 'd' (Just 3) (Node 'c' (Just 3) (Leaf 'a' 3) E E) E (Node 'h' (Just 3) E E (Leaf 'y' 3))   
t  = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E) (Node 'o' (Just 2) (Leaf 'd' 9) E (Leaf 's' 4)) E) (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8) (Leaf 'n' 7) E) E)

-- Se importan bibliotecas necesarias.
import System.IO -- Para la lectura de archivos.
import Data.List -- Para utilizar la función nub para eliminar elementos repetidos de una lista.

-- Lee un archivo de texto, agrega en una lista todos los números y palabras que contenga
-- y finalmente retorna dicha lista.
--leerArchivo :: FilePath -> IO [String]
leerArchivo nombreArchivo = do
  	contenido <- readFile nombreArchivo -- Se almacena todo el contenido del archivo.
  	let listaArchivo = lines contenido  -- Se guardan las filas del archivo en una lista.

  	-- Se verifica el cierre correcto del archivo.
  	if verificarCierreArchivo listaArchivo == True 
  		then recorrerPorFilas listaArchivo 0 (length listaArchivo) [] []
  		else putStrLn ("El archivo no cierra correctamente.") -- En caso de que no encuentre el "0 0".
  	--return listaArchivo

-- Recibe una lista con las filas del archivo, verifica si al final de la lista aparece "0 0" para 
-- indicar el cierre del documento y si lo encuentra devuelve True, en caso contrario devuelve False.
verificarCierreArchivo :: [String] -> Bool 
verificarCierreArchivo listaArchivo = do
	let tamArchivo = length listaArchivo
	if listaArchivo !! (tamArchivo - 1) == "0 0"
		then True
		else False

-- Recibe una lista con las filas del archivo, un contador que inicia en 0, la cantidad de lineas,
-- una lista de valores de la forma ["4", "2"], pero inicialmente es []; y una lista de personas 
-- de la forma ["Ana", "Carlos", "Ursula", "Karla"], pero inicialmente es [] también. 
-- Se utiliza para recorrer cada una de las filas del archivo para ir haciendo las verificaciones 
-- necesarias.
recorrerPorFilas :: [String] -> Int -> Int -> [String] -> [String] -> IO ()
recorrerPorFilas (x:xs) numLinea tamArchivo listaValores listaPersonas = do
  -- Se separa la línea en palabras que se guardan en una lista.
  -- Por ejemplo: recibe ["4 2"] y lo convierte a ["4", "2"].
  let listaLinea = words x
  -- Si llegó al final del archivo.
  if numLinea == (tamArchivo - 1) 
  	then putStrLn ("Terminó.") 
  	-- Si es una línea par, entonces se trata de "cantidadPersonas cantidadRelaciones", por ejemplo "4 4".
  	else if mod numLinea 2 == 0
  		then do
  			print listaLinea
  			recorrerPorFilas xs (numLinea + 1) tamArchivo listaLinea listaPersonas
  		-- Si es una línea impar, entonces se trata de las diferentes personas que forman parte de la red.
  		else 
        -- Verifica si los valores y la información de la red son correctos.
  			if verificarDatos listaValores listaLinea == True 
  				then do
            print listaLinea
            recorrerPorFilas xs (numLinea + 1) tamArchivo listaValores listaLinea 
  				else putStrLn ("Hay un error cerca de la línea " ++ show numLinea ++ " ó " ++ 
                          show (numLinea + 1) ++ " del archivo.")

-- Recibe una lista de valores y una lista de personas que forman parte de la red y verifica si la cantidad 
-- de personas y la cantidad de conexiones coinciden con la información de línea donde vienen las conexiones.
verificarDatos :: [String] -> [String] -> Bool
verificarDatos listaValores listaPersonas = do
	-- Se convieten a enteros los valores de la lista de valores.
	let cantPersonas      = read (listaValores !! 0) :: Int 
	let cantRelaciones 	  = read (listaValores !! 1) :: Int
	
	-- Se eliminan las personas repetidas de la lista de personas.
	let listaSinRepetidos = nub (listaPersonas) 

	-- Se verifica si la cantidad de personas (que debe estar entre 2 y 50, incluyendo a estos valores) 
  -- y la cantidad de relaciones (que debe ser mayor o igual a 1) en la lista, están correctas, de ser 
  -- así devuelve True, en caso contrario, devuelve False.
	if (2 <= cantPersonas && cantPersonas <= 50) && (1 <= cantRelaciones) && div (length listaPersonas) 2 == cantRelaciones && 
	   (length listaSinRepetidos) == cantPersonas
		then True
		else False



import Data.List
import System.IO
import Control.Exception ()
import Control.DeepSeq (deepseq)
import Data.Time.LocalTime (getZonedTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    iD :: String,
    entrada :: ZonedTime,
    saliDa :: Maybe ZonedTime  -- Usamos Maybe para representar que el estudiante aún está en el universidad o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante a la universidad
registrarEntrada :: String -> ZonedTime -> [Estudiante] -> [Estudiante]
registrarEntrada iDEstudiante tiempo universidad =
    Estudiante iDEstudiante tiempo Nothing : universidad

-- Función para registrar la salida de un estudiante de la universidad
registrarSaliDa :: String -> ZonedTime -> [Estudiante] -> [Estudiante]
registrarSaliDa iDEstudiante tiempo universidad =
    map (\v -> if iDEstudiante == iD v then v { saliDa = Just tiempo } else v) universidad

-- Función para buscar un estudiante por su iD en la universidad
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante iDEstudiante universidad =
    find (\v -> iDEstudiante == iD v && isNothing (saliDa v)) universidad
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función auxiliar para calcular la diferencia entre dos ZonedTime
diffZoned :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZoned t1 t2 = diffUTCTime (zonedTimeToUTC t1) (zonedTimeToUTC t2)

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getZonedTime
    return $ diffZoned tiempoActual (entrada estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad universidad = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante universidad))
    putStrLn "Estudiante guardado en el archivo universidad.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    conteniDo <- withFile "universidad.txt" ReadMode $ \h -> do
        conteniDo <- hGetContents h
        conteniDo `deepseq` return conteniDo
    let lineas = lines conteniDo
    return (map leerEstudiante lineas)
    where
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante iD entrada saliDa) =
    "Estudiante {iD = \"" ++ iD ++ "\", entrada = " ++ show entrada ++ ", salida = " ++ maybe "Nothing" show saliDa ++ "}"

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes en el universidad."
listarEstudiantes estudiantes = do
    putStrLn "estudiantes en el universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el resgistro de estudiantes desde el archivo de texto
    universidad <- cargarUniversidad
    putStrLn "¡BienveniDo al Sistema de registro de estudiantes de la universidad!"

    -- Ciclo principal del programa
    cicloPrincipal universidad

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal universidad = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por iD"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la iD del estudiante:"
            iDEstudiante <- getLine
            tiempoActual <- getZonedTime
            let universidadActualizado = registrarEntrada iDEstudiante tiempoActual universidad
            putStrLn $ "estudiante con iD " ++ iDEstudiante ++ " ingresado a la universidad."
            guardarUniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "2" -> do
            putStrLn "Ingrese la iD del estudiante a salir:"
            iDEstudiante <- getLine
            tiempoActual <- getZonedTime
            let universidadActualizado = registrarSaliDa iDEstudiante tiempoActual universidad
            putStrLn $ "estudiante con iD " ++ iDEstudiante ++ " salido de la universidad."
            guardarUniversidad universidadActualizado
            cicloPrincipal universidadActualizado

        "3" -> do
            putStrLn "Ingrese la iD del estudiante a buscar:"
            iDEstudiante <- getLine
            case buscarEstudiante iDEstudiante universidad of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con iD " ++ iDEstudiante ++ " se encuentra en el universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal universidad

        "4" -> do
            listarEstudiantes universidad
            cicloPrincipal universidad

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no váliDa. Por favor, seleccione una opción válida."
            cicloPrincipal universidad


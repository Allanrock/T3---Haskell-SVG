{--
        Esqueleto de programa para geraÃ§Ã£o de bubble cloud em Haskell.
        Mais informaÃ§Ãµes em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List
import Data.Function

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 3600

imageHeight :: Int
imageHeight = 3600


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


calcRaio :: [Int] -> Int -> [Float]
calcRaio [] soma = []
calcRaio lista soma = percent : calcRaio cauda soma
        where
        percent = (800*cabeca)/somaFloat + 1
        cauda = (tail lista)
        cabeca = fromIntegral (head lista) :: Float
        somaFloat = fromIntegral soma :: Float
-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [geraCirculos (fromIntegral w/2) (fromIntegral h/2) raios 0.0 []]
        where
        ordena = (sort dataset)
        raios = reverse (calcRaio ordena (sum dataset))
        


-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> String
svgCircle ((x,y),r) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,0,0)\" />\n" x y r (fromEnum (r))

-- Calcula o percentual para gerar o raio (raio máximo temporariamente 100)
calcCirculo :: Int -> Float
calcCirculo n
        |n < 10 = 25
        |n < 50 = 50
        |n < 250 = 100
        |n < 500 = 150
        |n < 1000 = 200
        |n < 3000 = 250

pitagoras :: Circle -> Circle -> Float
pitagoras ((x1,y1),_) ((x2,y2),_) = sqrt r3
        where
        r1 = (x1-x2)^2
        r2 = (y1-y2)^2
        r3 = (r1+r2)
                
boolIntersec :: [Circle] -> Circle -> Bool
boolIntersec [] n = False
boolIntersec lista n
        |distancia > somaRaios = boolIntersec (tail lista) n
        |distancia <= somaRaios = True
        where
        distancia = pitagoras (head lista) n
        somaRaios = snd (head lista) + (snd n)
               

geraPonto :: Float -> Point
geraPonto t = novo
        where
        px = 1800+(8 * t * (cos t))
        py = 1800+(8 * t * (sin t))
        novo = (px,py)
        
--Gera as coordenadas
geraCirculos :: Float -> Float -> [Float] -> Float -> [Circle] -> String
geraCirculos _ _ [] _ _ = []
geraCirculos x y raios t circles
        |circles == [] = geraCirculos x y raios t addcirc
        |boolIntersec circles ((geraPonto t),raio) == True = geraCirculos x y raios (t+0.25) circles
        |boolIntersec circles ((geraPonto t),raio) == False = svgCircle ((x, y), raio) ++ (geraCirculos px py(tail raios) t addcirc)
        where
        px = fst (geraPonto t)
        py = snd (geraPonto t)
        addcirc = ((x,y),raio) : circles
        raio = (head raios)
        
-- Configura o viewBox da imagem e coloca retangulo branco no fundo

svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf  "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h
module Main where
-- Driver which calls the different phases in the right order

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode
import CSharpPrint

start :: Parser s a -> [s] -> a
start p = fst . head . filter (null . snd) . parse p

main :: IO ()
main = do
		-- get command line arguments
		args  <- getArgs
		-- compute a list of input an output files
		files <- case args of
				[]  ->  do
						  putStrLn "no argument given; assuming example.cs"
						  return [("Example.cs", "Example.ssm")]
				xs  ->  do
						 putStrLn "succesful"
						 return (map (\ f -> (f, addExtension (dropExtension f) "ssm")) xs)
		-- translate each of the files
		mapM_ processFile files
        
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = formatCode 
                . foldCSharp codeAlgebra
                . start (pClass <* eof)
                . start lexicalScanner

checkParse xs = do
            x <- readFile xs
            let l = process x
            putStrLn l
        where
            process = show--formatCode 
                -- . foldCSharp codeAlgebra
                .start (pClass <* eof)
                . start lexicalScanner
                
make x = do
		-- compute a list of input an output files
		files <- case x of
				[]  ->  do
						  putStrLn "no argument given; assuming example.cs"
						  return [("Testconsts.cs", "Testconsts.ssm")]
				xs  ->  do
						 putStrLn "succesful"
						 return (map (\ f -> (f, addExtension (dropExtension f) "ssm")) xs)
		-- translate each of the files
		mapM_ processFile files
        
printer xs = do
            x <- readFile xs
            let l = process x
            putStrLn l
        where
            process = show
                . foldCSharp printAlgebra
                . start (pClass <* eof)
                . start lexicalScanner
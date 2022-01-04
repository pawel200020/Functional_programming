import System.IO;
import System.Environment;    -- importujemy ten modul po to, aby skorzystac z funkcji getArgs

showFile :: Handle -> IO ()
showFile handle = do
		      eof<-hIsEOF handle
		      if eof then return () 
			     else 
				  do
				    line<-hGetLine handle
				    print line
				    showFile handle
main :: IO ()        
main = do
           (firstArg:_) <-getArgs
           fileHandle <-openFile firstArg ReadMode
           showFile fileHandle
           hClose fileHandle           

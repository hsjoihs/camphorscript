import CamphorR.R_Step8
import System.IO
getFile::FilePath->IO String
getFile path=openFile path ReadMode>>=hGetContents
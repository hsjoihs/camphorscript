import CamphorR.R_Step8
import CamphorR.R_Base_Step7
import System.IO
import Camphor.IO
getFile :: FilePath -> IO String
getFile path = openFile path ReadMode >>= hGetContents
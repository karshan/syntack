module System.IO.Util
    (
      putStdErrLn
    ) where

import System.IO (hPutStrLn, stderr)

putStdErrLn :: String -> IO ()
putStdErrLn = hPutStrLn stderr

module Get where

-- import qualified Data.ByteString.Lazy as LBS
-- import Network.HTTP.Conduit

-- TODO modify to give a String and auto-generate filename, and directory?? from String
-- | TODO
-- >>> downloadFile "http://www.example.com" "./file.mp3"
-- inside downloadFile. url: http://www.example.com fpath: ./file.mp3
-- writing http://www.example.com to ./file.mp3
downloadFile :: String -> FilePath -> IO ()
downloadFile url fpath = do
  putStrLn $ "inside downloadFile. url: " ++ url ++ " fpath: " ++ fpath
  -- TODO uncomment these two commented lines
  -- response <- simpleHttp url
  putStrLn $ "writing " ++ url ++ " to " ++ fpath
  -- LBS.writeFile fpath response

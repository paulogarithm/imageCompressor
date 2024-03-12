module Compressor(compressor) where

    import Bootstrap(closest, Truple)

    list :: [Truple]
    list = [(33,18,109),(33,17,109),(35,18,111),(35,21,109),(38,21,112)]

    compressor :: IO ()
    compressor = do
        print (closest list (34,18,112))

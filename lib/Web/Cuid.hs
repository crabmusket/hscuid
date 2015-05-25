module Web.Cuid (
    newCuid
) where

import Data.String (fromString)
import Data.Text (Text)

newCuid :: IO Text
newCuid = return (fromString "c")

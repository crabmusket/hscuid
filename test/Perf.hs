import qualified Criterion.Main as C
import Web.Cuid (newCuid, newSlug)

main :: IO ()
main = C.defaultMain
    [ C.bench "newCuid" $ C.nfIO newCuid
    , C.bench "newSlug" $ C.nfIO newSlug
    ]

import Control.Lens (view)
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Debianize hiding (backups)
import MIMO.App (spec)
import MIMO.Debianize (customize)
import Stage1Def (theAppInfo)

main :: IO ()
main = performDebianization (seereasonDefaults >> customize (view spec theAppInfo))

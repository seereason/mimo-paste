import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Debianize hiding (backups)
import MIMO.Debianize (customize)
import Spec (spec)

main :: IO ()
main = performDebianization (seereasonDefaults >> customize spec)

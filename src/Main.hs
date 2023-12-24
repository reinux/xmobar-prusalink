import Xmobar
    ( defaultConfig,
      Config(alignSep, font, allDesktops, alpha, commands, template, textOutput),
      Date(Date),
      Kbd(Kbd),
      Monitors(Memory),
      XMonadLog(XMonadLog),
      Runnable(Run), xmobar )
import MK4Monitor ( MK4 (MK4), mk4Netrc )

config :: MK4 -> Config
config mk4 =
  defaultConfig
    { font = "DejaVu Sans Mono 9",
      allDesktops = True,
      alpha = 200,
      commands =
        [ Run XMonadLog,
          Run $ Memory ["t", "Mem: <usedratio>%"] 10,
          Run $ Kbd [],
          Run mk4,
          Run $ Date "%a %_d %b %Y <fc=#ee9a00>%H:%M:%S</fc>" "date" 10
        ],
      template = "%XMonadLog% }{ %mk4% | %kbd% | %date% | %memory%",
      textOutput = True,
      alignSep = "}{"
    }

main :: IO ()
main = do
  mk4 <- mk4Netrc "mk4"
  case mk4 of
    Nothing -> print "No netrc entry for mk4"
    Just mk4' -> xmobar (config mk4') -- or: configFromArgs config >>= xmobar
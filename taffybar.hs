{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import qualified GI.Gtk as Gtk
import System.Process (readProcess)
import System.Taffybar
import System.Taffybar.Context
import qualified System.Taffybar.SimpleConfig as S
import System.Taffybar.Util
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import System.Taffybar.Widget.SNITray (sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt)

main = startTaffybar $ S.toTaffyConfig taffyConfig

----------------
-- Utils
----------------

frenchLocale :: IO TimeLocale
frenchLocale = do
  timeZone <- getCurrentTimeZone
  return
    TimeLocale
      { wDays =
          [ ("Dimanche", "Dim"),
            ("Lundi", "Lun"),
            ("Mardi", "Mar"),
            ("Mercredi", "Mer"),
            ("Jeudi", "Jeu"),
            ("Vendredi", "Ven"),
            ("Samedi", "Sam")
          ],
        months =
          [ ("Janvier", "Jan"),
            ("Février", "Fév"),
            ("Mars", "Mar"),
            ("Avril", "Avr"),
            ("Mai", "Mai"),
            ("Juin", "Juin"),
            ("Juillet", "Juil"),
            ("Août", "Aoû"),
            ("Septembre", "Sep"),
            ("Octobre", "Oct"),
            ("Novembre", "Nov"),
            ("Décembre", "Déc")
          ],
        amPm = ("", ""),
        dateTimeFmt = "%d/%m/%y %H:%M:%S",
        dateFmt = " %d/%m/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S",
        knownTimeZones = [timeZone]
      }

----------------
-- Widgets
----------------

clockWidget :: TaffyIO Gtk.Widget
clockWidget = do
  l <- liftIO frenchLocale
  let taffyClock =
        textClockNewWith
          defaultClockConfig
            { clockTimeLocale = Just l,
              clockFormatString = "\xf017 %A %d %B %H:%M",
              clockUpdateStrategy = RoundedTargetInterval 30 0.0
            }

  w <- taffyClock
  addCssClass "end-widget" w
  addCssClass "clock" w

workspaceWidget :: TaffyIO Gtk.Widget
workspaceWidget =
  workspacesNew
    defaultWorkspacesConfig
      { widgetGap = 0,
        urgentWorkspaceState = True
      }


windowsWidget :: TaffyIO Gtk.Widget
windowsWidget = windowsNew defaultWindowsConfig

fsMonitorWidget :: TaffyIO Gtk.Widget
fsMonitorWidget =
  fsMonitorNew 500 ["/"]
    >>= addCssClass "fs-monitor"

batteryWidget :: TaffyIO Gtk.Widget
batteryWidget =
  textBatteryNew "\xf240   $percentage$% / $time$"
    >>= addCssClass "battery"

kernelVersionWidget :: TaffyIO Gtk.Widget
kernelVersionWidget = pollingLabelNew 30 kernelVersion
  where
    kernelVersion = readProcess "uname" ["-r"] "" >>= \s -> return $ T.pack $ "\xf303  " ++ head (lines s)

addCssClass :: T.Text -> Gtk.Widget -> TaffyIO Gtk.Widget
addCssClass c w = liftIO $ widgetSetClassGI w c

----------------
-- Main config
----------------

taffyConfig :: S.SimpleTaffyConfig
taffyConfig =
  S.defaultSimpleTaffyConfig
    { S.startWidgets =
        [ workspaceWidget
        ],
      S.centerWidgets =
        [ kernelVersionWidget
        ],
      S.endWidgets =
        [ sniTrayNew,
          clockWidget,
          batteryWidget,
          fsMonitorWidget,
          textCpuMonitorNew "\xf0e4   $total$" 2
            >>= addCssClass "cpu"
        ],
      S.barHeight = S.ExactSize 40,
      S.widgetSpacing = 30,
      S.cssPaths = ["/home/francois/.config/taffybar/taffybar.css"]
    }


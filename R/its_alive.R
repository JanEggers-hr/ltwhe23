# its_alive.R - Watchdog-Skript für update_all.R
#
# Wird per CRON-Job aufgerufen: Wenn das letzte Update der Log-Datei "wahl.log"
# länger als x Minuten zurückliegt, löst das Skript einen Alarm über Teams aus.

# Mehr als die Datumsfunktionen brauchen wir nicht
library(lubridate)
library(this.path)

# Das Projektverzeichnis "ltwhe23" als Arbeitsverzeichnis wählen
# Aktuelles Verzeichnis als workdir
setwd(this.path::this.dir())
# Aus dem R-Verzeichnis eine Ebene rauf
setwd("..")

# Teams-Funktionen einbinden
source("R/messaging.R")

# Maximales Alter in Sekunden?
max_alter = 120

# Startzeit festhalten
ts = now()

# Gibt es überhaupt eine Logdatei? 

if (file.exists("ltwhe.log")) {
  metadaten <- file.info("ltwhe.log")
  # Berechne Alter der Logdatei in Sekunden
  alter = as.integer(difftime(ts,metadaten$mtime,units="secs"))
  if (alter > max_alter)
  {
    cat("WATCHDOG its_alive.R: ltwhe.log seit ",alter," Sekunden unverändert")
    cat("Benenne ltwhe.log um in ltwhe_crash.log")
    file.rename("ltwhe.log","ltwhe_crash.log")
    teams_error("PROGRAMM STEHEN GEBLIEBEN? ltwhe.log ist seit ",alter," Sekunden unverändert")
  }
} else {
  # Tue nichts.
  cat("its_alive.R: ltwhe.log im Arbeitsverzeichnis",getwd(),"nicht gefunden")
}

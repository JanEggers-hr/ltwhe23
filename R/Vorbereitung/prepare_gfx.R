library(pacman)

# Laden und ggf. installieren
p_load(this.path)
p_load(readr)
p_load(lubridate)
p_load(tidyr)
p_load(stringr)
p_load(dplyr)
p_load(DatawRappr)
p_load(curl)
p_load(magick)
p_load(openxlsx)
p_load(R.utils)
p_load(utils)
p_load(jsonlite)

rm(list=ls())

# Aktuelles Verzeichnis als workdir
setwd(this.path::this.dir())
# Aus dem R-Verzeichnis eine Ebene rauf
setwd("..")

# Deutsche Zahlen, Daten, Datumsangaben
Sys.setlocale(locale = "de_DE.UTF-8")
TEST = TRUE
DO_PREPARE_MAPS = TRUE

# Logfile anlegen, wenn kein Test
if (!TEST) {
  logfile = file("obwahl.log")
  sink(logfile, append=T)
  sink(logfile, append=T, type="message")
}

# Messaging-Funktionen einbinden
source("R/messaging.R")


# Hole die Konfiguration, die Index-Daten und die Vergleichsdaten
source("R/lies_konfiguration.R")

# Funktionen einbinden
# Das könnte man auch alles hier in diese Datei schreiben, aber ist es übersichtlicher

check = tryCatch(
  { # Bibliotheken
    source("R/lies_aktuellen_stand.R")
    source("R/aktualisiere_grafiken.R")
  },
  warning = function(w) {teams_warning(w,title=paste0(wahl_name,": Bibliotheksfunktionen?"))},
  error = function(e) {teams_warning(e,title=paste0(wahl_name,": Bibliotheksfunktionen?"))}
)  



# Grafiken einrichten: Farbwerte und Switcher für die Karten
# Richtet auch die globale Variable switcher ein, deshalb brauchen wir sie

if (DO_PREPARE_MAPS) {
  
  # Säulengrafiken-Farben anpassen, individuelle CSV
  kreise_direkt_saeulen()
  
  # Alle Grafiken auf CSV-und JSON-URL umbiegen
  #fix_data(datawrapper_ids_df %>%  pull(dw_id))
  gemeinden_landesstimme_tabelle()
  gemeinden_direkt_tabelle()
  
  if (SERVER) {
    n <- now()
    system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/* gs://d.data.gcp.cloud.hr.de/livedaten/')
    copy_time <- now()-n
    cat("Operation took ",copy_time)
  } else { teams_warning("Lokaler Zyklus, keine Daten auf Google Bucket kopiert")}
  
} 

# Logging beenden
if (!TEST) {
  cat("OK: FERTIG - alle Stimmbezirke ausgezählt: ",as.character(ts),"\n")
  sink()
  sink(type="message")
  file.rename("obwahl.log","obwahl_success.log")
  teams_meldung(wahl_name," erfolgreich abgeschlossen.")
  
  
}

# EOF


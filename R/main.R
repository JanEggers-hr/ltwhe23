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

# Lies Kommandozeilen-Parameter: 
# (Erweiterte Funktion aus dem R.utils-Paket)
# Kommandozeilen-Argumente
args = R.utils::commandArgs(asValues = TRUE)
if (length(args)!=0) { 
  if (any(c("h","help","HELP") %in% names(args))) {
    cat("Parameter: \n",
        "--TEST schaltet Testbetrieb ein\n",
        "--DO_PREPARE_MAPS schaltet Generierung der Switcher ein\n",
        "wahl_name=<name> holt Index-Dateien aus dem Verzeichnis ./index/<name>\n\n")
  }
  TEST <- "TEST" %in% names(args)
  DO_PREPARE_MAPS <- "DO_PREPARE_MAPS" %in% names(args)
  if ("wahl_name" %in% names(args)) {
    wahl_name <- args[["wahl_name"]]
    if (!dir.exists(paste0("index/",wahl_name))) stop("Kein Index-Verzeichnis für ",wahl_name)
  }
} 
TEST = TRUE
DO_PREPARE_MAPS = FALSE



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



#---- MAIN ----
# Vorbereitung
gezaehlt <- 0 # Ausgezählte Stimmbezirke
ts <- as_datetime(startdatum) # ts, Zeitstempel, der letzten gelesenen Daten

# Anzahl Stimmbezirke bestimmen
# Anzahl Stimmbezirke: einmal aus der Hessen-Zeile filtern 
alte_daten <- hole_daten(stimmbezirke_url, copy = FALSE) # Leere Stimmbezirke
stimmbezirke_n <- alte_daten %>% filter(Gebietstyp == "LD") %>% select(all_of(stimmbezirke_i)) %>% pull()

# Grafiken einrichten: Farbwerte und Switcher für die Karten
# Richtet auch die globale Variable switcher ein, deshalb brauchen wir sie


if (DO_PREPARE_MAPS) {
  
  # Säulengrafiken-Farben anpassen, individuelle CSV
  kreise_direkt_saeulen()
  if (SERVER) {
    n <- now()
    system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/* gs://d.data.gcp.cloud.hr.de/livedaten/')
    copy_time <- now()-n
    cat("Operation took ",copy_time)
  } else { teams_warning("Lokaler Zyklus, keine Daten auf Google Bucket kopiert")}
  # Alle Grafiken auf CSV-und JSON-URL umbiegen
  fix_data(datawrapper_ids_df %>%  pull(dw_id))
  gemeinde
} 

# Schleife.
# Arbeitet so lange, bis alle Wahlbezirke ausgezählt sind. 
# Als erstes immer einmal durchlaufen lassen. 
while (gezaehlt < stimmbezirke_n) {
  check = tryCatch(
    { # Zeitstempel der Daten holen
      ts_daten <- check_for_timestamp(stimmbezirke_url)
    },
    warning = function(w) {teams_warning(w,title=paste0(wahl_name,": CURL-Polling"))},
    error = function(e) {teams_warning(e,title=paste0(wahl_name,": CURL-Polling"))}
  )  
  # Neuere Daten? 
  if (ts_daten > ts) {
    ts <- ts_daten
    live_df <- hole_daten(stimmbezirke_url)
    # Als erstes: Landesstimmen ganz Hessen
    forme_hessen_landesstimmen(live_df) %>% 
      aktualisiere_hessen_landesstimmen()
    cat("Hessen aktualisiert")
    #
    live_kreise_direkt_lang_df <- forme_kreise_direkt(live_df)
    write_csv(live_kreise_direkt_lang_df,"livedaten/kreise_direkt_lang.csv")
    aktualisiere_kreise_direkt(live_kreise_direkt_lang_df)
    cat("Grafiken Kreise-Direktstimmen CSV/JSON aktualisiert\n")
    #
    live_kreise_landesstimmen_lang_df <- forme_kreise_landesstimmen(live_df)
    write_csv(live_kreise_landesstimmen_lang_df,"livedaten/kreise_landesstimmen_lang.csv")
    aktualisiere_kreise_landesstimmen(live_kreise_landesstimmen_lang_df)
    #---- Noch keine Testdaten für Gemeinden ----
    
    # live_gemeinden_direkt_lang_df <- forme_gemeinden_direkt(live_df)
    # write_csv(live_gemeinden_direkt_lang_df,"livedaten/gemeinden_direkt_lang.csv")
    # aktualisiere_gemeinden_direkt(live_gemeinden_direkt_lang_df)
    # cat("Grafiken Gemeinde Direktstimmen CSV/JSON aktualisiert\n")
    # #
    # live_gemeinden_landesstimmen_lang_df <- forme_gemeinden_landesstimmen(live_df)
    # write_csv(live_gemeinden_landesstimmen_lang_df,"livedaten/gemeinden_landesstimmen_lang.csv")
    # # aktualisiere_staedte_landesstimmen(live_df) Schon mit drin
    # cat("Grafiken Gemeinde Landesstimmen CSV/JSON aktualisiert\n")
    # #
    cat("Aktualisierte Daten kopiert in",aktualisiere_bucket_alle(),"\n")
    #
    neu_gezaehlt <- live_df %>% filter(Gebietstyp == "LD") %>% select(all_of(gezaehlt_i)) %>% pull()
    # Nachricht neu gezählte Stimmbezirke
    teams_meldung("Gezählte Stimmbezirke: ",neu_gezaehlt," (neu: ",neu_gezaehlt-gezaehlt,")")
    gezaehlt <- neu_gezaehlt
  } else {
    # Logfile erneuern und 30 Sekunden schlafen
    system("touch obwahl.log")
    if (TEST) cat("Warte...\n")
    Sys.sleep(30)
  }
}
# Titel der Grafik "top" umswitchen
# dw_edit_chart(top_id,title="Ergebnis: Wahlsieger")
# dw_publish_chart(top_id)

# Logging beenden
if (!TEST) {
  cat("OK: FERTIG - alle Stimmbezirke ausgezählt: ",as.character(ts),"\n")
  sink()
  sink(type="message")
  file.rename("obwahl.log","obwahl_success.log")
}
teams_meldung(wahl_name," erfolgreich abgeschlossen.")


# EOF
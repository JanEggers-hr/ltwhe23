# lies_aktuellen_stand.R
#
# Enthält die Funktion zum Lesen der aktuellen Daten. 

#---- Hilfsfunktionen ----

hole_daten <- function(stand_url,a_directory = "daten") {
  #' Schreibt das Dataframe mit den zuletzt geholten Stimmbezirks-Daten
  #' als Sicherungskopie in das angegebene Verzeichnis
  #' 
  if (!dir.exists(a_directory)) {
    dir.create(a_directory)
  }
  
  fname = paste0(a_directory,"/",
                 str_replace_all(now(),"\\:","") %>% str_sub(1,15),
                 ".csv")
  # Bei Internet-Adresse: Daten aus dem Netz in den lokalen Ordner kopieren
  if (str_detect(stand_url,"^http")) {
    check = tryCatch(
      { 
        # Einmal Daten einlesen
        utils::download.file(stand_url,fname)
      },
      warning = function(w) {teams_warning(w,title=paste0(wahl_name,": Daten von ",stand_url," kopieren"))},
      error = function(e) {teams_warning(e,title=paste0(wahl_name,": Kann Daten nicht von ",stand_url,
                                                        " kopieren nach ",fname))}
    )  
  }
  # Jetzt vom lokalen Laufwerk einlesen
  check = tryCatch(
    {
      tmp_df <-read_delim(fname, 
                           delim = ";", escape_double = FALSE, 
                           locale = locale(date_names = "de", 
                                           decimal_mark = ",", 
                                           grouping_mark = "."), 
                           trim_ws = TRUE, 
                           skip =1)
    },
    warning = function(w) {teams_warning(w,title=paste0(wahl_name,": Datenakquise - Warnung beim Einlesen von ",fname))},
    error = function(e) {teams_warning(e,title=paste0(wahl_name,": Datenakquise - Fehler beim Einlesen von ",fname))}
  )
  return(tmp_df)
}

hole_letztes_df <- function(a_directory = "daten") {
  #' Schaut im angegebenen Verzeichnis nach der zuletzt angelegten Datei
  #' und holt die Daten zurück in ein df
  if (!dir.exists(a_directory)) return(tibble())
  # Die zuletzt geschriebene Datei finden und einlesen
  neuester_file <- list.files(a_directory, full.names=TRUE) %>% 
    file.info() %>% 
    # Legt eine Spalte namens path an
    tibble::rownames_to_column(var = "path") %>% 
    arrange(desc(ctime)) %>% 
    head(1) %>% 
    # Pfad wieder rausziehen
    pull(path)
  if (length(neuester_file)==0) {
    # Falls keine Daten archiviert, gibt leeres df zurück
    return(tibble())
  } else {
    return(hole_daten(neuester_file))
  }
}

#--- CURL-Polling (experimentell!)
#
# Gibt das Änderungsdatum der Daten-Datei auf dem Wahlamtsserver zurück - 
# wenn es sich verändert hat, ist das das Signal, neue Daten zu holen. 
check_for_timestamp <- function(my_url) {
  # Erst checken: Wirklich eine Internet-Verbindung? 
  # Sonst behandle als lokale Datei. 
  if(str_detect(my_url,"^http")) {
    tmp <- curlGetHeaders(my_url, redirect = T, verify = F)
    # Redirect 
    if (stringr::str_detect(tmp[1]," 404")) {
      library(curl)
      h <- new_handle()
      # Das funktioniert, holt aber alle Daten -> hohe Last
      t <- curl_fetch_memory(my_url,handle=h)$modified %>% 
        as_datetime() + hours(1)
    } else {
      t <- tmp[stringr::str_detect(tmp,"last-modified")] %>%
        stringr::str_replace("last-modified: ","") %>%
        parse_date_time("%a, %d %m %Y %H:%M:%S",tz = "CET")
    }
  } else { # lokale Datei
    t = file.info(my_url)$ctime %>%  as_datetime
    print(t)
  }
  return(t)
}


#---- Lese-Funktionen ----

# Das hier ist die Haupt-Lese-Funktion - ein Wrapper für hole_daten
lies_stimmbezirke <- function(stand_url = stimmbezirke_url) {
  #' Versuche, Daten vom Wahlamtsserver zu lesen - und gib ggf. Warnung oder Fehler zurück
  #' Schreibt eine Meldung ins Logfile - zugleich ein Lesezeichen
  cat(as.character(now())," - Neue Daten lesen\n") # Touch logfile
  check = tryCatch(
    { 
      stand_df <- hole_daten(stimmbezirke_url)
    },
    warning = function(w) {teams_warning(w,title="OB-Wahl: Datenakquise")},
    error = function(e) {teams_warning(e,title="OB-Wahl: Datenakquise")})
  return(stand_df)
}

#' hole_kreise_direkt()
#' 
#' Funktion formatiert den Kreis-Direktwahlergebnis-Teil in eine lange Tabelle um,
#' ergänzt sie um Vergleichswerte von 2018, und gibt die Tabelle zurück. 
hole_kreise_direkt <- function(live_df) {
  # Vorbereitung: Bruda, mach Tabelle lang!
  
  #Liste mit den Kreisnamen
  live_kreise_direkt_long_df <- live_df %>% 
    filter(Gebietstyp == "WK") %>% 
    select(wk = 1,
           wk_name = 2, 
           wahlberechtigt = 6,
           waehler = 10,
           wahlbeteiligung = 12,
           gueltig = 15,
           ungueltig = 14,
           ungueltig_prozent = 17, 
           all_of(spaltenindex_direkt_df$idx)
    ) %>% 
    mutate(wk = as.integer(str_sub(wk,1,3))) %>% 
    pivot_longer(cols = 9:33,names_to="partei",values_to ="stimmen")   %>% 
    mutate(partei = str_replace(partei," Wahlkreisstimmen",""))%>% 
    # Ergänze die (Nach-)Namen der Direktkandidaten
    left_join(kandidaten_alle_df %>% select (wk,partei=Partei,Nachname,name),
               by=c("wk","partei"))  %>% 
    # Parteien ausfiltern
    filter(!is.na(name)) %>% 
    # Ergänze 2018er Ergebnisse aus der "Frankentabelle" (Kombination direkte und umgerechnete WK)
    
    left_join(frankentable_direkt_lang_df,by=c("wk","partei")) %>% 
    mutate(differenz = prozent - prozent_2018)
  return(live_kreise_direkt_long_df)
  # Filtere Kreise
}

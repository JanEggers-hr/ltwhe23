# lies_aktuellen_stand.R
#
# Enthält die Funktion zum Lesen der aktuellen Daten. 

#---- Hilfsfunktionen ----

hole_daten <- function(stand_url,a_directory = "daten", copy=TRUE) {
  #' Schreibt das Dataframe mit den zuletzt geholten Stimmbezirks-Daten
  #' als Sicherungskopie in das angegebene Verzeichnis
  #' 
  if (!dir.exists(a_directory)) {
    dir.create(a_directory)
  }
  if (copy) {
    fname = paste0(a_directory,"/",
                   str_replace_all(now(),"\\:","") %>% str_sub(1,15),
                   ".csv")
  } else {
    fname = "livedaten/tmp.csv"
    if (file.exists(fname)) { file.remove(fname)}
  }
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
  } else {
     R.utils::copyFile(stand_url,fname) 
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
  if (file.exists("livedaten/tmp.csv")) { file.remove("livedaten/tmp.csv")}
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
    # tmp <- curlGetHeaders(my_url, redirect = T, verify = F)
    # # Redirect 
    # if (stringr::str_detect(tmp[1]," 404")) {
    #   library(curl)
    #   h <- new_handle()
    #   # Das funktioniert, holt aber alle Daten -> hohe Last
    #   t <- curl_fetch_memory(my_url,handle=h)$modified %>% 
    #     as_datetime() + hours(1)
    # } else {
    #   t <- tmp[stringr::str_detect(tmp,"last-modified")] %>%
    #     stringr::str_replace("last-modified: ","") %>%
    #     parse_date_time("%a, %d %m %Y %H:%M:%S",tz = "CET")
    #
    # ersetzt durch supersimple Routine, die die erste Zeile der Datei liest und auswertet
    first_line <- readLines(my_url, n = 1)
    t <- first_line %>% str_extract("(?<=Stand\\: ).+$") %>% as_datetime(format = "%d.%m.%Y %H:%M:%S")
    print(t)
  } else { # lokale Datei
    t = file.info(my_url)$ctime %>%  as_datetime
    print(t)
  }
  return(t)
}


#---- Gelesene Daten filtern und aufbereiten ----

#' forme_hessen_landesstimmen
#' 
#' Nimmt die Livedaten und formt sie in die Lang-Tabelle mit 2018er Vergleichswerten um
forme_hessen_landesstimmen <- function(live_df){
  live_hessen_landesstimmen_lang_df <- live_df %>% 
    filter(Gebietstyp == "LD") %>% 
    select(wk = 1,
           wk_name = 2, 
           stimmbezirke = all_of(stimmbezirke_i),
           gezaehlt = all_of(gezaehlt_i), 
           wahlberechtigt = 6,
           waehler = 10,
           wahlbeteiligung = 12,
           gueltig = 15,
           ungueltig = 14,
           ungueltig_prozent = 17, 
           all_of(spaltenindex_landesstimmen_df$idx) 
    ) %>% 
    mutate(wk = as.integer(str_sub(wk,1,3))) %>% 
    pivot_longer(cols = 11:31,names_to="partei",values_to ="stimmen")  %>%
    mutate(across(c(5:10,12), ~ ifelse(is.na(.),0,.))) %>% 
    # Parteinamen korrigieren
    mutate(partei = str_replace(partei," Landesstimmen",""))%>% 
    # Prozentanteil errechnen
    mutate(prozent = ifelse (stimmen == 0,0,stimmen/gueltig*100)) %>% 
    left_join(frankentable_landesstimmen_lang_df %>%  
                filter(wk == 0) %>% 
                select(partei,prozent_2018),
              by="partei") %>% 
    mutate(prozent_2018 = ifelse(is.na(prozent_2018),0,prozent_2018)) %>% 
    mutate(differenz = ifelse(prozent==0,0,prozent-prozent_2018))
  return(live_hessen_landesstimmen_lang_df)
}    



#' forme_kreise_direkt()
#' 
#' Funktion formatiert den Kreis-Direktwahlergebnis-Teil in eine lange Tabelle um,
#' ergänzt sie um Vergleichswerte von 2018, und gibt die Tabelle zurück. 
forme_kreise_direkt <- function(live_df) {
  # Vorbereitung: Bruda, mach Tabelle lang!
  
  #Liste mit den Kreisnamen
  live_kreise_direkt_lang_df <- live_df %>% 
    filter(Gebietstyp == "WK") %>% 
    select(wk = 1,
           wk_name = 2, 
           stimmbezirke = all_of(stimmbezirke_i),
           gezaehlt = all_of(gezaehlt_i), 
           wahlberechtigt = 6,
           waehler = 10,
           wahlbeteiligung = 12,
           gueltig = 15,
           ungueltig = 14,
           ungueltig_prozent = 17, 
           all_of(spaltenindex_direkt_df$idx)
    ) %>% 
    mutate(wk = as.integer(str_sub(wk,1,3))) %>% 
    pivot_longer(cols = 11:35,names_to="partei",values_to ="stimmen")  %>%
    mutate(across(c(5:10,12), ~ ifelse(is.na(.),0,.))) %>% 
    # Parteinamen korrigieren
    mutate(partei = str_replace(partei," Wahlkreisstimmen",""))%>% 
    # Prozentanteil errechnen
    mutate(prozent = ifelse (stimmen == 0,0,stimmen/gueltig*100)) %>% 
    # Ergänze die (Nach-)Namen der Direktkandidaten
    left_join(direktkandidaten_df %>% select (wk,partei,Nachname,name),
               by=c("wk","partei"))  %>% 
    # Parteien ausfiltern
    filter(!is.na(name)) %>% 
    # Ergänze 2018er Ergebnisse aus der "Frankentabelle" (Kombination direkte und umgerechnete WK)
    
    left_join(frankentable_direkt_lang_df,by=c("wk","partei")) %>% 
    group_by(wk) %>% 
    fill(16:20) %>% 
    ungroup() %>% 
    mutate(across(c(stimmen_2018,prozent_2018), ~ ifelse(is.na(.),0,.)))   %>% 
    mutate(differenz = ifelse(prozent == 0,0,prozent - prozent_2018))
  return(live_kreise_direkt_lang_df)
  # Filtere Kreise
}

#' forme_kreise_landesstimmen
#' 
#' Funktion formatiert den Kreis-Direktwahlergebnis-Teil in eine lange Tabelle um,
#' ergänzt sie um Vergleichswerte von 2018, und gibt die Tabelle zurück. 
forme_kreise_landesstimmen <- function(live_df) {
  # Vorbereitung: Bruda, mach Tabelle lang!
  
  #Liste mit den Kreisnamen
  live_kreise_landesstimmen_lang_df <- live_df %>% 
    filter(Gebietstyp == "WK") %>% 
    select(wk = 1,
           wk_name = 2, 
           stimmbezirke = all_of(stimmbezirke_i),
           gezaehlt = all_of(gezaehlt_i), 
           wahlberechtigt = 6,
           waehler = 10,
           wahlbeteiligung = 12,
           gueltig = 15,
           ungueltig = 14,
           ungueltig_prozent = 17, 
           all_of(spaltenindex_landesstimmen_df$idx)
    ) %>% 
    mutate(wk = as.integer(str_sub(wk,1,3))) %>% 
    pivot_longer(cols = 11:31,names_to="partei",values_to ="stimmen")  %>%
    mutate(across(c(5:10,12), ~ ifelse(is.na(.),0,.))) %>% 
    # Parteinamen korrigieren
    mutate(partei = str_replace(partei," Landesstimmen",""))%>% 
    # Prozentanteil errechnen
    mutate(prozent = ifelse (stimmen == 0,0,stimmen/gueltig*100)) %>% 
  # Ergänze 2018er Ergebnisse aus der "Frankentabelle" (Kombination direkte und umgerechnete WK)
  left_join(frankentable_landesstimmen_lang_df,by=c("wk","partei")) %>% 
    group_by(wk) %>% 
    fill(wahlberechtigte_2018:veraendert) %>% 
    ungroup() %>% 
    mutate(across(c(stimmen_2018,prozent_2018), ~ ifelse(is.na(.),0,.)))   %>% 
  return(live_kreise_landesstimmen_lang_df)
  # Filtere Kreise
}


#' forme_gemeinden_direkt
#' 
#' Für alle Gemeinden aus den Livedaten eine Tabelle
forme_gemeinden_direkt <- function(live_df) {
  #Liste mit den Kreisnamen
  live_gemeinden_direkt_lang_df <- live_df %>%
    # Städte UND Gemeinden - THEORETISCH. Tatsächlich: Keine Direktkandidaten in den KS
    filter(Gebietstyp %in% c("VF","KS")) %>% 
    #.Kleine Schwierigkeit mit Offenbach: Hat wk 0 - wie alle kreisfreien Städte
    # (die ja idR mehrere Wahlkreise umfassen)
    # Entweder die Namen aus der gemeinden_alle nach AGS reinjoinen, oder
    # Sonderbedingung für Offenbach. 
    select(wk = 1,
           g_name = 2, 
           stimmbezirke = all_of(stimmbezirke_i),
           gezaehlt = all_of(gezaehlt_i), 
           wahlberechtigt = 6,
           waehler = 10,
           wahlbeteiligung = 12,
           gueltig = 15,
           ungueltig = 14,
           ungueltig_prozent = 17, 
           all_of(spaltenindex_direkt_df$idx)
    ) %>% 
    pivot_longer(cols = 11:35,names_to="partei",values_to ="stimmen") %>%
    # NA in den Stimmenzähl-Spalten und den Parteistimmen-Spalten tilgen
    mutate(across(c(5:10,12), ~ ifelse(is.na(.),0,.))) %>% 
    # AGS und Wahlkreis aus dem Gebietsschlüssel extrahieren
    mutate(AGS = str_sub(wk,4,9)) %>% 
    mutate(wk = as.integer(str_sub(wk,1,3))) %>%
    # Offenbach korrigieren
    mutate(wk = ifelse(AGS == "413000",43,wk)) %>% 
    # Namen aus der Namenstabelle überschreibt "Hans-Stadten-Stadt" etc.
    left_join(gemeinden_alle_df %>% distinct(AGS,name), by="AGS") %>% 
    mutate(g_name = name) %>% 
    select(-name) %>% 
    # Parteinamen extrahieren - mit V3-Partei!
    mutate(partei = str_replace(partei," Wahlkreisstimmen",""))%>% 
    # Prozentanteil errechnen
    mutate(prozent = ifelse (stimmen == 0,0,stimmen/gueltig*100)) %>% 
    # Ergänze die (Nach-)Namen der Direktkandidaten
    left_join(direktkandidaten_df %>% select (wk,partei,Nachname,name),
              by=c("wk","partei"))%>% 
    # Parteien ausfiltern
    filter(!is.na(name)) %>% 
    # Ergänze 2018er Ergebnisse aus der "Frankentabelle" (Kombination direkte und umgerechnete WK)
    left_join(frankentable_direkt_lang_df,by=c("wk","partei")) %>% 
    group_by(wk) %>% 
    fill(wahlberechtigte_2018:veraendert) %>% 
    ungroup() %>% 
    mutate(across(c(stimmen_2018,prozent_2018), ~ ifelse(is.na(.),0,.)))   %>% 
    mutate(differenz = ifelse(prozent == 0,0,prozent - prozent_2018))
  return(live_gemeinden_direkt_lang_df)
  # Filtere Kreise
  
}

#' forme_gemeinden_landesstimmen
#' 
#' Für alle Gemeinden aus den Livedaten eine Tabelle
forme_gemeinden_landesstimmen <- function(live_df) {
  #Liste mit den Kreisnamen
  live_gemeinden_landesstimmen_lang_df <- live_df %>%
    # Städte UND Gemeinden!
    filter(Gebietstyp %in% c("VF","KS")) %>% 
    #.Kleine Schwierigkeit mit Offenbach: Hat wk 0 - wie alle kreisfreien Städte
    # (die ja idR mehrere Wahlkreise umfassen)
    # Entweder die Namen aus der gemeinden_alle nach AGS reinjoinen, oder
    # Sonderbedingung für Offenbach. 
    select(wk = 1,
           g_name = 2, 
           stimmbezirke = all_of(stimmbezirke_i),
           gezaehlt = all_of(gezaehlt_i), 
           wahlberechtigt = 6,
           waehler = 10,
           wahlbeteiligung = 12,
           gueltig = 15,
           ungueltig = 14,
           ungueltig_prozent = 17, 
           all_of(spaltenindex_landesstimmen_df$idx)
    ) %>% 
    # Nur die Parteien von den Landeslisten
    pivot_longer(cols = 11:31,names_to="partei",values_to ="stimmen")  %>%
    # NA in den Stimmenzähl-Spalten und den Parteistimmen-Spalten tilgen
    mutate(across(c(5:10,12), ~ ifelse(is.na(.),0,.))) %>% 
    # AGS und Wahlkreis aus dem Gebietsschlüssel extrahieren
    mutate(AGS = str_sub(wk,4,9)) %>% 
    mutate(wk = as.integer(str_sub(wk,1,3))) %>%
    # Offenbach korrigieren
    mutate(wk = ifelse(AGS == "413000",43,wk)) %>% 
    # Namen aus der Namenstabelle überschreibt "Hans-Stadten-Stadt" etc.
    left_join(gemeinden_alle_df %>% distinct(AGS,name), by="AGS") %>% 
    mutate(g_name = name) %>% 
    select(-name) %>% 
  # Parteinamen korrigieren
    mutate(partei = str_replace(partei," Landesstimmen",""))%>% 
    # Prozentanteil errechnen
    mutate(prozent = ifelse (stimmen == 0,0,stimmen/gueltig*100)) %>% 
    # Ergänze 2018er Ergebnisse aus der "Frankentabelle" (Kombination direkte und umgerechnete WK)
    left_join(frankentable_landesstimmen_lang_df,by=c("wk","partei")) %>% 
    group_by(wk) %>% 
    fill(wahlberechtigte_2018:veraendert) %>% 
    ungroup() %>% 
    mutate(across(c(stimmen_2018,prozent_2018), ~ ifelse(is.na(.),0,.)))   %>% 
    mutate(differenz = ifelse(prozent == 0,0,prozent - prozent_2018))
  return(live_gemeinden_landesstimmen_lang_df)
  # Filtere Kreise
  
}


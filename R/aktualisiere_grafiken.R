#' aktualisiere_grafiken.R
#' 
#' Enthält die Funktionen: 
#' 


#--- Grafikfunktionen ----
generiere_auszählungsbalken <- function(anz = gezaehlt,max_s = stimmbezirke_n,ts = ts) {
  fortschritt <- floor(anz/max_s*100)
  annotate_str <- paste0("Ausgezählt sind ",
                         # Container Fake-Balken
                         "<span style='height:24px;display: flex;justify-content: space-around;align-items: flex-end; width: 100%;'>",
                         # Vordere Pufferzelle 70px
                         "<span style='width:70px; text-align:center;'>",
                         anz,
                         "</span>",
                         # dunkelblauer Balken
                         "<span style='width:",
                         fortschritt,
                         "%; background:#002747; height:16px;'></span>",
                         # grauer Balken
                         "<span style='width:",
                         100-fortschritt,
                         "%; background:#CCC; height:16px;'></span>",
                         # Hintere Pufferzelle 5px
                         "<span style='width:5px;'></span>",
                         # Ende Fake-Balken
                         "</span>",
                         "<br>",
                         " von ",max_s,
                         " Stimmbezirken - ",
                         "<strong>Stand: ",
                         format.Date(ts, "%d.%m.%y, %H:%M Uhr"),
                         "</strong>"
  )
  
}

notes_text_auszaehlung <- function(anz = gezaehlt,max_s = stimmbezirke_n,ts = ts,...) {
  sd <- as_datetime(startdatum)
  if (ts >= sd) {
    return(paste0(...,generiere_auszählungsbalken(anz,max_s,ts)))
  } else {
    return(paste0(...,
                  "Auszählung beginnt am ",
                  format(sd,"%A, %d. %B %Y, %H:%M Uhr")))
  }
}

#---- Daten-Kopierfunktionen ----

# Kopiere Livedaten-Ordner in das Google Bucket
aktualisiere_bucket_alle <- function() {
  if (SERVER) {
    n <- now()
    system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/* gs://d.data.gcp.cloud.hr.de/livedaten/')
    copy_time <- now()-n
    return(copy_time)
  } else return(NA)
}

# Kopiere nur die Direktkandidaten der Kreise
aktualisiere_bucket_kreise_direkt <- function() {
  if (SERVER) {
    n <- now()
    system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/wk*_direkt.* gs://d.data.gcp.cloud.hr.de/livedaten/')
    copy_time <- now()-n
    return(copy_time)
  } else return(NA)
}


#---- Metadaten-Anpassungsfunktionen ----

#' metadaten_balken
#' 
#' Kopiert die Metadaten-Anpassungen aus der Parteien-Tabelle in die Balkengrafik, 
#' vor allem die Farben. 
#' 


#---- Grafiken einrichten ----

kreise_direkt_saeulen <- function() {
  for (wahlkreis in 1:55) {
    wk_str <- formatC(wahlkreis, width = 3,format="fg", flag="0")
    # Hole eine Grafik
    fname <- datawrapper_ids_df %>% filter(id == wk_str) %>% 
      pull(fname) %>% first()
    dw_id <- datawrapper_ids_df %>% filter(id == wk_str) %>% 
      pull(dw_id) %>% first()
    
    kand_df <- direktkandidaten_df %>% filter(wk == wahlkreis)
    farbliste <- setNames(as.list(kand_df$farbwert), 
                          kand_df$name)
    meta <- dw_retrieve_chart_metadata(dw_id)
    viz <- meta$content$metadata$visualize
    # Balkengrafik
    viz$`custom-colors` <- farbliste
    viz$`x-grid-format` <- "0.0%"
    viz$`custom-range`[[1]] <- "0"
    viz$`custom-range`[[2]] <- "50"
    
    # Visual-Metadaten hochladen
    dw_edit_chart(dw_id,visualize = viz)
    #
    title <- paste0("Wahlkreis ",wahlkreis," - ",
                    kand_df %>% pull(wk_name) %>% first,
                    ": Stimmen fürs Direktmandat")
    kand_str <- paste0(kand_df %>% tail(nrow(.)-5) %>% 
                         mutate(n = paste0(name,": 0,0%")) %>% pull(n),
                            collapse = ", ")
    # Metadaten anlegen
    forced_meta <- list()
    forced_meta[["title"]] <- title
    forced_meta[["describe"]][["intro"]] <- "Bisher gezählte Stimmanteile für das Direktmandat im Wahlkreis; fünf erste Kandidierende von der Liste"
    forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
    forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
    forced_meta[["annotate"]][["notes"]] <- paste0(ifelse(nchar(kand_str) > 0,
                                                   "Sonstige: ",
                                                   ""),
                                                   kand_str,"<br><br>",
    # hier später der Auszählungsbalken                                              
      "Auszählung beginnt am 8.10.2023, 18 Uhr")
    # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
    # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
    # (obwohl die eine ganz normale Liste ist)
    forced_meta_json <- toJSON(forced_meta,force=T)
    write(forced_meta_json,
          paste0("livedaten/",fname,".json"))
    
    # CSV anlegen
    # Im Prinzip: Eine leere Kandidatenliste für den Wahlkreis
    kand_list_df <- kand_df %>%
      mutate(prozent = 0.0) %>% 
      select(name,prozent) %>% 
      head(5)
    write_csv(kand_list_df,paste0("livedaten/",
                                  fname,
                                  ".csv"))
    
  }
  
}

  
#' gemeinde_direkt_tabelle()
#' 
#' Passt die Metadaten der Landesstimmen-Grafiken an:
#' - Farbtabelle für Barcharts in das Vorbild laden
#' - Vorbild-Visualize-Metadaten klonen
#' - Data-Daten klonen 
#' - Daten überschreiben
#' - Livedaten generieren
#' Die haben 3 Spalten (Partei, Veränderung, prozentplusminus)
gemeinde_direkt_tabelle <- function() {
  # Vorbild-Metadaten laden
  source_meta <- dw_retrieve_chart_metadata(dw_template4)
  viz <- source_meta$content$metadata$visualize
  # Farbliste mit allen Partei-IDs - ruhig die lange
  farbliste <- setNames(as.list(partei_idx_df$farbwert), 
                        parteien_idx_df$name)
  viz[["columns"]][["stimmen"]][["customColorBarBackground"]] <- farbliste
  viz[["columns"]][["stimmen"]][["customBarColorBy"]] <- "partei"
  gemeinden_v <- gemeinden_alle_df %>% 
    filter(!(AGS %in% staedte_v)) %>% 
             pull(AGS)
  for (g in gemeinden_v) {
    # Hole den "letzten" (also den zweiten) Eintrag zu dieser AGS
    fname <- datawrapper_ids_df %>% filter(id == g) %>% 
      pull(fname) %>% last()
    dw_id <- datawrapper_ids_df %>% filter(id == g) %>% 
      pull(dw_id) %>% last()
    gemeinde_name <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(name)
    wk <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(wk)
    wk_name <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(wk_name)
    
    # Visual-Metadaten hochladen
    old_metadata <- dw_retrieve_chart_metadata(dw_id) 
    # Neuen Listeneintrag mit den visualize-Metadaten unter der id generieren
    dat <- old_metadata$content$metadata$data
    # Metadaten überschreiben
    # Livedaten, URL in den Data-Zweig kopieren
    dat[["upload-method"]]= "external-data"
    dat[["external-data"]] = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                    fname,
                                    ".csv")
    dat[["external-metadata"]] = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                        fname,
                                        ".json")
    dat[["use-datawrapper-cdn"]] = FALSE
    # 
    dw_edit_chart(dw_id, data = dat, visualize = viz)

    
    title <- paste0(gemeinde_name, ": Landesstimmen")
    intro <- paste0("Zweitstimmen in ",gemeinde_name,", alle Wahllisten, in der Reihenfolge vom Wahlzettel. ",
                    "Werte in Klammern geben die Differenz zur letzten Wahl 2018 an.")
    # Metadaten anlegen
    forced_meta <- list()
    forced_meta[["title"]] <- title
    forced_meta[["describe"]][["intro"]] <- intro
    forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
    forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
    forced_meta[["annotate"]][["notes"]] <- notes_text_auszaehlung(0,0,as_datetime(startdatum) - days(1))
      
    # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
    # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
    # (obwohl die eine ganz normale Liste ist)
    forced_meta_json <- toJSON(forced_meta,force=T)
    write(forced_meta_json,
          paste0("livedaten/",fname,".json"))
    
    # CSV anlegen
    # Im Prinzip: Eine leere Parteienliste für den Wahlkreis
    part_list_df <- parteien_listen_df %>%
      mutate(stimmen = 0) %>%
      mutate(prozent = "0,0% (+0)") %>% 
      mutate(wk = wk) %>% 
      select(partei,stimmen,prozent,wk) 
    write_csv(kand_list_df,paste0("livedaten/",
                                  fname,
                                  ".csv"))
    #---------- Letzte Aktion: Neu publizieren ----
    dw_publish_chart(dw_id)
  }
}


# Lies s


#

#' copy_visuals 
#' 
#' @description 
#' Kopiert die visual-Metadaten von einer Vorlage-Grafik auf alle anderen in der Liste.
#' Sichert die überschriebenen Visuals in einer Liste - und gibt die zurück.
copy_visuals <- function(dw_source,dw_id_v) {
  # Vorbild-Grafik auslesen
  source_meta <- dw_retrieve_chart_metadata(dw_source)
  meta_backup <- list()
  # visualize-Zweig extrahieren
  vis <- source_meta$content$metadata$visualize
  for (id in dw_id_v) {
    old_metadata <- dw_retrieve_chart_metadata(id) 
    # Neuen Listeneintrag mit den visualize-Metadaten unter der id generieren
    meta_backup[[id]] <- old_metadata$content$metadata$visualize
    # Metadaten überschreiben
    dw_edit_chart(id, visualize = vis)
  }
  return(meta_backup)
}

#' copy_fix_data 
#' 
#' @description 
#' Holt sich die Dateinamen für die externen Quellen und überschreibt die Einstellungen
#' so, dass das externe CSV / Metadaten-JSON gezogen wird. 
fix_data <- function(dw_id_v) {
  for (did in dw_id_v) {
    old_metadata <- dw_retrieve_chart_metadata(did) 
    # Neuen Listeneintrag mit den visualize-Metadaten unter der id generieren
    dat <- old_metadata$content$metadata$data
    # fname aus Tabelle holen
    fname <- datawrapper_ids_df %>% filter(dw_id == did) %>% pull(fname)
    # Metadaten überschreiben
    # Livedaten, URL in den Data-Zweig kopieren
    dat[["upload-method"]]= "external-data"
    dat[["external-data"]] = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                    fname,
                                    ".csv")
    dat[["external-metadata"]] = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                              fname,
                                              ".json")
    dat[["use-datawrapper-cdn"]] = FALSE
    # 
    dw_edit_chart(did, data = dat)
    dw_publish_chart(did)
  }
  return(TRUE)
}



write_meta_json <- function(fname,title,intro,notes) {
  # Metadaten anlegen
  forced_meta <- list()
  forced_meta[["title"]] <- new_dw$content$title
  forced_meta[["describe"]][["intro"]] <- new_dw$content$metadata$describe$intro
  forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
  forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
  forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
  forced_meta[["annotate"]][["notes"]] <- "Auszählung beginnt am 8.10.2023, 18 Uhr"
  # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
  # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
  # (obwohl die eine ganz normale Liste ist)
  forced_meta_json <- toJSON(forced_meta,force=T)
  write(forced_meta_json,
        paste0("livedaten/",fname,".json"))
}

write_data_csv <- function(fname,data)
#---- Hauptfunktionen ----


#' aktualisiere_kreise_direkt
#' 
#' Erwartet ein langes Dataframe mit den Ergebnissen nach Partei
#' und kopiert 
aktualisiere_kreise_direkt <- function(live_long_df, wk_v = c(1:55)) {
  # Gehe durch die Wahlkreis-IDs und suche die passenden Wahlkreisdaten
  for (i in wk_v) {
    wahlkreis_df <- live_long_df %>% filter()
  }
}

aktualisiere_kreise_landesstimmen <- function(live_df) {
  
}

aktualisiere_gemeinden_direkt <- function(live_df) {
  
}

aktualisiere_gemeinden_landesstimmen <- function(live_df) {
  
}

aktualisiere_staedte_landesstimmen <- function(live_df){
  
}

aktualisiere_hessen_landesstimmen <- function(live_df){
  
}










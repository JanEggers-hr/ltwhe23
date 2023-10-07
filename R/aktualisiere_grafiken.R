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

notes_text_auszaehlung <- function(gezaehlt = gezaehlt,stimmbezirke = stimmbezirke_n,ts = ts,...) {
  sd <- as_datetime(startdatum)
  if (ts >= sd) {
    return(paste0(...,generiere_auszählungsbalken(anz=gezaehlt,
                                                  max_s = stimmbezirke,
                                                  ts)))
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

#' kreise_direkt_saeulen
#' 
#' Richtet die Direktkandidaten-Grafiken für alle 55 Kreise ein
#' Die brauche zwei Spalten in den Daten: 
#' - name (der Langname mit Partei)
#' - prozent (die Prozentzahl)
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

  
#' gemeinden_landesstimme_tabelle()
#' 
#' Passt die Metadaten der Landesstimmen-Grafiken an:
#' - Farbtabelle für Barcharts in das Vorbild laden
#' - Vorbild-Visualize-Metadaten klonen
#' - Data-Daten klonen 
#' - Daten überschreiben
#' - Livedaten generieren
#' Die haben 3 Spalten (Partei, Veränderung, prozentplusminus)
gemeinden_landesstimme_tabelle <- function() {
  # Vorbild-Metadaten laden
  source_meta <- dw_retrieve_chart_metadata(dw_template4)
  viz <- source_meta$content$metadata$visualize
  # Farbliste mit allen Partei-IDs - ruhig die lange
  farbliste <- setNames(as.list(parteien_idx_df$farbwert), 
                        parteien_idx_df$partei)
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
    # Spalte wk aus den Daten ausblenden
    dat[["column-format"]][["wk"]][["ignore"]] <- TRUE
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
    write_csv(part_list_df,paste0("livedaten/",
                                  fname,
                                  ".csv"))
    #---------- Letzte Aktion: Neu publizieren ----
    dw_publish_chart(dw_id)
  }
}

#' staedte_landesstimme_tabelle()
#' 
#' Im Prinzip völlig identisch, nur eben für die vier Städte
#' Die haben 3 Spalten (Partei, Veränderung, prozentplusminus)
staedte_landesstimme_tabelle <- function() {
  # Vorbild-Metadaten laden
  source_meta <- dw_retrieve_chart_metadata(dw_template4)
  viz <- source_meta$content$metadata$visualize
  # Farbliste mit allen Partei-IDs - ruhig die lange
  farbliste <- setNames(as.list(parteien_idx_df$farbwert),
                        parteien_idx_df$partei)
  viz[["columns"]][["stimmen"]][["customColorBarBackground"]] <- farbliste
  viz[["columns"]][["stimmen"]][["customBarColorBy"]] <- "partei"
  gemeinden_v <- staedte_v
  for (g in gemeinden_v) {
    # Hole den "letzten" (also den zweiten) Eintrag zu dieser AGS
    fname <- datawrapper_ids_df %>% filter(id == g) %>%
      pull(fname) %>% last()
    dw_id <- datawrapper_ids_df %>% filter(id == g) %>%
      pull(dw_id) %>% last()
    gemeinde_name <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(name) %>% first()
    # alle Wahlkreise!
    wk <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(wk)
    #wk_name <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(wk_name) %>% first()

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
    intro <- paste0("Zweitstimmen in ",gemeinde_name,", alle Wahlkreise (",
                    paste0(wk,collapse=", "),") -",
                      " Alle Wahllisten, in der Reihenfolge vom Wahlzettel. ",
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
      mutate(wk = first(wk)) %>%
      select(partei,stimmen,prozent,wk)
    write_csv(part_list_df,paste0("livedaten/",
                                  fname,
                                  ".csv"))
    #---------- Letzte Aktion: Neu publizieren ----
    dw_publish_chart(dw_id)
  }
}

#' gemeinde_direkt_tabelle
#' 
#' Direktstimmen in der jeweiligen Gemeinde - im Prinzip nicht so viel anders 
#' als die Landesstimmen. 
#' Enthalten zusätzlich zu partei den (Kombi-)name des Kandidaten,
#' die Stimmen und die Prozente/Veränderung. Die Partei-Spalte ist in der Anzeige
#' ausgeblendet. 
gemeinden_direkt_tabelle <- function() {
  # Vorbild-Metadaten laden
  source_meta <- dw_retrieve_chart_metadata(dw_template2)
  viz <- source_meta$content$metadata$visualize
  # Farbliste mit allen Partei-IDs - ruhig die lange
  farbliste <- setNames(as.list(parteien_idx_df$farbwert), 
                        parteien_idx_df$name)
  viz[["columns"]][["stimmen"]][["customColorBarBackground"]] <- farbliste
  viz[["columns"]][["stimmen"]][["customBarColorBy"]] <- "partei"
  gemeinden_v <- gemeinden_alle_df %>%
    # Städte raus (weil: mehrfacher Wahlkreis)
    filter(!AGS %in% staedte_v) %>% 
    pull(AGS) %>% unique()
  for (g in gemeinden_v) {
    # Hole den ersten Eintrag zu dieser AGS - direkt
    fname <- datawrapper_ids_df %>% filter(id == g) %>% 
      pull(fname) %>% first()
    dw_id <- datawrapper_ids_df %>% filter(id == g) %>% 
      pull(dw_id) %>% first()
    gemeinde_name <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(name)
    wk <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(wk)
    wk_name <- gemeinden_alle_df %>% filter(AGS == g) %>% pull(wk_name)
    wahlkreis <- wk
    # Hole die Kandidaten

    
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
    
    
    title <- paste0(gemeinde_name, ": Stimmen fürs Direktmandat")
    intro <- paste0("Erststimmen in ",gemeinde_name," für die Wahl des Direktkandidaten des Wahlkreises ", wk)
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
    kand_df <- direktkandidaten_df %>% filter(wk == wahlkreis) %>% 
      mutate(stimmen = 0) %>%
      mutate(prozent = "0,0% (+0)") %>% 
      select(name,partei,stimmen,prozent) 
    write_csv(kand_df,paste0("livedaten/",
                                  fname,
                                  ".csv"))
    #---------- Letzte Aktion: Neu publizieren ----
    dw_publish_chart(dw_id)
  }
}

#---- Kopier- und Hilfsroutinen ----
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

#' fix_data 
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

fix_dwcdn <- function(id_v) {
  for (id in id_v) {
    meta <- dw_retrieve_chart_metadata(id)
    dat <- meta$content$metadata$data
    dat$`use-datawrapper-cdn`<- FALSE
    dw_edit_chart(chart_id = id, data = dat)
  }
}

fix_publishes <- function(id_v) {
  for (id in id_v) {
    meta <- dw_retrieve_chart_metadata(id)
    pub <- meta$content$metadata$pub
    pub$blocks$blocks$`get-the-data` <- TRUE
    pub$blocks$enabled <- TRUE
    pub$blocks$`download-image` <- TRUE
    dw_edit_chart(chart_id = id, publish =  pub)
  }
}

fix_hide_wk <- function(id_v) {
  for (id in id_v) {
    meta <- dw_retrieve_chart_metadata(id)
    dat <- meta$content$metadata$data
    data$`column-format`$wk$ignore <- TRUE
    dw_edit_chart(chart_id = id, data = dat)
  }
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

#---- Hauptfunktionen ----


#' aktualisiere_kreise_direkt
#' 
#' Erwartet ein langes Dataframe mit den Ergebnissen nach Partei
#' und kopiert die Daten zusammen. 
#' 
#' Die Daten sind hier sehr schlicht: Spalten name, prozent
#' Metadaten ergänzen
aktualisiere_kreise_direkt <- function(live_kreise_direkt_lang_df, wk_v = c(1:55)) {
  lkdl_df <- live_kreise_direkt_lang_df
  # Gehe durch die Wahlkreis-IDs und suche die passenden Wahlkreisdaten
  for (i in wk_v) {
    wahlkreis_df <- lkdl_df %>% 
      filter(wk == i) %>% 
      arrange(desc(prozent))
    # Stimmbezirke gesamt und bisher gezählt
    # Spalte 142 und 143
    stimmbezirke <- wahlkreis_df %>% pull(stimmbezirke) %>% first()
    gezaehlt <- wahlkreis_df %>% pull(gezaehlt) %>% first()
    wk <- i
    wk_name <- wahlkreis_df %>% pull(wk_name) %>% first()
    fname <- datawrapper_ids_df %>%  
      filter(typ == "w") %>% 
      filter(as.integer(id) == i) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      first()
    kand_df <- wahlkreis_df %>% 
      filter(wk == i) %>% 
      select(name,
             prozent) 
    write_csv(kand_df %>% head(5),paste0("livedaten/",
                             fname,
                             ".csv"))
    # Metadaten einrichten: 
    kand_str <- paste0(kand_df %>% tail(nrow(.)-5) %>% 
                         mutate(n = paste0(name,": 0,0%")) %>% pull(n),
                       collapse = ", ")
    title <- paste0("Wahlkreis ",wk,
                    " - ",wk_name,
                    ": Stimmen fürs Direktmandat",
                    ifelse(gezaehlt == stimmbezirke,""," - TREND"))
    intro <- paste0("Erststimmen",
                    " für die Wahl des Direktkandidaten des Wahlkreises ")
    notes <- notes_text_auszaehlung(gezaehlt,
                                    stimmbezirke, 
                                    ts,
                                    kand_str)
    # TEST - Feature
    if (TEST) {
      # Text mit Hintergrundfarbe
      intro = paste0("<b style='color:#dfedf8;'>TITLE ",title,"<br><br>INTRO ",intro,"</b>")
      title = paste0("*** TEST *** - FIKTIVE DATEN wk ",wk)
    }
    # Metadaten anlegen
    forced_meta <- list()
    forced_meta[["title"]] <- title
    forced_meta[["describe"]][["intro"]] <- intro
    forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
    forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
    forced_meta[["annotate"]][["notes"]] <- notes
    # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
    # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
    # (obwohl die eine ganz normale Liste ist)
    forced_meta_json <- toJSON(forced_meta,force=T)
    write(forced_meta_json,
          paste0("livedaten/",fname,".json"))
    
  }
}

#' aktualisiere_kreise_landesstimmen
#' 
#' Grafik-Generierung Tabelle Kreise Landesstimmen. Ausgabespalten: 
#' partei,stimmen,prozent,wk (Wahklreis aber ausgeblendet)
aktualisiere_kreise_landesstimmen <- function(live_kreise_landesstimmen_lang_df) {
  lkll_df <- live_kreise_landesstimmen_lang_df
  lkll_v <- lkll_df %>% pull(wk) %>% unique()
  for (w in lkll_v) {
    # Einen Kreis
    kreis_df <- lkll_df %>% 
      filter(wk == w) %>%
      # Auf die Parteien beschränken, bei denen es Kandidaten gibt im Kreis
      filter(partei %in% parteien_listen_df$partei) %>% 
      mutate(differenz = prozent - prozent_2018)
    # Stimmbezirke gesamt und bisher gezählt
    # Spalte 142 und 143
    stimmbezirke <- kreis_df %>% pull(stimmbezirke) %>% first()
    gezaehlt <- kreis_df %>% pull(gezaehlt) %>% first()
    # Dran denken: Bei Städten ist es mehr als einer, 
    wk <- w 
    wk_name <- kreis_df %>% pull(wk_name) %>% unique()
    veraendert <- kreis_df %>% pull(veraendert) %>% unique()
    fname <- datawrapper_ids_df %>%  
      filter(typ == "w") %>% 
      filter(as.integer(id) == w) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      first()
    liste_df <- kreis_df %>% 
      mutate(prozent = paste0(formatC(prozent,digits=1,format="f", 
                                      big.mark = ".",decimal.mark = ","),
                              " (",
                              formatC(differenz,digits=1,format="f", 
                                      flag="+", big.mark = ".",decimal.mark = ","),
                              ")")) %>% 
      select(partei,stimmen,prozent,wk)
    write_csv(liste_df,paste0("livedaten/",
                              fname,
                              ".csv"))
    # Metadaten einrichten: 
    title <- paste0("Wahlkreis ",wk,
                    " - ",wk_name,
                    ": Landesstimmen",
                    ifelse(stimmbezirke == gezaehlt,""," - TREND"))
    intro <- paste0("Zweitstimmen im Wahlkreis für die Parteien im neuen Landtag",
                    " (in Klammern: Ergebnis 2018, soweit vorhanden)")
    notes <- notes_text_auszaehlung(gezaehlt,
                                    stimmbezirke, 
                                    ts,
                                    "in der Reihenfolge vom Stimmzettel<br><br>",
                                    ifelse(veraendert & !(wk %in% c(34,36)),
                                              "Ergebnisse 2018 umgerechnet auf den aktuellen, veränderten Wahlkreiszuschnitt<br><br>",
                                              ""),
                                    ifelse(wk %in% c(34,36),
                                           "Ergebnisse 2018 für damaligen Wahlkreis; Umrechnung auf den neuen, veränderten Wahlkreiszuschnitt aus technischen Gründen nicht möglich<br><br>",
                                           ""))
    # TEST - Feature
    if (TEST) {
      # Text mit Hintergrundfarbe
      intro = paste0("<b style='color:#dfedf8;'>TITLE ",title,"<br><br>INTRO ",intro,"</b>")
      title = paste0("*** TEST *** - FIKTIVE DATEN wk ",wk)
    }
    # Metadaten anlegen
    forced_meta <- list()
    forced_meta[["title"]] <- title
    forced_meta[["describe"]][["intro"]] <- intro
    forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
    forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
    forced_meta[["annotate"]][["notes"]] <- notes
    # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
    # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
    # (obwohl die eine ganz normale Liste ist)
    forced_meta_json <- toJSON(forced_meta,force=T)
    write(forced_meta_json,
          paste0("livedaten/",fname,".json"))
  }
}

#' aktualisiere_gemeinde_direkt
#' 
#' Grafik-Generierung Gemeinden. Ausgabespalten: 
#' Enthalten zusätzlich zu partei den (Kombi-)name des Kandidaten,
#' die Stimmen und die Prozente/Veränderung. Die Partei-Spalte ist in der Anzeige
#' ausgeblendet. 
aktualisiere_gemeinden_direkt <- function(live_gemeinden_direkt_lang_df) {
  lgdl_df <- live_gemeinden_direkt_lang_df
  # Gehe durch die Gemeinde-IDs 
  lgdl_v <- lgdl_df %>% pull(AGS) %>% unique()
  for (g in lgdl_v) {
    # Eine Gemeinde isolieren
    gemeinde_df <- lgdl_df %>% 
      filter(AGS == g)  
   # Stimmbezirke gesamt und bisher gezählt
    # Spalte 142 und 143
    stimmbezirke <- gemeinde_df %>% pull(stimmbezirke) %>% first()
    gezaehlt <- gemeinde_df %>% pull(gezaehlt) %>% first()
    # Dran denken: Bei Städten ist es mehr als einer, 
    wk <- gemeinde_df %>% pull(wk) %>% unique() 
    g_name <- gemeinde_df %>% pull(g_name) %>% unique()
    fname <- datawrapper_ids_df %>%  
      filter(typ == "g") %>% 
      filter(id == g) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      first()
    kand2_df <- gemeinde_df %>% 
      mutate(prozent = paste0(formatC(prozent,digits=1,format="f", 
                                     big.mark = ".",decimal.mark = ","),
                             " (",
                             formatC(differenz,digits=1,format="f", 
                                     flag="+", big.mark = ".",decimal.mark = ","),
                             ")")) %>% 
      select(name,partei,stimmen,prozent)
    write_csv(kand2_df,paste0("livedaten/",
                                         fname,
                                         ".csv"))
    # Metadaten einrichten: 
    title <- paste0(g_name,
                    ": Stimmen fürs Direktmandat",
                    ifelse(stimmbezirke == gezaehlt,""," - TREND"))
    intro <- paste0("Erststimmen in ",
                    g_name, 
                    " für die Direktkandidaten im Wahlkreis ",wk,
                    " (in Klammern: Ergebnis 2018, soweit vorhanden)")
    notes <- notes_text_auszaehlung(gezaehlt,
                                    stimmbezirke, 
                                    ts,"in der Reihenfolge vom Stimmzettel<br><br>")
    # TEST - Feature
    if (TEST) {
      # Text mit Hintergrundfarbe
      intro = paste0("<b style='color:#dfedf8;'>TITLE ",title,"<br><br>INTRO ",intro,"</b>")
      title = paste0("*** TEST *** - FIKTIVE DATEN wk ",wk)
    }
    # Metadaten anlegen
    forced_meta <- list()
    forced_meta[["title"]] <- title
    forced_meta[["describe"]][["intro"]] <- intro
    forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
    forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
    forced_meta[["annotate"]][["notes"]] <- notes
    # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
    # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
    # (obwohl die eine ganz normale Liste ist)
    forced_meta_json <- toJSON(forced_meta,force=T)
    write(forced_meta_json,
          paste0("livedaten/",fname,".json"))
    
  }
  
}

#' aktualisiere_gemeinden_landesstimmen
#'
#' Die haben 3 Spalten (Partei, Stimmen, Prozent); Wahlkreis ausgeblendet
aktualisiere_gemeinden_landesstimmen <- function(live_gemeinden_landesstimmen_lang_df) {
  lgll_df <- live_gemeinden_landesstimmen_lang_df
  # Gehe durch die Gemeinde-IDs 
  lgll_v <- lgll_df %>% pull(AGS) %>% unique()
  for (g in lgll_v) {
    # Eine Gemeinde isolieren
    gemeinde_df <- lgll_df %>% 
      filter(AGS == g) %>% 
      filter(partei %in% parteien_listen_df$partei)
    # Stimmbezirke gesamt und bisher gezählt
    # Spalte 142 und 143
    stimmbezirke <- gemeinde_df %>% pull(stimmbezirke) %>% first()
    gezaehlt <- gemeinde_df %>% pull(gezaehlt) %>% first()
    # Dran denken: Bei Städten ist es mehr als einer, 
    wk <- gemeinde_df %>% pull(wk) %>% unique() 
    g_name <- gemeinde_df %>% pull(g_name) %>% unique()
    print(g_name)
    fname <- datawrapper_ids_df %>%  
      filter(typ == "g") %>% 
      filter(id == g) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      first()
    list2_df <- gemeinde_df %>% 
      mutate(prozent = paste0(formatC(prozent,digits=1,format="f", 
                                      big.mark = ".",decimal.mark = ","),
                              " (",
                              formatC(differenz,digits=1,format="f", 
                                      flag="+", big.mark = ".",decimal.mark = ","),
                              ")")) %>% 
      select(partei,stimmen,prozent,wk)
    write_csv(list2_df,paste0("livedaten/",
                              fname,
                              ".csv"))
    # Metadaten einrichten: 
    title <- paste0(g_name,
                    ": Landesstimmen",
                    ifelse(stimmbezirke == gezaehlt,""," - TREND"))
    intro <- paste0("Zweitstimmen in ",g_name,
                    ", alle Wahllisten. ",
                    "Werte in Klammern geben die Differenz zur letzten Wahl 2018 an.")
    notes <- notes_text_auszaehlung(gezaehlt,
                                    stimmbezirke, 
                                    ts,"in der Reihenfolge vom Stimmzettel<br><br>")
    # TEST - Feature
    if (TEST) {
      # Text mit Hintergrundfarbe
      intro = paste0("<b style='color:#dfedf8;'>TITLE ",title,"<br><br>INTRO ",intro,"</b>")
      title = paste0("*** TEST *** - FIKTIVE DATEN wk ",wk)
    }
    # Metadaten anlegen
    forced_meta <- list()
    forced_meta[["title"]] <- title
    forced_meta[["describe"]][["intro"]] <- intro
    forced_meta[["describe"]][["byline"]] <- "Jan Eggers/Sandra Kiefer"
    forced_meta[["describe"]][["source-url"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-name"]] <- "https://wahlen.hessen.de/landtagswahlen"
    forced_meta[["annotate"]][["notes"]] <- notes
    # Liste in JSON - der force-Parameter ist nötig, weil R sonst darauf
    # beharrt, dass es mit der S3-Klasse dw_chart nichts anfangen kann
    # (obwohl die eine ganz normale Liste ist)
    forced_meta_json <- toJSON(forced_meta,force=T)
    write(forced_meta_json,
          paste0("livedaten/",fname,".json"))
    
  }
}

# aktualisiere_staedte_landesstimmen <- function(live_df){
#   
# }

#' aktualisiere_hessen_landesstimmen
#' 
#' Grafikausgabe Landesstimmen ganz Hessen (direkt über dw-Publish)
aktualisiere_hessen_landesstimmen <- function(lhll_df = live_hessen_landesstimmen_lang_df){
  stimmbezirke <- lhll_df %>% pull(stimmbezirke) %>% first()
  gezaehlt <- lhll_df %>% pull(gezaehlt) %>% first()
  hessen_df <- lhll_df %>% 
    mutate(plusminus = ifelse(prozent==0,"(.)",
                              paste0("(",
                                     formatC(prozent-prozent_2018,
                                             digits=1,format="f", flag="+",
                                             big.mark = ".",decimal.mark = ",")))) %>% 
    select(partei,prozent,plusminus)
  # Daten direkt hochladen
  dw_data_to_chart(hessen_df,chart_id = hessen_id)
  title = paste0("Hessen: Landessstimmen",ifelse(stimmbezirke == gezaehlt,""," - TREND"))
  intro = "Zweitstimmen für die Parteien landesweit; in Klammern die Stimmanteile bei der Landtagsahl 2018, soweit vorhanden. Reihenfolge der Parteien wie auf dem Stimmzettel."
  notes = notes_text_auszaehlung(gezaehlt,stimmbezirke,ts)
  # TEST-Feature
  if (TEST) {
    intro = paste0("<b style='color:#dfedf8;'>TITLE ",title,"<br><br>INTRO ",intro,"</b>")
    title ="*** TEST *** wk0 - FIKTIVE DATEN"
  }
  dw_edit_chart(chart_id = hessen_id,
                title = title,
                intro = intro,
                annotate = notes)
  dw_publish_chart(chart_id = hessen_id)
}










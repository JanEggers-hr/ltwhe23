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
  
  
 
# Landesstimmen-Grafiken
# Haben 3 Spalten (Partei, Veränderung, prozentplusminus)

# Lies s
source_meta <- dw_retrieve_chart_metadata("p6i6a")

viz[["columns"]][["stimmen"]][["customColorBarBackground"]]


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
#' Kopiert die data-Metadaten von einer Vorlage-Grafik auf alle anderen in der Liste.
#' Die sind vor allem für die Benennung und Verrechnung der Achsen nötig - aber haben
#' sich als problematisch herausgestellt, wenn man sie setzt. 
#' 
#' Überschriebene Data-Einstellungen werden in einer Liste/als JSON gesichert. 
#' 
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

aktualisiere_staedte_landesstimmen {
  
}

aktualisiere_hessen_landesstimmen {
  
}










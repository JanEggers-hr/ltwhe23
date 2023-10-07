# generiere_dw.R
#
# Legt alle Datawrapper-Karten an und speichert die JSON-Files mit den 
# Metadaten für die Live-Aktualisierung. 

# Namen der Grafiken: 
# - g06xxxxxx_direkt (Gemeinde, alle Wahlkreisstimmen)
# - g06xxxxxx_land (Gemeinde, alle Landesstimmen)
# - wk0xx_direkt (Wahlkreis, alle Wahlkreisstimmen)
# - wk0xx_land (Wahlkreis, alle Landesstimmen)

library(pacman)
p_load(dplyr)
p_load(stringr)
p_load(openxlsx)
p_load(jsonlite)
p_load(lubridate)
p_load(readr)

rm(list=ls())

# Aktuelles Verzeichnis als workdir
setwd(this.path::this.dir())
# Aus dem R-Verzeichnis eine Ebene rauf
setwd("..")

# Globale Parameter
ltw_folder_id = "144571" # Datawrapper-Ordner wahlen/ltwhe23
hsde_theme = "hessischer-rundfunk"
bucket_url = "https://d.data.gcp.cloud.hr.de/ltwhe23/"


require(DatawRappr)


#### Hilfsfunktionen zum Umgang mit den Kandidaten- und Listendaten
listen_df <- read.xlsx("index/listen.xlsx") 

direktkandidaten_df <- read.xlsx("index/kandidaten_alle.xlsx") %>% 
  filter(!is.na(wk)) %>% 
  rename(partei = Partei) %>% 
  left_join(listen_df %>% rename(p_id = id),by="partei") %>% 
  arrange(wk,p_id)

gemeinden_df <- read.xlsx("index/gemeinden_alle.xlsx") %>% 
  mutate(AGS = as.character(AGS)) %>% 
  filter(bevoelkerung > 0)

wahlkreisnamen_v <- gemeinden_df %>% select (wk,wk_name) %>% 
  distinct(wk, wk_name) %>% pull(wk_name)

ks_ags <- gemeinden_df %>% filter(Gebietstyp == "KS") %>% 
  # Offenbach rausnehmen und wie Gemeinde behandeln
  filter(!str_detect(name,"Offenbach")) %>% 
  pull (AGS) %>% unique()



#' dw_direkt_anlegen
#' 
#' @description Legt eine Säulengrafik der Direktstimmen für eine Gemeinde 
#' oder einen Wahlkreis in Datawrapper an, richtet die Farben und Namen ein, 
#' biegt die Verweise auf CSV- und JSON- Dateien im Google Bucket um (Live-Daten
#' und Live-Metadaten), und legt diese Daten im livedaten-Ordner an.
#' Zieht sich den Namen der Gemeinde aus der Tabelle gemeinden_df 
#'
#' @param id String mit der ID der Gemeinde oder des Wahlkreises
#' @param type description
#' 
#' @returns Datawrapper-ID der angelegten Karte
#' 
#' 
dw_direkt_anlegen <- function(id="001") {
  # Einzelfunktionen: Lege Wahlkreis, Stadt oder Gemeinde an
  # Gemeinde und Stadt unterscheiden sich nur durch den Titel
  w_direkt <- function(id) {
    id_i <- as.numeric(id)
    id <- formatC(id_i, width = 3,format="fg", flag="0")
    # id bereinigen
    title <- paste0("Wahlkreis ",id_i," - ",
                    wahlkreisnamen_v[id_i],
                    ": Stimmen fürs Direktmandat")
    intro <- "Die nach Auszählung führenden fünf Direktkandidaten, Prozentanteile"
    # Wahlkreiskarte: Säulengrafik der ersten fünf
    #
    # Datawrapper gibt ein Objekt dw_chart zurück - eigentlich eine Liste, 
    # die die ganzen Metadaten-Elemente in einer JSON-artigen Struktur enthält.
    # In Python wär's ein Dictionary of dictionaries. 
    new_dw <- dw_create_chart(title = title, 
                                type="column-chart",
                                folderId = ltw_folder_id,
                                theme = hsde_theme) 
    
    
    # Namen in die ANmerkung - erst mal
    new_dw$content$metadata$annotate$notes <- paste0("wk",id,"_direkt")
    
    return(new_dw)
  }
  # Wie gesagt: Gemeinde und Stadt nur unterschiedlich im Titel, 
  # deshalb den Wrapper 
  gs_direkt <- function(title) {
    # Der gemeinsame Teil von Gemeinden udn Städten
    intro <- "Ausgezählte Stimmen für die Direktkandidaten, in der Reihenfolge vom Wahlzettel"
    new_dw <- dw_create_chart(title = title, 
                              type="tables",
                              folderId = ltw_folder_id,
                              theme = hsde_theme)
    # Namen in die ANmerkungen - erst mal
    new_dw$content$metadata$annotate$notes <- paste0("g06",id,"_direkt")
    # Jetzt die Schlüssel verändern, die verändert werden sollen: 
    return(new_dw)
  }
  g_direkt <- function(id) {
    name <- gemeinden_df %>% filter(id == AGS) %>% 
      pull(name) %>% first()
    title <-paste0(name,": Stimmen fürs Direktmandat")
    return(gs_direkt(title))
  }
  s_direkt <- function(id) {
    name <- gemeinden_df %>% filter(id == AGS) %>% 
      pull(name) %>% first()
    title <- paste0("Kreisfreie Stadt ",name,
                    ": Stimmen fürs Direktmandat")
    return(gs_direkt(title))
  }
  
  # Let's get the party started!
  id_i <- as.integer(id)
  if (id_i > 0 & id_i < 56) {
    # Wahlkreis?
    new_dw <- w_direkt(id)
  } else {
    # G oder S? 
    if (id %in% ks_ags) {
      new_dw <- s_direkt(id)
    } else {
      # Gemeinde - checken, ob bekannt, sonst stop
      if (!id %in% gemeinden_df$AGS) stop("Unbekannte ID ",id)
      new_dw <- g_direkt(id)
    }
  }

  # EIn paar allgemeine Dinge einrichten - mit der noch leeren Grafik
  # ... und unter Publish verstecken sich die Publikations-Optionen
  #
  # Alle Daten, Datenauswahl-, Farb- und Benennungs-Metadaten 
  # werden gesondert angepasst. (Das muss ja ggf. auch live gehen!)
  publish <- list()
  publish$blocks$enabled <- TRUE
  publish$blocks$embed <- TRUE
  publish$blocks[["blocks"]][["get-the-data"]] <- TRUE
  describe <- list()
  describe[["source-name"]] <- "Hessisches Statistisches Landesamt"
  describe[["source-url"]] <- "https://wahlen.hessen.de/landtagswahlen"
  describe[["byline"]] <- "Jan Eggers/Sandra Kiefer, hr" 
  dw_edit_chart(chart_id = new_dw$id,
                annotate = "Auszählung beginnt am 8.10.2023 um 18 Uhr",
#                describe = describe,
                byline = "Jan Eggers/Sandra Kiefer, hr",
                publish = publish
                )
  # Kleine Fiesheit: einige der Metadaten-Keys, z.B. annotate$notes, 
  # weichen jetzt in der Grafik von den zurückgegebenen ab. Stört aber nicht
  # weiter, weil sie ohnehin überschrieben werden. 
  return(new_dw)
}
  
#--- Funktion Landesstimmen (alles: Tabellen) ----  
dw_landesstimmen_anlegen <- function(id="001") {
  # Einzelfunktionen: Lege Wahlkreis, Stadt oder Gemeinde an
  # Gemeinde und Stadt unterscheiden sich nur durch den Titel
  w_landesstimmen <- function(id) {
    id_i <- as.numeric(id)
    id <- formatC(id_i, width = 3,format="fg", flag="0")
    # id bereinigen
    title <- paste0("Wahlkreis ",id_i," - ",
                    wahlkreisnamen_v[id_i],
                    ": Landesstimmen")
    intro <- "Alle Stimmen für die Parteien im neuen Landtag, in der Reihenfolge vom Stimmzettel"
    new_dw <- dw_create_chart(title = title, 
                              type="tables",
                              folderId = ltw_folder_id,
                              theme = hsde_theme) 
    
    
    # Namen in die ANmerkung - erst mal
    new_dw$content$metadata$annotate$notes <- paste0("wk",id,"_landesstimmen")
    new_dw$content$metadata$describe$intro <- intro
    
    return(new_dw)
  }
  # Wie gesagt: Gemeinde und Stadt nur unterschiedlich im Titel, 
  # deshalb den Wrapper 
  gs_landesstimmen <- function(title) {
    # Der gemeinsame Teil von Gemeinden udn Städten
    new_dw <- dw_create_chart(title = title, 
                              type="tables",
                              folderId = ltw_folder_id,
                              theme = hsde_theme)
    new_dw$content$metadata$annotate$notes <- paste0("g06",id,"_landesstimmen")
   return(new_dw)
  }
  g_landesstimmen <- function(id) {
    name <- gemeinden_df %>% filter(id == AGS) %>% 
      pull(name) %>% first()
    title <-paste0(name,": Landesstimmen")
    new_dw <- gs_landesstimmen(title)
    # Jetzt die Schlüssel verändern, die verändert werden sollen:
    new_dw$content$metadata$describe$intro <- paste0(
      "Stimmen für die Parteien im neuen Landtag, ",
      "in der Reihenfolge vom Wahlzettel"
    )

    return(new_dw)
  }
  s_landesstimmen <- function(id) {
    name <- gemeinden_df %>% filter(id == AGS) %>% 
      pull(name) %>% first()
    title <- paste0("Kreisfreie Stadt ",name,
                    ": Landesstimmen für die Parteien")
    new_dw <- gs_landesstimmen(title)
    wahlkreise_stadt_v <- gemeinden_df %>% filter(id == AGS) %>% 
      arrange(wk) %>% pull(wk) %>% unique()
    new_dw$content$metadata$describe$intro <- paste0("Stimmen für die Parteien ",
                    "im neuen Landtag; alle Wahlkreise (",
                    paste(wahlkreise_stadt_v[1:length(wahlkreise_stadt_v)-1]),
                    last(wahlkreise_stadt_v),") zusammen")
    return(new_dw)
  }
  
  # Let's get the party started!
  id_i <- as.integer(id)
  if (id_i > 0 & id_i < 56) {
    # Wahlkreis?
    new_dw <- w_landesstimmen(id)
  } else {
    # G oder S? 
    if (id %in% ks_ags) {
      new_dw <- s_landesstimmen(id)
      
    } else {
      # Gemeinde - checken, ob bekannt, sonst stop
      if (!id %in% gemeinden_df$AGS) stop("Unbekannte ID ",id)
      new_dw <- g_landesstimmen(id)
    }
  }
  
  # EIn paar allgemeine Dinge einrichten - mit der noch leeren Grafik
  # ... und unter Publish verstecken sich die Publikations-Optionen
  #
  # Alle Daten, Datenauswahl-, Farb- und Benennungs-Metadaten 
  # werden gesondert angepasst. (Das muss ja ggf. auch live gehen!)
  publish <- list()
  publish$blocks$enabled <- TRUE
  publish$blocks$embed <- TRUE
  publish$blocks[["blocks"]][["get-the-data"]] <- TRUE
  describe <- list()
  describe[["source-name"]] <- "Hessisches Statistisches Landesamt"
  describe[["source-url"]] <- "https://wahlen.hessen.de/landtagswahlen"
  describe[["byline"]] <- "Jan Eggers/Sandra Kiefer, hr" 

  
  dw_edit_chart(chart_id = new_dw$id,
                annotate = "Auszählung beginnt am 8.10.2023 um 18 Uhr",
                describe = describe,
                byline = "Jan Eggers/Sandra Kiefer, hr",
                publish = publish
  )
  # Kleine Fiesheit: einige der Metadaten-Keys, z.B. annotate$notes, 
  # weichen jetzt in der Grafik von den zurückgegebenen ab. Stört aber nicht
  # weiter, weil sie ohnehin überschrieben werden. 
  return(new_dw)
}



#### MAIN ####

# Livedaten-Verzeichnis anlegen bzw. leeren
if (dir.exists("./livedaten")) {
  # alles löschen
  livedaten_files <- list.files("./livedaten/", full.names=TRUE) 
  for (f in livedaten_files) {
    # Grausam, I know. 
    file.remove(f)
  }
} else {
  dir.create("./livedaten")
}

# Jetzt erst mal die Direktkandidaten-Säulen für die Wahlkreise
dw_df <- tibble()
for (wahlkreis in 1:55) {
  wk_str <- formatC(wahlkreis, width = 3,format="fg", flag="0")
  # Hole eine Grafik
  new_dw <- dw_direkt_anlegen(wk_str)
  fname <- new_dw$content$metadata$annotate$notes
  dw_id <- new_dw$id
  dw_df <- rbind(dw_df, tibble(id = wk_str,
                               typ = "w",
                               dw_id = dw_id,
                               fname = fname))
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
  
  # CSV anlegen
  # Im Prinzip: Eine leere Kandidatenliste für den Wahlkreis
  kand_list_df <- direktkandidaten_df %>% 
    filter(wk == wahlkreis) %>%
    mutate(prozent = 0.01) %>% 
    mutate(name = paste0(Nachname," (",partei,")")) %>% 
    select(name,prozent) %>% 
    head(5)
  write_csv(kand_list_df,paste0("livedaten/",
                                fname,
                                ".csv"))
}

# Liste mit den Datawrapper-IDs als XLSX exportieren
write.xlsx(dw_df,"datawrapper_ids.xlsx",overwrite = T)

#--- Die Direkt-Tabellen für die Gemeinden ----
g_v <- gemeinden_df %>% 
  # Die Gutsbezirke rausnehmen
  filter(bevoelkerung>0) %>% 
  filter(!AGS %in% ks_ags) %>% 
  pull(AGS) %>% 
  unique()

for (g in g_v) {
    new_dw <- dw_direkt_anlegen(g)
    fname <- new_dw$content$metadata$annotate$notes
    dw_id <- new_dw$id
    dw_df <- rbind(dw_df, tibble(id = g,
                                 typ = "g",
                                 dw_id = dw_id,
                                 fname = fname))
    
  # Noch ein wenig Metadaten-Zauber: Farbwerte der Parteien übergeben
  
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
  
  wahlkreis <- gemeinden_df %>% filter(AGS==g) %>% pull(wk) %>% first()
  # CSV anlegen
  # Im Prinzip: Eine leere Kandidatenliste für den Wahlkreis
  kand_list_df <- direktkandidaten_df %>% 
    filter(wk == wahlkreis) %>%
    mutate(stimmen = 0.01, prozent = "0,0% (+0)") %>% 
    mutate(name = paste0(Nachname," (",partei,")")) %>% 
    select(name,partei,stimmen,prozent) 
  write_csv(kand_list_df,paste0("livedaten/",
                                fname,
                                ".csv"))
}

# Liste mit den Datawrapper-IDs als XLSX exportieren
write.xlsx(dw_df,"datawrapper_ids.xlsx",overwrite = T)

#--- Landesstimmen: alle Wahlkreise, alle Gemeinden, alle Städte ----
part_list_df <- listen_df %>% 
  select(partei) %>% 
  mutate(stimmen = 0.01, prozent = "0,0% (+0)")

for (wahlkreis in 1:55) {
  wk_str <- formatC(wahlkreis, width = 3,format="fg", flag="0")
  # Hole eine Grafik
  new_dw <- dw_landesstimmen_anlegen(wk_str)
  fname <- new_dw$content$metadata$annotate$notes
  dw_id <- new_dw$id
  dw_df <- rbind(dw_df, tibble(id = wk_str,
                               typ = "w",
                               dw_id = dw_id,
                               fname = fname))
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
  
  # CSV anlegen
  # Im Prinzip: Eine leere Parteienliste für den Wahlkreis
  write_csv(part_list_df %>% mutate(wk = wahlkreis),paste0("livedaten/",
                                fname,
                                ".csv"))
}

# Liste mit den Datawrapper-IDs als XLSX exportieren
write.xlsx(dw_df,"datawrapper_ids.xlsx",overwrite = T)


for (g in g_v) {
  new_dw <- dw_landesstimmen_anlegen(g)
  fname <- new_dw$content$metadata$annotate$notes
  dw_id <- new_dw$id
  dw_df <- rbind(dw_df, tibble(id = g,
                               typ = "g",
                               dw_id = dw_id,
                               fname = fname))
  
  # Noch ein wenig Metadaten-Zauber: Farbwerte der Parteien übergeben
  
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
  
  wahlkreis <- gemeinden_df %>% filter(AGS==g) %>% pull(wk) %>% first()
  # CSV anlegen
  
  write_csv(part_list_df %>% mutate(wk = wahlkreis),paste0("livedaten/",
                                fname,
                                ".csv"))
}

# Liste mit den Datawrapper-IDs als XLSX exportieren
write.xlsx(dw_df,"datawrapper_ids.xlsx",overwrite = T)

# Stadt
for (s in ks_ags) {
  new_dw <- dw_landesstimmen_anlegen(s)
  fname <- new_dw$content$metadata$annotate$notes
  dw_id <- new_dw$id
  dw_df <- rbind(dw_df, tibble(id = s,
                               typ = "s",
                               dw_id = dw_id,
                               fname = fname))
  
  # Noch ein wenig Metadaten-Zauber: Farbwerte der Parteien übergeben
  
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
  
   
  write_csv(part_list_df,paste0("livedaten/",
                                                           fname,
                                                           ".csv"))
}


# Liste mit den Datawrapper-IDs als XLSX exportieren
write.xlsx(dw_df,"datawrapper_ids.xlsx",overwrite = T)

# Last but not least: Ganz Hessen
# new_dw <- dw_create_chart(title = "Landesstimmen: Hessen gesamt", 
#                           type="tables",
#                           folderId = ltw_folder_id,
#                           theme = hsde_theme)

# Alle Livedaten kopieren
n <- now()
system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/* gs://d.data.gcp.cloud.hr.de/livedaten/')
copy_time <- now()-n
cat("Operation took ",copy_time)


#### Grafiken scharf schalten ####
# Ultrastumpfer und nicht sehr R-mäßiger Loop - aber die DW-Grafiken sind ohnehin langsam
#
for (i in 1:nrow(dw_df)) {
  new_id <- dw_df$dw_id[i]
  fname <- dw_df$fname[i]
    
  meta_data <- list()
  meta_data[["upload-method"]]= "external-data"
  meta_data[["external-data"]] = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                        fname,
                                        ".csv")
  meta_data[["external-metadata"]] = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                            fname,
                                            ".json")
  meta_data[["use-datawrapper-cdn"]] = FALSE
  dw_edit_chart(new_id,data = meta_data)
  dw_publish_chart(new_id)
}

#### Parteifarben in die Tabellen setzen ####
# Au

einrichten_tabellengrafik <- function(dw_ids_v) {
  # Parteifarben an die Tabelle so übergeben, dass die Balkengrafik-Spalte
  # über die Spalte "partei" in der Farbe gesteuert werden kann. 
  # Geht davon aus, dass der Vektor Datawrapper-IDs für Tabellen enthält 
  
  # Liste mit Namen und Farben erzeugen
  # Jedem Attribut der Liste (Parteiname) wird ein Farbwert als Attribut zugewiesen
  farbliste <- setNames(as.list(listen_df$farbwert), 
           listen_df$partei)
  
  for (id in dw_ids_v) {
    meta <- dw_retrieve_chart_metadata(id)
    viz <- meta$content$metadata$visualize
    # Balkengrafik
    viz$columns$stimmen$ShowAsBar = TRUE
    # Balkenfarbe nach anderer Spalte
    viz$columns$stimmen$customBarColor = TRUE
    viz$columns$stimmen$customBarColorBy = "partei"
    viz$columns$stimmen$customColorBarBackground <- farbliste
    # Spalte "partei" ausblenden
    viz$columns$partei$showOnDesktop = FALSE
    viz$columns$partei$showOnMobile = FALSE
    # Format der Stimmen-Spalte
    viz$columns$stimmen$format = '0'
    viz$columns$stimmen$barRangeMin ='0'
    ####
    # Den Schlüssel an der Maximal-Prozentzahl aller Tabellen orientieren später
    viz$columns$stimmen$barRangeMin = '10'
    
    # Textgröße anpassen
    viz$header$style$fontSize = 0.9
    viz$columns$stimmen$style$fontSize = 0.9
    viz$columns$name$style$fontSize = 0.9
    viz$columns$prozent$style$fontSize = 0.9
    
    
    # Spalten umbenennen
    # ...das scheint zu crashen. 
    # 
    # mydata <- meta$content$metadata$data
    # mydata$changes$`0`$value = "Kandidat/in"
    # mydata$changes$`0`$previous = "name"
    # mydata$changes$`0`$row = "0"
    # mydata$changes$`1`$value = "Partei"
    # mydata$changes$`1`$previous = "partei"
    # mydata$changes$`1`$row = "1"
    # mydata$changes$`2`$value = "Stimmen"
    # mydata$changes$`2`$previous = "stimmen"
    # mydata$changes$`2`$row = "2"
    # mydata$changes$`3`$value = "+/-"
    # mydata$changes$`3`$previous = "prozent"
    # mydata$changes$`3`$row = "3"
    # 
    
    
    # Schriftgrößen anpassen
    
    
    dw_edit_chart(id,visualize = viz,
                  data =  mydata)
  }
} 



#### TEST ####
t <- dw_retrieve_chart_metadata("8Eviv")
# tt <- read_file("livedaten/Untitled.md") %>% str_extract_all(.,"(?<=s id\\: ).....")
# ALle angelegten Tabellen wieder löschen
if (FALSE) {
  dw_df <- read.xlsx("livedaten/datawrapper_ids.xlsx")
  for (dw in dw_df$dw_id) {
    try(dw_delete_chart(dw))
  }
  
}

t3 <- dw_retrieve_chart_metadata("i084q")

dw_source <- "ODjTJ"
t4 <- dw_retrieve_chart_metadata(src)
desc2 <- t4$content$metadata$describe

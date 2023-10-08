#' generiere_testdaten.R
#' 
#' Macht aus den Templates für Gemeinde- und Wahlkreisergebnisse
#' jeweils eine Serie von fiktiven Livedaten, um das Befüllen der
#' Grafiken testen zu können. 
#' 

require(tidyr)
require(dplyr)
require(readr)
require(stringr)
require(openxlsx)

# Alles weg, was noch im Speicher rumliegt
rm(list=ls())
TEST = TRUE

# Aktuelles Verzeichnis als workdir
setwd(this.path::this.dir())
# Aus dem R/Vorbereitung-Verzeichnis zwei Ebenen rauf
setwd("../..")

source("R/lies_konfiguration.R")
source("R/lies_aktuellen_stand.R")

#---- Funktion zum Testdaten-Löschen ----
lösche_testdaten <- function(){
  q <- tolower(readline(prompt = "Testdaten löschen - sicher? "))
  if (!(q %in% c("j","y","ja"))) { return() }
  # Datenarchiv weg
  if (file.exists("testdaten/fom_df.rds")){
    file.remove("testdaten/fom_df.rds")
  }
  # Testdaten
  testdaten_files <- list.files("testdaten", full.names=TRUE) 
  for (f in testdaten_files) {
    # Grausam, I know. 
    if (str_detect(f,"ortsteile[0-9]+\\.csv") |
        str_detect(f,"wahllokale[0-9]+\\.csv")) {
      file.remove(f)
    }
  }
}

vorlage_url = "Musterdateien/Muster_Downloaddatei_Wahlergebnisse_Landtagswahl_2023.csv"
# Vorlagen laden
# Leider geht mein Skript von Daten auf Wahllokal-Ebene aus, die Daten hier
# sind aber auf Orts-Ebene. 
vorlage_df <- read_delim(vorlage_url,  
                                    delim = ";", escape_double = FALSE, 
                                    locale = locale(date_names = "de", 
                                                    decimal_mark = ",", 
                                                    grouping_mark = "."), 
                                    trim_ws = TRUE, 
                                    skip =1) %>% 
  # Alle Schnellmeldungen aus der Vorlage löschen
  mutate(`Anzahl Wahlbezirke ausgezählt` = 0) %>% 
  mutate(across(c(6,10,11,12,14:46,76:108), ~ ifelse(is.na(.),0,.))) 
  # Joine die alten Wahldaten 2018 rein
  
hessen_df <- vorlage_df %>% filter(Gebietstyp == "LD")

wahllokale_max <- hessen_df$`Anzahl Wahlbezirke`



kreise_2018_df <- e2018_df %>% 
  filter(is.na(GKZ) & !is.na(Wahlkreis)) %>% 
  select(wk = Wahlkreis,
         wahlberechtigte2018 = 9,
         waehler2018 = 10,
         wahlschein2018 = 11,
         ungueltige2018 = 12,
         gueltige2018 = 13,
         ungueltige_l_2018 = 33,
         gueltige_l_2018 = 34) %>% 
  mutate(across(everything(), ~ as.integer(.)))

gemeinden_2018_df <- e2018_df %>% 
  filter(nchar(GKZ) == 6) %>% 
  select(ags = GKZ,
         wahlberechtigte2018 = 9,
         waehler2018 = 10,
         wahlschein2018 = 11,
         ungueltige2018 = 12,
         gueltige2018 = 13,
         ungueltige_l_2018 = 33,
         gueltige_l_2018 = 34) %>% 
  mutate(across(everything(), ~ as.integer(.)))

g_direkt_lang_2018_df <- e2018_df %>% filter(nchar(GKZ) == 6) %>% 
  select(ags = GKZ,
         14:32)%>% 
  pivot_longer(cols = 2:20, names_to = "partei", values_to ="stimmen_2018") %>% 
  mutate(partei = str_replace(partei,"\\.\\.\\..+","")) %>% 
  mutate(stimmen_2018 = as.integer(stimmen_2018)) %>%
  mutate(stimmen_2018 = ifelse(is.na(stimmen_2018),0,stimmen_2018))
  
g_landesstimmen_lang_2018_df <- e2018_df %>% filter(nchar(GKZ) == 6) %>% 
  select(ags = GKZ,
         35:57) %>% 
  pivot_longer(cols = 2:24, names_to = "partei", values_to ="stimmen_2018") %>% 
  mutate(partei = str_replace(partei,"\\.\\.\\..+","")) %>% 
  mutate(stimmen_2018 = as.integer(stimmen_2018)) %>% 
  mutate(stimmen_2018 = ifelse(is.na(stimmen_2018),0,stimmen_2018))



parteien_v <- function(ags) {
  # Schaut, welcher Wahlkreis (bzw. welche Wahlkreise) 
  # zu dieser AGS gehören und liefert dann die Gemeinden 
  wk_v <- gemeinden_alle_df %>% filter(AGS %in% ags) %>% pull(wk)
  partei_v <- direktkandidaten_df %>% filter(wk %in% wk_v) %>% pull(partei) %>% unique()
}

i = 0
# Schleife für die Kreis-Tests: Solange "ausgezählt" sind...
while(hessen_df %>% 
      pull(`Anzahl Wahlbezirke ausgezählt`) < wahllokale_max) {
  # Splitte Vorlage in Hessen, Kreise, Gemeinden
  # Jetzt Zufallszahlen
  print(i)
  factor <- function() { return(i/5 +runif(1,0,1/5)) }
  kreise_df <- vorlage_df %>% filter(Gebietstyp == "WK") %>% 
    mutate(wk = as.integer(str_sub(Gebietsschlüssel,1,3))) %>% 
    # Wahllokale ausgezählt - Zufall, aber immer ein Fünftel mehr
    mutate(`Anzahl Wahlbezirke ausgezählt` = floor(`Anzahl Wahlbezirke` * factor())) %>% 
    mutate(`Anzahl Wahlbezirke ausgezählt` = ifelse(`Anzahl Wahlbezirke ausgezählt` > `Anzahl Wahlbezirke`,
                                                    `Anzahl Wahlbezirke`,
                                                    `Anzahl Wahlbezirke ausgezählt`)) %>% 
    left_join(kreise_2018_df,by = "wk") %>% 
    mutate(Wahlberechtigte = wahlberechtigte2018,
      `Wählerinnen und Wähler`= floor(waehler2018 * factor()),
      `Wählerinnen und Wähler mit Wahlschein` = floor(wahlschein2018 * factor()),
      # Wahlbeteiligung später
      `ungültige Wahlkreisstimmen` = floor(ungueltige2018 * factor()),
      #gültig später
    ) %>% 
    mutate(Wahlbeteiligung = `Wählerinnen und Wähler` / Wahlberechtigte *100,
           `gültige Wahlkreisstimmen` = `Wählerinnen und Wähler` - `ungültige Wahlkreisstimmen`) %>% 
    mutate(`ungültige Landesstimmen` =  floor(ungueltige_l_2018 * factor())) %>% 
    mutate(`gültige Landesstimmen` = `Wählerinnen und Wähler` - `ungültige Landesstimmen`) %>% 
    # Parteien ins Langformat bringen
    pivot_longer(cols = 18:46,names_to ="partei", values_to = "stimmen") %>% 
    # Reale Ergebnisse 2018 nach Direktmandat/Partei
    left_join(frankentable_direkt_lang_df %>%  
                mutate(partei = paste0(partei," Wahlkreisstimmen")) %>% 
                select(wk,partei,stimmen_2018), by=c("wk","partei")) %>% 
    # Errechne Zufallsanteil
    mutate(stimmen = floor(stimmen_2018 * factor())) %>%
    select(-stimmen_2018,wk) %>% 
    # Zurück ins Querformat, Zusatzspalten weg
    pivot_wider(names_from = partei, values_from = stimmen)  %>% 
    # Zurücksortieren
    select(1:17,
           # jettz die neu erzeugten Stimmen
           128:156,
           # normal weiter
           18:119) %>% 
  # Landesstimmen
    mutate(wk = as.integer(str_sub(Gebietsschlüssel,1,3))) %>% 
    pivot_longer(cols = 80:108,names_to ="partei", values_to = "stimmen") %>% 
    # Reale Ergebnisse 2018 nach Direktmandat/Partei
    left_join(frankentable_landesstimmen_lang_df %>%  
                mutate(partei = paste0(partei," Landesstimmen")) %>% 
                select(wk,partei,stimmen_2018), by=c("wk","partei"))  %>% 
    # Errechne Zufallsanteil
    mutate(stimmen = floor(stimmen_2018 * factor())) %>%
    select(-stimmen_2018,wk) %>% 
    # Zurück ins Querformat, Zusatzspalten weg
    pivot_wider(names_from = partei, values_from = stimmen) %>% 
    # Zurücksortieren
    select(1:79,
           121:149,
           80:119)
    for (ii in c(6:137,143)) {
      hessen_df[[ii]] <- sum(kreise_df[[ii]])
    }
  gemeinden_df <- vorlage_df %>% filter(Gebietstyp %in% c("VF","KS")) %>% 
    mutate(ags = as.integer(str_sub(Gebietsschlüssel,4,9))) %>% 
    # Wahllokale ausgezählt - Zufall, aber immer ein Fünftel mehr
    mutate(`Anzahl Wahlbezirke ausgezählt` = floor(`Anzahl Wahlbezirke` * factor())) %>% 
    mutate(`Anzahl Wahlbezirke ausgezählt` = ifelse(`Anzahl Wahlbezirke ausgezählt` > `Anzahl Wahlbezirke`,
                                                    `Anzahl Wahlbezirke`,
                                                    `Anzahl Wahlbezirke ausgezählt`)) %>% 
    left_join(gemeinden_2018_df,by = "ags") %>% 
    mutate(Wahlberechtigte = wahlberechtigte2018,
           `Wählerinnen und Wähler`= floor(waehler2018 * factor()),
           `Wählerinnen und Wähler mit Wahlschein` = floor(wahlschein2018 * factor()),
           # Wahlbeteiligung später
           `ungültige Wahlkreisstimmen` = floor(ungueltige2018 * factor()),
           #gültig später
    ) %>% 
    mutate(Wahlbeteiligung = `Wählerinnen und Wähler` / Wahlberechtigte *100,
           `gültige Wahlkreisstimmen` = `Wählerinnen und Wähler` - `ungültige Wahlkreisstimmen`) %>% 
    mutate(`ungültige Landesstimmen` =  floor(ungueltige_l_2018 * factor())) %>% 
    mutate(`gültige Landesstimmen` = `Wählerinnen und Wähler` - `ungültige Landesstimmen`) %>% 
    # Parteien ins Langformat bringen
    pivot_longer(cols = 18:46,names_to ="partei", values_to = "stimmen") %>% 
    # Kandidaten für die bekannten Gemeinden 
    # nicht filtern, sonst hat man später zu wenige Spalten
    # rowwise() %>% 
    # mutate(pv = list(parteien_v(ags))) %>% 
    # ungroup() %>% 
    mutate(partei = str_replace(partei," Wahlkreisstimmen","")) %>% 
    # mutate(p_exists = (partei %in% pv[[1]])) %>% 
    # filter(p_exists) %>% 
    # select(-p_exists,-pv) %>% 
  # Reale Ergebnisse 2018 nach Direktmandat/Partei
    left_join(g_direkt_lang_2018_df %>%  
                select(ags,partei,stimmen_2018), by=c("ags","partei")) %>% 
    # Errechne Zufallsanteil
    mutate(stimmen = floor(stimmen_2018 * factor())) %>%
    select(-stimmen_2018) %>% 
    # Zurück ins Querformat, Zusatzspalten weg, Namen wieder normalisieren
    mutate(partei = paste0(partei," Wahlkreisstimmen")) %>% 
    pivot_wider(names_from = partei, values_from = stimmen)  %>% 
    # Zurücksortieren
    select(1:17,
           # jettz die neu erzeugten Stimmen
           128:156,
           # normal weiter
           18:119) %>% 
    # Landesstimmen
    mutate(ags = as.integer(str_sub(Gebietsschlüssel,4,9))) %>% 
    pivot_longer(cols = 80:108,names_to ="partei", values_to = "stimmen") %>% 
    # Reale Ergebnisse 2018 nach Direktmandat/Partei
    mutate(partei = str_replace(partei," Landesstimmen","")) %>% 
    left_join(g_landesstimmen_lang_2018_df %>%  
                select(ags,partei,stimmen_2018), by=c("ags","partei"))  %>% 
    # Errechne Zufallsanteil
    mutate(stimmen = floor(stimmen_2018 * factor())) %>%
    select(-stimmen_2018,ags) %>% 
    # Zurück ins Querformat, Zusatzspalten weg, Namen wieder Normalisieren
    mutate(partei = paste0(partei,(" Landesstimmen"))) %>% 
    pivot_wider(names_from = partei, values_from = stimmen) %>% 
    # Zurücksortieren
    select(1:79,
           121:149,
           80:119)
  # Alles zusammenbinden, schreiben  
  muster_df <- hessen_df %>% bind_rows(kreise_df) %>% bind_rows(gemeinden_df)
  write_csv2(muster_df,"testdaten/kreise_tmp.csv")
  datumsstring = paste0("Musterdatei;JanEggers;Stand: ",format(now(),format="%d.%m.%Y %H:%M:%S"))
  tmp <- c(datumsstring,read_lines("testdaten/kreise_tmp.csv"))
  write_lines(tmp,paste0("testdaten/muster_kreise_",i,".csv"))
  i = i+1
} # End FOR
cat("DONE.")


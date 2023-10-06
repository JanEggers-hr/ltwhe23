library(tidyverse)
library(pacman)
library(readr)
p_load(openxlsx)

rm(list=ls())

# Aktuelles Verzeichnis als workdir
setwd(this.path::this.dir())
# Aus dem R-Verzeichnis eine Ebene rauf
setwd("..")

#' vorbereitung_wahlkreisdaten.R
#' 
#' Erstellung der Index-Dateien durch FIltern und Joinen der Ausgangsdaten
#' In der Regel jeden Abschnitt für sich ausführen; voriges Environment ggf. der
#' Übersichtlichkeit halber löschen


#### Wahlkreis-s anlegen ####

# Erledigt. Besser mit den aufgearbeiteten Daten arbeiten. 
if (FALSE)
{
  wahlkreise_df <- read.xlsx("index/hsl-wahlkreise-2023.xlsx") %>% 
  select(wk = LWK, wk_name = LWK_NAME)

  gemeinden_df <- read.xlsx("index/gemeinden-kreise.xlsx",sheet=1) %>% 
    select(AGS,name=kommune_name,flaeche=`Fläche_km2`,
           bevoelkerung=Gesamtbevölkerung) %>% 
    mutate(AGS = as.integer(AGS)) 
  
  kreise_df <- read.xlsx("index/gemeinden-kreise.xlsx", sheet =2) %>% 
    select(k_ags = AGS,kreis_name=hr_name)
  
  tabelle_df <- read.xlsx("index/gemeinden_wahlkreise_lang.xlsx") %>% 
    rename(wk = Wahlkreis) %>% 
    left_join(wahlkreise_df,by="wk") %>% 
    mutate(k_ags = (AGS %/% 1000) * 1000) %>% 
    left_join(kreise_df,by="k_ags") %>% 
    left_join(gemeinden_df, by="AGS")
  
  unused_df <- read.xlsx("index/gemeinden_wahlkreise_lang.xlsx") %>%
    anti_join(gemeinden_df,by="AGS")
  
  #write.xlsx(tabelle_df,"../index/wahlgemeinden_alle.xlsx", overwrite = T)
  # Zuordnung Stadtteile -> Wahlkreis?
}

#### Parteien mit Wahllisten
if (FALSE) {
  parteien_v <- read.xlsx("index/kandidaten_alle.xlsx") %>% 
    filter(!is.na(Listenplatz)) %>% 
    pull(Partei) %>% unique()
  
  write.xlsx(read.xlsx("index/parteien_idx.xlsx") %>% 
               filter(partei %in% parteien_v),
             "index/parteien_listen.xlsx")
}

#### Gemeinden zu Sophora-Geotag-IDs

if (FALSE) {
  sophora_df <- read.xlsx("index/sophora-export-1.xlsx") %>% 
    bind_rows(read.xlsx("index/sophora-export-2.xlsx")) %>% 
    bind_rows(read.xlsx("index/sophora-export-3.xlsx")) %>% 
    bind_rows(read.xlsx("index/sophora-export-4.xlsx")) %>% 
    bind_rows(read.xlsx("index/sophora-export-5.xlsx")) %>% 
    select(AGS = 3, hr_name = 1, sophora_id = 4, uuid = 5, import_id = 6) %>% 
    arrange(AGS)
  
  sophora_tests_df <- sophora_df %>% left_join(tabelle_df %>% 
                                                 filter(Gebietstyp != "ST") %>% 
                                                 select(AGS,wk,wk_name,kreis_name) %>% 
                                                 mutate(AGS=paste0("06",AGS)),
                                               by="AGS")
  
  
  write.xlsx(sophora_tests_df,"index/ags_sophora_id.xlsx", overwrite = T)  
}

#### Indexdatei Spalten zu Ergebnissen ####

if (FALSE) {
  direktkandidaten_df <- read.xlsx("index/kandidaten_alle.xlsx") %>% 
    filter(!is.na(wk)) %>% 
    arrange(wk,Partei)
  
  listenkandidaten_df <- read.xlsx("index/kandidaten_alle.xlsx") %>% 
    filter(!is.na(Listenplatz)) %>% 
    arrange(wk,Partei)
  
  
  listen_df <- read_delim("Musterdateien/Wahlvorschlaege_Landtagswahl_2023.csv", 
                          delim = ";", escape_double = FALSE, col_names = FALSE, 
                          col_types = cols(X1 = col_integer()), 
                          trim_ws = TRUE) %>% 
    select(id=1,partei=2,parteiname=3)
  
  wahldaten_df <- read_delim("Musterdateien/Muster_Downloaddatei_Wahlergebnisse_Landtagswahl_2023.csv", 
                             delim = ";", escape_double = FALSE, 
                             locale = locale(date_names = "de", 
                                             decimal_mark = ",", 
                                             grouping_mark = "."), 
                             trim_ws = TRUE, skip = 1)
  
  
  spalten_df <- tibble(spalte = colnames(wahldaten_df)) %>%  mutate(idx = seq_along(spalte)) 
  
  # Parteien zu Spalten
  
  # Kandidaten zu Spalten:
  # Spalten 18-46
  spaltenindex_direkt_df <- spalten_df %>%  filter(idx %in% 18:46) %>% 
    mutate(partei = stringr::str_replace(spalte," Wahlkreisstimmen","")) %>% 
    select(-spalte) %>% 
    # Parteien, die nicht zur Direktwahl antreten: 
    # - ÖDP
    # - Verjüngungsforschung
    # - DIE NEUE MITTE
    # - BUNDESPARTEI KLIMALISTE
    mutate(OK = (partei %in% unique(direktkandidaten_df$Partei))) %>% 
    filter(OK) %>% select(-OK) 
  
  
  write.xlsx(spaltenindex_direkt_df,"index/spaltenindex_direkt.xlsx",overwrite = T)

  # Wahllisten zu Spalten: 
  # Spalten 80-108
  
  spaltenindex_landesstimmen_df <- spalten_df %>%  filter(idx %in% 80:108) %>% 
    mutate(partei = stringr::str_replace(spalte," Landesstimmen","")) %>% 
    select(-spalte) %>% 
    # Parteien, die nicht zur Listenwahl antreten: 
    # - Klimaliste Wählerliste (sondern: Bundesliste)
    # - Bündnis C
    # - WDMR
    # - MERA25
    # - NEV
    # - PP
    # - SGV
    # - Solibew
    mutate(OK = (partei %in% unique(listenkandidaten_df$Partei))) %>% 
    filter(OK) %>% select(-OK) 
  
  write.xlsx(spaltenindex_landesstimmen_df,"index/spaltenindex_landesstimmen.xlsx")
    
}


#--- Wahlkreis-Ergebnisse 2018 umgerechnet, Index 2018 ----


gemeinden_df <- read.xlsx("index/gemeinden_alle.xlsx")
dw_id <- read.xlsx("index/datawrapper_ids.xlsx")
dw_id_direkt_kreise <- dw_id %>% filter(typ=="w") %>% filter(str_detect(fname,"direkt$"))
dw_id_direkt_gemeinden <- dw_id %>% filter(typ=="g") %>% filter(str_detect(fname,"direkt$"))
dw_id_landesstimmen_gemeinden <- dw_id %>% filter(typ %in% c("g","s")) %>% filter(str_detect(fname,"landesstimmen$"))
dw_id_landesstimmen_kreise <- dw_id %>% filter(typ =="w") %>% filter(str_detect(fname,"landesstimmen$"))


gemeinden_id_df <- gemeinden_df %>% filter(Gebietstyp %in% c("VF","KS")) %>% 
  select(AGS,name,wk,wk_name,kreis_name,flaeche,bevoelkerung,) %>% 
  filter(bevoelkerung > 0) %>% 
  mutate(AGS = as.character(AGS)) %>% 
  mutate(bev_dichte = bevoelkerung / flaeche) %>% 
  mutate(dichte_quintil = ntile(bev_dichte,5)) %>% 
  left_join(dw_id_direkt_gemeinden %>% select(AGS=id,direkt_id=dw_id),by="AGS") %>% 
  left_join(dw_id_landesstimmen_gemeinden %>%  select(AGS=id,landesstimmen_id=dw_id),by="AGS")

write.xlsx(gemeinden_id_df,"info/gemeinden_idx.xlsx")

kreise_id_df <- gemeinden_df %>% filter(Gebietstyp %in% c("VF", "KS")) %>% 
  select(AGS,name,wk,wk_name,kreis_name,flaeche,bevoelkerung,) %>% 
  group_by(wk) %>% 
  summarize(wk_name = first(wk_name),
            kreise = paste(unique(kreis_name),collapse= " "), 
            gemeinden = n(),
            bevoelkerung = sum(bevoelkerung), 
            flaeche = sum(flaeche),
            ) %>% 
  mutate(wk = formatC(wk,width = 3,format="fg", flag="0")) %>% 
  mutate(bev_dichte = bevoelkerung / flaeche) %>% 
  mutate(dichte_quintil = ntile(bev_dichte,5)) %>% 
  left_join(dw_id_direkt_kreise %>% select(wk=id,direkt_id=dw_id),by="wk") %>% 
  left_join(dw_id_landesstimmen_kreise %>%  select(wk=id,landesstimmen_id=dw_id),by="wk")

write.xlsx(kreise_id_df,"info/kreise_idx.xlsx")

#### Direktwahl-Sieger 2018 ####

direkt2018_df <- read.xlsx("ergebnisse2018/vergleichsdaten_hs_umgerechnet_2018.xlsx",
                           sheet = 4,
                           startRow = 4,
                           colNames = FALSE) %>% 
  select(wk = 1,
         wk_name = 2,
         full_name = 3,
         partei = 4, 
         stimmen = 5,
         prozent = 6, 
         vorsprung = 7) %>% 
  mutate(gueltig = stimmen/prozent*100,
         zweiter_stimmen = stimmen - vorsprung) %>% 
  mutate(zweiter_prozent = zweiter_stimmen/gueltig * 100) %>% 
  mutate(Nachname = str_replace(full_name,"Dr\\. ","" )) %>% 
  mutate(Nachname = str_extract(Nachname,"[A-Za-zäöüßÄÖÜ\\-]+")) %>% 
  filter(!is.na(wk))

write.xlsx(direkt2018_df,"ergebnisse2018/direktwahl2018.xlsx")

direkt2018_n_df <- read.xlsx("ergebnisse2018/vergleichsdaten_hs_umgerechnet_2018.xlsx",
                           sheet = 4,
                           startRow = 4,
                           colNames = FALSE) %>% 
  select(wk = 1,
         wk_name = 2,
         full_name = 3,
         partei = 4, 
         stimmen = 5,
         prozent = 6, 
         vorsprung = 7) %>% 
  mutate(gueltig = stimmen/prozent*100,
         zweiter_stimmen = stimmen - vorsprung) %>%
  # Nachrücker und Ausgeschiedene markieren
  mutate(nachrücker = is.na(wk)) %>% 
  mutate(ausgeschieden = lead(nachrücker)) %>% 
  # Löcher stopfen
  mutate_all(~if_else(is.na(.x), lag(.x), .x)) %>% 
  mutate(gueltig = stimmen/prozent*100,
         zweiter_stimmen = stimmen - vorsprung) %>% 
  mutate(zweiter_prozent = zweiter_stimmen/gueltig * 100) %>% 
  mutate(Nachname = str_replace(full_name,"Nachrücker\\: ","")) %>% 
  mutate(Nachname = str_replace(Nachname,"Nachrückerin\\: ","")) %>%
  mutate(Vorname = str_extract(full_name,"(Dr\\. )|(Prof\\. )")) %>% 
  mutate(Vorname = paste0(ifelse(is.na(Vorname),"",Vorname),str_split_i(full_name,"\\, ",2))) %>% 
  # Delorzify
  mutate(Nachname = str_replace(Nachname,"Prof\\. ","" )) %>% 
  # Doktoren raus
  mutate(Nachname = str_replace(Nachname,"Dr\\. ","" )) %>% 
  mutate(Nachname = str_extract(Nachname,"[A-Za-zäöüßÄÖÜ\\-]+")) %>% 
  # Kreisnamen enthalten irritierende Leerzeichen- 
  # deshalb nochmal korrekte Kreisnamen reinholen
  left_join(read.xlsx("index/wahlkreise_alle.xlsx") %>% 
              select(wk,wkn = wk_name),by="wk" ) %>% 
  mutate(wk_name = wkn) %>% 
  select (-wkn)

write.xlsx(direkt2018_n_df,"ergebnisse2018/direktwahl2018_nachrücker.xlsx")

#--- Ergebnisse 2018 Landesstimmen nach Wahlkreis umgerechnet ----

k2018_umgerechnet_df <- read.xlsx("ergebnisse2018/vergleichsdaten_hs_umgerechnet_2018.xlsx",
                                                  sheet = 6,
                                                  startRow = 5,
                                                  colNames = FALSE) %>% 
  select(wk = 1, 
         wk_name = 2,
         wahl = 3,
         wahlberechtigte = 4,
         waehler = 5,
         wahlbeteiligung = 6,
         ungueltig = 7,
         ungueltig_prozent = 8,
         gueltig = 9,
         CDU = 10,
         GRÜNE = 12,
         SPD = 14,
         AfD = 16,
         FDP = 18,
         `DIE LINKE` = 20,
         `FREIE WÄHLER` = 22,
         Tierschutzpartei = 24,
         `Die PARTEI` = 26,
         PIRATEN = 28,
         ÖDP = 30,
         NPD = 32,
         Sonstige = 34
         ) %>% 
  # Wahlkreis-Zelle aus der Zwischenzeile darunter ausfüllen
  fill(wk,wk_name) %>% 
  # Zwischenzeilen ausfiltern
  filter(!is.na(wahl)) %>% 
  # Sonderbedingugn für ganz Hessen
  mutate(wk = ifelse(is.na(wk),0,wk))


k2018_umgerechnet_landesstimmen_df <- k2018_umgerechnet_df %>% 
  filter(wahl == "L  18 L") %>% 
  select(-wahl) %>% 
  # Die Frankfurt-Punkte durch NA ersetzen
  mutate(across(everything(), ~  ifelse(str_detect(., "\\•"), NA, .)))

k2018_umgerechnet_direkt_df <- k2018_umgerechnet_df %>% 
  filter(wahl == "L  18 W") %>% 
  select(-wahl) %>% 
  # Die Frankfurt-Punkte durch NA ersetzen
  mutate(across(everything(), ~  ifelse(str_detect(., "\\•"), NA, .)))

write.xlsx(k2018_umgerechnet_landesstimmen_df,"ergebnisse2018/kreis_landesstimmen_um_2018.xlsx")
write.xlsx(k2018_umgerechnet_direkt_df,"ergebnisse2018/kreis_direkt_um_2018.xlsx")

#--- 2018er Ergebnisse nicht umgerechnet ----


kreise2018_df <- read_csv2("ergebnisse2018/wahlergebnisse2.csv", 
                                    locale = locale(date_names = "de",
                                                    decimal_mark = ",", 
                                                    grouping_mark = ".", 
                                                    encoding = "ISO-8859-1"), 
                                    skip = 1) %>% 
  filter(is.na(GKZ) & Wahlkreis %in% 1:55) %>% 
  mutate(across(6:55, ~ as.numeric(.)))

#temp
spalten_direkt_2018_df <- read_csv2("ergebnisse2018/wahlergebnisse2.csv", 
                            locale = locale(date_names = "de",
                                            decimal_mark = ",", 
                                            grouping_mark = ".", 
                                            encoding = "ISO-8859-1")) %>% 
  rename(x =1) %>% 
  filter(str_detect(x,"^Lfd")) %>% 
  select(14:32) %>% 
  pivot_longer(everything(),names_to = "idx", values_to = "partei") %>% 
  mutate(idx = as.integer(str_extract(idx,"[0-9]+")))

spalten_landesstimmen_2018_df <- read_csv2("ergebnisse2018/wahlergebnisse2.csv", 
                                    locale = locale(date_names = "de",
                                                    decimal_mark = ",", 
                                                    grouping_mark = ".", 
                                                    encoding = "ISO-8859-1")) %>% 
  rename(x =1) %>% 
  filter(str_detect(x,"^Lfd")) %>% 
  select(35:57) %>% 
  pivot_longer(everything(),names_to = "idx", values_to = "partei") %>% 
  mutate(idx = as.integer(str_extract(idx,"[0-9]+")))


kreise_direkt_2018_df <- kreise2018_df %>% 
  select(wk = 2, 
         wk_name = 4,
         wahlberechtigte = 9,
         waehler = 10,
         wahlbeteiligung = 6,
         ungueltig = 12,
         ungueltig_prozent = 8,
         gueltig = 13,
         14:32) %>% 
  mutate(wahlbeteiligung = waehler / wahlberechtigte * 100,
         ungueltig_prozent = ungueltig / waehler * 100)

# Spaltennamen begradigen
colnames(kreise_direkt_2018_df) <- c(colnames(kreise_direkt_2018_df)[1:8],
                                     spalten_direkt_2018_df %>% pull(partei))

write.xlsx(kreise_direkt_2018_df,"ergebnisse2018/kreise_direkt_2018.xlsx")

# "Frankentabelle" aus den umgerechneten Wahlkreis-Werten plus der 2018er-Ergebnisse
# für die nicht umzurechnenden Wahlkreise Frankfurt I und III.

frankentabelle_direkt_df <- k2018_umgerechnet_direkt_df %>% 
  filter(!(wk %in% c(34,36))) %>%
  mutate(across(3:21, ~as.numeric(ifelse(.=="x",NA,.)))) %>% 
  bind_rows(kreise_direkt_2018_df %>% 
              mutate (Sonstige = `DIE VIOLETTEN` +
                        LKR + 
                        `MENSCHLICHE WELT` +
                        `V-Partei3` +
                        APPD +
                        DiB +
                        NEV +
                        `ÖkoLinX Hessen`) %>% 
              select(-`DIE VIOLETTEN`,
                     -LKR,
                     -`MENSCHLICHE WELT`,
                     -`V-Partei3`,
                     -APPD,
                     -DiB,
                     -NEV,
                     -`ÖkoLinX Hessen`) %>% 
              filter(wk %in% c(34,36))
            )

write.xlsx(frankentabelle_kreise_df,"ergebnisse2018/frankentabelle_direkt_2018.xlsx")

# Landesstimmen direkt 
kreise_landesstimmen_2018_df <- kreise2018_df %>% 
  select(wk = 2, 
         wk_name = 4,
         wahlberechtigte = 9,
         waehler = 8,
         wahlbeteiligung = 6,
         ungueltig = 33,
         ungueltig_prozent = 7,
         gueltig = 34,
         35:57) %>% 
  mutate(waehler = ungueltig + gueltig) %>% 
  mutate(wahlbeteiligung = waehler / wahlberechtigte * 100,
         ungueltig_prozent = ungueltig / waehler * 100) 
  

# Spaltennamen begradigen
colnames(kreise_landesstimmen_2018_df) <- c(colnames(kreise_landesstimmen_2018_df)[1:8],
                                     spalten_landesstimmen_2018_df %>% pull(partei))

write.xlsx(kreise_landesstimmen_2018_df,"ergebnisse2018/kreise_landesstimmen_2018.xlsx")


frankentabelle_landesstimmen_df <- k2018_umgerechnet_landesstimmen_df %>% 
  filter(!(wk %in% c(34,36))) %>%
  mutate(across(3:21, ~as.numeric(ifelse(.=="x",NA,.)))) %>% 
  bind_rows(kreise_landesstimmen_2018_df %>% 
              mutate(across(3:31, ~ as.numeric(.))) %>% 
              mutate (Sonstige = `Graue Panther`+
                        BüSo +
                        `AD-Demokraten` +
                        `Bündnis C` +
                        BGE +
                        `DIE VIOLETTEN` + 
                        LKR +
                        `MENSCHLICHE WELT` + 
                        `Die Humanisten` +
                        Gesundheitsforschung +
                        `V-Partei3`) %>% 
              select(-`Graue Panther`,
                     -BüSo,
                     -`AD-Demokraten`,
                     -`Bündnis C`,
                     -BGE,
                     -`DIE VIOLETTEN`,
                     -LKR,
                     -`MENSCHLICHE WELT`,
                     -`V-Partei3`,
                     -`Die Humanisten`,
                     -Gesundheitsforschung) %>% 
              filter(wk %in% c(34,36))
  )

write.xlsx(frankentabelle_landesstimmen_df,"ergebnisse2018/frankentabelle_landesstimmen_2018.xlsx")

#---- Für Sandra die tabelle der Kandidat:innen nach Kreis - und die Kreise mit Änderungen ----

wk_vergroesserungen_df <- read.xlsx("index/gemeinden_alle.xlsx") %>% 
  filter(!is.na(Wahlkreis2018)) %>% 
  mutate(neu_dazu = ifelse(is.na(name),Stadtteilname,name)) %>% 
  select(wk,wk_name,neu_dazu)

wk_verkleinerungen_df <- read.xlsx("index/gemeinden_alle.xlsx") %>%
  filter(!is.na(Wahlkreis2018)) %>% 
  mutate(abgegeben = ifelse(is.na(name),Stadtteilname,name)) %>% 
  select(-wk) %>% 
  select(wk = Wahlkreis2018,abgegeben) %>% 
  left_join(read.xlsx("index/wahlkreise_alle.xlsx"),by="wk") %>% 
  select(wk,wk_name,abgegeben) %>% 
  arrange(wk)

write.xlsx(wk_vergroesserungen_df,"index/wk_vergroesserungen.xlsx")
write.xlsx(wk_verkleinerungen_df,"index/wk_verkleinerungen.xlsx")

wk_direkt_2018_df <- read.xlsx("index/kandidaten_alle.xlsx") %>%
  filter(!is.na(wk)) %>% 
  mutate(link_kandidatencheck = ifelse(is.na(Check_id),NA,
                                       paste0("https://www.hessenschau.de/politik/landtagswahl/kandidatencheck/candidate/",Check_id))) %>% 
  mutate(vorname = paste0(ifelse(is.na(Titel),"",paste0(Titel," ")),Vorname)) %>% 
  select(wk, wk_name = wkn, partei = Partei,
         nachname = Nachname, vorname, beruf = Beruf, geboren = Geburtsjahr, 
         geburtsort = Geburtsort,
         link_kandidatencheck) %>% 
  left_join(read.xlsx("index/parteien_idx.xlsx"), by="partei") %>% 
  arrange(wk,id) %>%
  select(-id,-farbwert) %>% 
  left_join(read.xlsx("ergebnisse2018/direktwahl2018_nachrücker.xlsx") %>% 
              filter(!nachrücker) %>% 
              select(wk, 
                     sieger2018_vorname= Vorname,
                     sieger2018_nachname = Nachname, 
                     sieger2018_partei = partei,
                     sieger2018_prozent = prozent,
                     sieger_vorsprung = vorsprung), 
            by="wk") %>% 
  left_join(read.xlsx("ergebnisse2018/direktwahl2018_nachrücker.xlsx") %>% 
              filter(nachrücker) %>% 
              select(wk,
                     nachrücker_vorname = Vorname,
                     nachrücker_nachname = Nachname),
            by="wk")

write.xlsx(wk_direkt_2018_df,"info/kandidaten_kreis.xlsx")
  
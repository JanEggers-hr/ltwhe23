#' lies_konfiguration.R
#' 
#' Konfigurationsdatei lesen, Indexdateien lesen, zusätzliche Indizes erzeugen
#' 

# (das später durch ein schnelleres .rda ersetzen)
# load ("index/index.rda")

# Konfiguration auslesen und in Variablen schreiben
#
# Generiert für jede Zeile die genannte Variable mit dem Wert value
#
# Derzeit erwartet das Programm: 
# - wahl_name - Name der Wahl; für "ltwhe23" leer lassen 
# - stimmbezirke_url - URL auf Ergebnisdaten
# - kandidaten_fname - Dateiname der Kandidierenden-Liste (s.u.)

# - parteien_fname - Index aller kandidierenden Parteien mit Langnamen und Farbwert

# - datawrapper_fname - Dateiname für die Datawrapper-Verweis-Datei

#' 


# Falls der Parameter wahl_name noch nicht definiert ist, 
# setze ihn erst mal auf das derzeitige Verzeichnis. 
if (exists("wahl_name")) {
  index_pfad = paste0("index/",wahl_name,"/")
  if (!dir.exists(index_pfad)) {
    index_pfad <- "index/"
  }
} else {
  index_pfad <- "index/"
}

if (!exists("TEST")) TEST <- FALSE
# Lies die Indexdatei aus dem Verzeichnis wahl_name. 
# Falls keines angegeben: aus dem aktuellen Verzeichnis
if (TEST) {
  config_df <- read.xlsx(paste0(index_pfad,"config_test.xlsx"))
} else {
  config_df <- read.xlsx(paste0(index_pfad,"config.xlsx"))
}
for (i in c(1:nrow(config_df))) {
  # Erzeuge neue Variablen mit den Namen und Werten aus der CSV
  assign(config_df$name[i],
         # Kann man den Wert auch als Zahl lesen?
         # Fieses Regex sucht nach reiner Zahl oder Kommawerten.
         # Keine Exponentialschreibweise!
         ifelse(grepl("^[0-9]*\\.*[0-9]+$",config_df$value[i]),
                # Ist eine Zahl - wandle um
                as.numeric(config_df$value[i]),
                # Keine Zahl - behalte den String
                config_df$value[i]))
}

lies_daten <- function(index_pfad = index_pfad, fname) {
  fname = paste0(index_pfad, fname)
  if (toupper(str_extract(fname,"(?<=\\.)[A-zA-Z]+$")) %in% c("XLS","XLSX")) {
    # Ist offensichtlich eine Excel-Datei. Lies das erste Sheet.
    return(read.xlsx(fname))
  } else {
    # Geh von einer CSV-Datei aus.
    # Schau in die erste Zeile und zähle Kommas vs. Semikolons
    first_line <- readLines(fname, n = 1)
    commas <- str_split(first_line, ",") %>% unlist() %>% length()
    semicolons <- str_split(first_line, ";") %>% unlist() %>% length()
    if (commas > semicolons) {
      return(read_csv(fname))
    } else {
      # Glaube an das Gute im Menschen: Erwarte Trenn-Semikolon, UTF-8 und deutsche Kommasetzung.
      return(read_csv2(fname,
                       locale = locale(
                         date_names = "de",
                         date_format = "%Y-%m-%d",
                         time_format = "%H:%M:%S",
                         decimal_mark = ",",
                         grouping_mark = ".",
                         encoding = "UTF-8",
                         asciify = FALSE
                       )))
    }
    
  }
}

#### Indexdaten holen ####
# alle _df Variablen aus der hinterlegten Datei lesen

vars_df <- config_df %>% filter(str_detect(name,"\\_df$"))
for (i in 1:nrow(vars_df)) {
  assign(vars_df$name[i],lies_daten(index_pfad,vars_df$value[i]))
} 

# Läufst du auf dem Server?
SERVER <- dir.exists("/home/jan_eggers_hr_de") 

#### Ergebnisse 2018 vorbereiten ####

# Vektor mit allen WK, die sich verändert haben: 
wk_veraendert_v <- c(wk_vergroesserungen_df %>%  pull(wk),
                     wk_verkleinerungen_df %>% pull(wk)) %>% 
  unique()


# Den simplen Teil zuerst: die "Frankentables" laden
#
# (Das sind die Kreis-Ergebnisse umgerechnet auf die neuen WK-Zuschnitte)
# Zusätzliche Variable veraendert gibt an, ob WK-Zuschnitt sich verändert hat

frankentable_direkt_df <- read.xlsx("ergebnisse2018/frankentabelle_direkt_2018.xlsx",
                                    sep.names = "_",
                                    check.names =T) %>% 
  mutate(veraendert = (wk %in% wk_veraendert_v))

# read.xlsx weigert sich, Leerzeichen in Variablennamen zu lesen. Deshalb der Unterstrich
# und hier die Umwandlung
colnames(frankentable_direkt_df) <- colnames(frankentable_direkt_df) %>% str_replace("_"," ")

frankentable_landesstimmen_df <- read.xlsx("ergebnisse2018/frankentabelle_landesstimmen_2018.xlsx",
                                           sep.names = "_",
                                           check.names =T) %>% 
  mutate(veraendert = (wk %in% wk_veraendert_v))

colnames(frankentable_landesstimmen_df) <- colnames(frankentable_landesstimmen_df) %>% 
  str_replace("_"," ")

# "Frankentabelle" (umgerechnete 2018er Ergebnisse, ergänzt) als Langformat.
frankentable_direkt_lang_df <- frankentable_direkt_df %>% 
  pivot_longer(cols=9:21,
               names_to="partei",
               values_to="stimmen_2018") %>% 
  select(wk,wahlberechtigte_2018 = wahlberechtigte,
         waehler_2018 = waehler,
         gueltig_2018 = gueltig,
         ungueltig_2018 = ungueltig,
         veraendert,
         partei,
         stimmen_2018) %>% 
  mutate(prozent_2018 = (stimmen_2018 / gueltig_2018 * 1000)/10)

frankentable_landesstimmen_lang_df <- frankentable_landesstimmen_df %>% 
  pivot_longer(cols=9:21,
               names_to="partei",
               values_to="stimmen_2018") %>% 
  select(wk,wahlberechtigte_2018 = wahlberechtigte,
         waehler_2018 = waehler,
         gueltig_2018 = gueltig,
         ungueltig_2018 = ungueltig,
         veraendert,
         partei,
         stimmen_2018) %>% 
  mutate(prozent_2018 = (stimmen_2018 / gueltig_2018 * 1000) /10)


# Gemeinde-Ergebnisse laden
e2018_df <- read_delim("ergebnisse2018/wahlergebnisse2.csv", 
                       delim = ";", escape_double = FALSE, 
                       locale = locale(date_names = "de", 
                                       decimal_mark = ",", 
                                       grouping_mark = ".", 
                                       encoding = "WINDOWS-1252"), 
                       trim_ws = TRUE, skip = 1) %>% 
  # Zeilen 1-2 enthalten Grütze, Zeile 3 enthält ganz Hessen
  filter(row_number()>2)

# Spalte 28 und 57 enthalten die V-Partei3
colnames(e2018_df) <- str_replace(colnames(e2018_df),"V-Partei3","V-Partei³")

### Spalten 14-32: Wahlkreisstimmen
direkt_2018_df <- e2018_df %>%  
  select(wk = 2,
         wk_name = 4, 
         AGS = 3,
         wahlberechtigt = 9,
         waehler = 10,
         wahlbeteiligung = 11,
         gueltig = 13,
         ungueltig = 12,
         ungueltig_prozent = 8, 
         14:32
  ) %>% 
  mutate(across(4:ncol(.), ~ as.numeric(.))) %>% 
  mutate(wahlbeteiligung = waehler / wahlberechtigt * 100,
         ungueltig_prozent = 100 - (gueltig / waehler * 100)) %>%
  # Sonderbedingung: Ganz Hessen = "Wahlkreis 0"
  mutate(wk = if_else(is.na(wk),0,wk))
  # Gemeindefusion Wesertal: 633021 und 633027 zusammen zu 633030

# Jetzt die Namen umfrickeln
colnames(direkt_2018_df) <- colnames(direkt_2018_df) %>% 
  str_replace("\\.\\.\\.[0-9]+$","")


wesertal_df <- direkt_2018_df %>% filter(AGS %in% c("633021","633027")) %>% 
  summarize(wk = first(wk),
            wk_name = "Wesertal",
            AGS = "633030",
            across(c(4:28), ~ sum(.))) %>% 
  mutate(wahlbeteiligung = waehler / wahlberechtigt * 100) %>% 
  mutate(across(c(4:28),~ as.integer(.)))

direkt_2018_df <- rbind(direkt_2018_df %>% 
  filter(!(AGS %in% c("633021","633027"))), wesertal_df)


# Gemeinden und Städte (hier: noch nicht getrennt)
gemeinden_direkt_2018_df <- direkt_2018_df %>% 
  filter(!is.na(AGS)) %>%
  # Stimmbezirks-Zeilen ausfiltern; die machen wir diesmal nicht
  filter(nchar(AGS) == 6) %>% 
  select(-wk_name) %>% 
  # vernünftige Gemeindenamen und Wahlkreisnamen reinjoinen
  left_join(gemeinden_alle_df %>% select(AGS,name,wk_name), by="AGS") %>% 
  relocate(AGS,name,wk_name) %>% 
  # Mehrfach-Einträge für Städte rauswerfen
  distinct(AGS,name,.keep_all=TRUE)

gemeinden_direkt_2018_lang_df <- gemeinden_direkt_2018_df %>% 
  pivot_longer(cols=c(11:29),names_to="partei",values_to="stimmen_2018") %>% 
  mutate(prozent_2018 = (stimmen_2018 / gueltig * 1000)/10)  
  
### Spalten 35-57: Landesstimmen
landesstimmen_2018_df <- e2018_df %>%  
  select(wk = 2,
         wk_name = 4,
         AGS = 3, 
         wahlberechtigt = 9,
         waehler = 10,
         wahlbeteiligung = 11,
         gueltig = 34,
         ungueltig = 33,
         ungueltig_prozent = 8, 
         35:57
  ) %>% 
  mutate(across(4:ncol(.), ~ as.numeric(.))) %>% 
  mutate(wahlbeteiligung = waehler / wahlberechtigt * 100,
         ungueltig_prozent = gueltig / waehler * 100) %>%
  # Sonderbedingung: Ganz Hessen = "Wahlkreis 0"
  mutate(wk = if_else(is.na(wk),0,wk))

# Jetzt die Namen umfrickeln
colnames(landesstimmen_2018_df) <- colnames(landesstimmen_2018_df) %>% 
  str_replace("\\.\\.\\.[0-9]+$","")  

g_wesertal_df <- landesstimmen_2018_df %>% filter(AGS %in% c("633021","633027")) %>% 
  summarize(wk = first(wk),
            wk_name = first(wk_name),
            AGS = "633030",
            across(c(4:32), ~ sum(.))) %>% 
  mutate(wahlbeteiligung = waehler / wahlberechtigt * 100)


landesstimmen_2018_df <- rbind(landesstimmen_2018_df %>% 
  filter(!(AGS %in% c("633021","633027"))),g_wesertal_df)
  



# Gemeinden und Städte (hier: noch nicht getrennt)
gemeinden_landesstimmen_2018_df <- landesstimmen_2018_df %>% 
  filter(!is.na(AGS)) %>%
  # Stimmbezirks-Zeilen ausfiltern; die machen wir diesmal nicht
  filter(nchar(AGS) == 6) %>% 
  select(-wk_name) %>% 
  # vernünftige Gemeindenamen und Wahlkreisnamen reinjoinen
  left_join(gemeinden_alle_df %>% select(AGS,name,wk_name), by="AGS") %>% 
  relocate(AGS,name,wk_name) %>% 
  # Mehrfach-Einträge für Städte rauswerfen
  distinct(AGS,name,.keep_all=TRUE)

gemeinden_landesstimmen_2018_lang_df <- gemeinden_landesstimmen_2018_df %>% 
  pivot_longer(cols=c(11:33),names_to="partei",values_to="stimmen_2018") %>% 
  mutate(prozent_2018 = (stimmen_2018 / gueltig * 1000)/10)

# Ganz Hessen? Ganz Hessen!
hessen_landesstimmen_lang_df <- landesstimmen_2018_df %>% 
  filter(wk == 0 & is.na(AGS)) %>% 
  select(-AGS) %>% 
  pivot_longer(cols = 9:31,
               names_to = "partei",
               values_to = "stimmen") %>% 
  mutate(prozent = (stimmen / gueltig * 1000)/10)


# Direktkandidaten

direktkandidaten_df <- kandidaten_alle_df %>% 
  filter(!is.na(wk)) %>% 
  rename(partei = Partei) %>% 
  left_join(parteien_idx_df %>% rename(p_id = id),by="partei") %>% 
  arrange(wk,p_id) %>% 
  mutate(name = paste0(Nachname," (",partei,")")) %>% 
  select(wk,wk_name = wkn,
         name,partei,farbwert,Titel,Nachname, Vorname, 
         Geburtsjahr, Geburtsort, Beruf,
         Listenplatz, Check_id)

#--- MISC ---
# Kassel, Darmstadt, Frankfurt, Wiesbaden - die KF mit mehr als einem Wahlkreis
# Offenbach wird als Gemeinde in einem Wahlkreis behandelt (der halt nur eine Gemeinde hat)
staedte_v <- c("611000","411000","412000","414000")

# Parteien, für die es Frankentable-Vergleichswerte gibt
v2018_v <- colnames(frankentable_direkt_df)[9:20]

         
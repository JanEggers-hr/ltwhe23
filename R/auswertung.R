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
TEST = FALSE
DO_PREPARE_MAPS = FALSE
REPUBLISH = FALSE

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



ts <- ts_daten
# Alles live mit Tabelle und 
live_df <- hole_daten(stimmbezirke_url)
stimmbezirke_n <- live_df %>% filter(Gebietstyp == "LD") %>% select(all_of(stimmbezirke_i)) %>% pull()

t_df <- live_df %>% filter(Gebietstyp == "VF") %>% 
  select(Gebietsschlüssel,Gebietsbezeichnung,freigegeben,
         gezaehlt = `Anzahl Wahlbezirke ausgezählt`,stimmbezirke = `Anzahl Wahlbezirke`) %>% 
  filter(freigegeben==0)
    #
live_hessen_landesstimmen_lang_df <- forme_hessen_landesstimmen(live_df)
live_kreise_direkt_lang_df <- forme_kreise_direkt(live_df)
live_kreise_landesstimmen_lang_df <- forme_kreise_landesstimmen(live_df)
live_gemeinden_direkt_lang_df <- forme_gemeinden_direkt(live_df)
live_gemeinden_landesstimmen_lang_df <- forme_gemeinden_landesstimmen(live_df)

#---- Hochburgen nach Partei ----

hessen_lang_df <- live_hessen_landesstimmen_lang_df %>% 
  select(partei,prozent,differenz)
gemeinden_parteien_df <- live_gemeinden_landesstimmen_lang_df %>% 
  mutate(ags= str_sub(gs,4,9)) %>% 
  mutate(wk = str_sub(gs,1,3) %>% as.integer()) %>% 
  select(ags,g_name,wk,
         wahlbeteiligung,
         partei,
         prozent,
         differenz
         ) %>% 
  group_by(partei) %>% 
  filter(partei %in% parteien_idx_df$partei[1:7])

hochburgen_df <- gemeinden_parteien_df%>% 
  ungroup() %>% 
  arrange(desc(prozent)) %>% 
  group_by(partei) %>% 
  slice(1:5) %>% 
  bind_rows(gemeinden_parteien_df %>% ungroup() %>% 
              arrange(prozent) %>% 
              group_by(partei) %>% 
              slice(5:1))


gewinn_verlust_df <- gemeinden_parteien_df %>% 
  group_by(partei) %>% 
  arrange(desc(differenz)) %>% 
  slice(1:3) %>% bind_rows(gemeinden_parteien_df %>% 
                             group_by(partei) %>% 
                             arrange(differenz) %>% 
                             slice(3:1)) %>% 
  ungroup() %>% 
  left_join(parteien_idx_df,by="partei") %>% 
  arrange(id) 

write.xlsx(gewinn_verlust_df,"analysen/gewinn_verlust.xlsx", overwrite=T)

parteien <- createWorkbook()
for (p in parteien_idx_df$partei) {
  addWorksheet(parteien,sheetName = p)
  tmp_df <- hochburgen_df %>% filter(partei==p) 
  writeData(parteien,sheet = p,tmp_df)
}
saveWorkbook(parteien,"analysen/hochburgen_schwachstellen_gemeinden.xlsx", overwrite = T)

kreise_parteien_df <- live_gemeinden_landesstimmen_lang_df %>% 
  mutate(wk= as.numeric(str_sub(gs,1,3))) %>% 
  select(wk,g_name,
         wahlbeteiligung,
         partei,
         prozent,
         differenz
  ) %>% 
  group_by(partei) %>% 
  filter(partei %in% parteien_idx_df$partei[1:7])

k_hochburgen_df <- kreise_parteien_df%>% 
  ungroup() %>% 
  arrange(desc(prozent)) %>% 
  group_by(partei) %>% 
  slice(1:5) %>% 
  bind_rows(gemeinden_parteien_df %>% ungroup() %>% 
              arrange(prozent) %>% 
              group_by(partei) %>% 
              slice(5:1))

parteien2 <- createWorkbook()
for (p in parteien_idx_df$partei) {
  addWorksheet(parteien2,sheetName = p)
  tmp_df <- k_hochburgen_df %>% filter(partei==p) 
  writeData(parteien2,sheet = p,tmp_df)
}
saveWorkbook(parteien2,"analysen/hochburgen_schwachstellen_gemeinden.xlsx", overwrite = T)

#--- Volatilität ----
# ...ist das Maß für die Veränderung im Wahlkreis/in der Gemeinde. 

# Volatilität der Ergebnisse
volatility <- function(p_v) {
  v <- sum(abs(p_v)) / 2 
} 

volatilität_df <- live_gemeinden_landesstimmen_lang_df %>% 
  mutate(ags= str_sub(gs,4,9)) %>% 
  select(ags,g_name,
         wahlbeteiligung,
         partei,
         prozent,
         differenz
  ) %>% 
  group_by(ags) %>% 
  summarize(g_name=first(g_name),
            wahlbeteiligung = first(wahlbeteiligung),
            volatilität = volatility(differenz)) %>% 
  arrange(desc(volatilität))

k_volatilität_df <- live_kreise_landesstimmen_lang_df %>% 
  select(wk,wk_name,
         wahlbeteiligung,
         partei,
         prozent,
         prozent_2018
  ) %>% 
  mutate(differenz = prozent-prozent_2018) %>% 
  select(-prozent_2018) %>% 
  group_by(wk) %>% 
  summarize(wk_name=first(wk_name),
            wahlbeteiligung = first(wahlbeteiligung),
            volatilität = volatility(differenz)) %>% 
  arrange(desc(volatilität))

volatil <- createWorkbook()
addWorksheet(volatil,sheetName = "Kreise")
addWorksheet(volatil,sheetName = "Gemeinden")
  writeData(volatil, sheet ="Kreise", k_volatilität_df)
  writeData(volatil, sheet = "Gemeinden", volatilität_df)

saveWorkbook(parteien,"analysen/hochburgen_schwachstellen_gemeinden.xlsx", overwrite = T)

for (p in parteien_idx_df$partei[1:7]) {
  choro_df <- gemeinden_parteien_df %>% 
    filter(partei == p) 
  write.xlsx(choro_df,paste0("analysen/choropleth_",p,".xlsx"))
}

choro_alle_df <- gemeinden_parteien_df %>% 
  left_join(volatilität_df %>% select(ags,volatilität),by="ags") %>% 
  mutate(ags=paste0("06",ags)) %>% 
  pivot_wider(names_from=partei,values_from=c(prozent,differenz)) 

write.xlsx(choro_alle_df,"analysen/choropleth_alle.xlsx")


# generiere-dummy-url.R

# legt für die TS eine Tabelle mit den URLs der Ergebnisartikel an


library(stringr)
library(dplyr)
library(tidyr)
library(openxlsx)

# Aktuelles Verzeichnis als workdir
setwd(this.path::this.dir())
# Aus dem R-Verzeichnis eine Ebene rauf
setwd("..")

sophora_str <- "https://www.hessenschau.de/politik/landtagswahl/ergebnisse/"
gemeinden_df <- read.xlsx("./gemeinden-kreise.xlsx",sheet="Gemeinden") %>%
  filter(Gesamtbevölkerung > 0) %>% 
  select(AGS,hr_name) %>% 
  mutate(hr_url = paste0(sophora_str,"ltwhe23-kommune-g06",AGS,"-ergebnis-100.html"))

write.xlsx(gemeinden_df,"./info/hessenschau_gemeinden_autotext_url.xlsx")

wahlkreise_df <- read.xlsx("./index/wahlkreise_alle.xlsx") %>% 
  select(wk, wk_name) %>% 
  arrange(wk) %>% 
  mutate(hr_url = paste0(sophora_str,"ltwhe23-wahlkreis-wk",
                         formatC(wk, width = 3,format="fg", flag="0"),
                         "-ergebnis-100.html"))

write.xlsx(wahlkreise_df,"./info/hessenschau_wahlkreise_autotext_url.xlsx")


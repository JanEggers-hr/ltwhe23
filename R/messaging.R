library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(teamr)

#' messaging.R
#' 
#' Kommunikation mit Teams
#' 
#' Webhook wird als URL im Environment gespeichert. Wenn nicht dort, dann Datei 
#' webhook_ltwhe.key im Nutzerverzeichnis ~/key/ einlesen.
#' MSG-Funktion schreibt alles in die Logdatei und auf den Bildschirm. (Vgl. Corona.)



# Webhook schon im Environment? 
if ((t_txt <- Sys.getenv("WEBHOOK_LTWHE")) == "") {
  t_txt <- read_file("~/key/webhook_ltwhe.key")
  Sys.setenv(WEBHOOK_LTWHE= t_txt)
} 

teams_meldung <- function(...,title="ltwhe-Update") {
  cc <- teamr::connector_card$new(hookurl = t_txt)
  if (TEST) {title <- paste0("TEST: ",title) }
  cc$title(paste0(title," - ",lubridate::with_tz(lubridate::now(),
                                                 "Europe/Berlin")))
  alert_str <- paste0(...)
  cc$text(alert_str)
  cc$print()
  cc$send()
} 

teams_error <- function(...) {
  alert_str <- paste0(...)
  teams_meldung("***FEHLER: ",...)
  stop(alert_str)
} 

teams_warning <- function(...) {
  alert_str <- paste0(...)
  teams_meldung("***WARNUNG: ",...)
  warning(alert_str)
} 


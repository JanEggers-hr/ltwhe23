#' aktualisiere_grafiken.R
#' 
#' Enthält die Funktionen: 
#' 

#---- Hauptfunktionen ----

#' aktualisiere_hessen_landesstimmen
#' 
#' Grafikausgabe Landesstimmen ganz Hessen (direkt über dw-Publish)
aktualisiere_hessen_landesstimmen <- function(lhll_df = live_hessen_landesstimmen_lang_df){
  stimmbezirke <- lhll_df %>% pull(stimmbezirke) %>% first()
  gezaehlt <- lhll_df %>% pull(gezaehlt) %>% first()
  waehler <- lhll_df %>% pull(waehler) %>% first()
  wahlberechtigt <- lhll_df %>% pull(wahlberechtigt) %>% first()
  ungueltig <- lhll_df %>% pull(ungueltig) %>% first()
  
  hessen_df <- lhll_df %>% 
    mutate(plusminus = ifelse(prozent==0,"(.)",
                              paste0("(",
                                     formatC(prozent-prozent_2018,
                                             digits=1,format="f", flag="+",
                                             big.mark = ".",decimal.mark = ","),
                                     ")"
                              ))) %>% 
    select(partei,prozent,plusminus)
  # Daten direkt hochladen
  dw_data_to_chart(hessen_df,chart_id = hessen_id)
  title = paste0("Hessen: Landesstimmen",ifelse(stimmbezirke == gezaehlt," - ERGEBNIS"," - TREND (bislang ausgezählte Stimmen)"))
  intro = paste0("Abgegebene Stimmen: ",
                 formatC(waehler,format="d",
                         decimal.mark=",", big.mark="."),
                 
                 ", davon ungültig: ",
                 formatC(ungueltig / waehler * 100,format="f",
                         decimal.mark=",", digits = 1, big.mark="."),
                 "%, ",
                 ifelse(stimmbezirke == gezaehlt,
                        paste0(
                          "Wahlbeteiligung: ",
                          formatC(waehler / wahlberechtigt * 100,format="f",
                                  decimal.mark=",", digits = 1, big.mark="."),
                          "%"),
                        ""),
                 
                 "<br><br>Zweitstimmen für die Parteien landesweit; auf Zehntel gerundet. in Klammern die Stimmanteile bei der Landtagswahl 2018, ",
                 "soweit vorhanden")
  notes = notes_text_auszaehlung(gezaehlt,
                                 stimmbezirke,
                                 ts,
                                 " Reihenfolge der Parteien wie auf dem Stimmzettel.<br><br>")
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
    waehler <- wahlkreis_df %>% pull(waehler) %>% first()
    wahlberechtigt <- wahlkreis_df %>% pull(wahlberechtigt) %>% first()
    ungueltig <- wahlkreis_df %>% pull(ungueltig) %>% first()
    wk <- i
    wk_name <- wahlkreis_df %>% pull(wk_name) %>% first()
    veraendert <- wahlkreis_df %>% pull(veraendert) %>% first()
    fname <- datawrapper_ids_df %>%  
      filter(typ == "w") %>% 
      filter(as.integer(id) == i) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      first()
    kand_df <- wahlkreis_df %>% 
      filter(wk == i) %>% 
      mutate(prozent = paste0(formatC(prozent,digits=1,format="f", 
                                      big.mark = ".",decimal.mark = ","),
                              "% (",
                              ifelse(partei %in% v2018_v,
                              formatC(differenz,digits=1,format="f",
                                      flag="+", big.mark = ".",decimal.mark = ","),
                              "?"),
                              ")"
                              )
             ) %>% 
      select(name,partei,stimmen,prozent) 
    write_csv(kand_df %>% head(5),paste0("livedaten/",
                                         fname,
                                         ".csv"))
    # Metadaten einrichten: 
    wahlbeteiligung_str <- paste0("Abgegebene Stimmen: ",
                                  formatC(waehler,format="d",
                                          decimal.mark=",", big.mark="."),
                  
                                  ", davon ungültig: ",
                                  formatC(ungueltig / waehler * 100,format="f",
                                          decimal.mark=",", digits = 1, big.mark="."),
                                  "%, ",
                                  ifelse(stimmbezirke == gezaehlt,
                                         paste0(
                                           "Wahlbeteiligung: ",
                                           formatC(waehler / wahlberechtigt * 100,format="f",
                                                   decimal.mark=",", digits = 1, big.mark=".")),
                                         ""),
      "<br><br>"
    )
    kand_str <- paste0("Weitere: ",
                       paste0(kand_df %>% 
                                tail(nrow(.)-5) %>% 
                                mutate(nn = paste0(name,": ",
                                           formatC(prozent, 
                                                   format = "f",
                                                   digits=1,
                                                   decimal.mark = ",",
                                                   big.mark = ".")))  %>% pull(nn),
                       collapse = ", "),"<br><br>")
    title <- paste0("Wahlkreis ",wk,
                    " - ",wk_name,
                    ": Stimmen fürs Direktmandat",
                    ifelse(gezaehlt == stimmbezirke,
                           " - ERGEBNIS",
                           " - TREND (bislang ausgezählte Stimmen)"))
    intro <- paste0(wahlbeteiligung_str,
                    "Erststimmen für die Wahl des Direktkandidaten im Wahlkreis - ",
                    "die derzeit fünf führenden Kandidatinnen und Kandidaten. ",
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018")
    notes <- notes_text_auszaehlung(gezaehlt,
                                    stimmbezirke, 
                                    ts,kand_str,
                                    ifelse(veraendert,
                                           "Der Zuschnitt des Wahlkreises hat sich seit der letzten Landtagswahl verändert. Ergebnisse 2018 umgerechnet auf den aktuellen Wahlkreiszuschnitt<br><br>",
                                           "")
                                    )

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
    forced_meta[["describe"]][["source-name"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-url"]] <- "https://wahlen.hessen.de/landtagswahlen"
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
    waehler <- kreis_df %>% pull(waehler) %>% first()
    wahlberechtigt <- kreis_df %>% pull(wahlberechtigt) %>% first()
    ungueltig <- kreis_df %>% pull(ungueltig) %>% first()
    # Dran denken: Bei Städten ist es mehr als einer, 
    wk <- w 
    wk_name <- kreis_df %>% pull(wk_name) %>% unique()
    veraendert <- kreis_df %>% pull(veraendert) %>% unique()
    fname <- datawrapper_ids_df %>%  
      filter(typ == "w") %>% 
      filter(as.integer(id) == w) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      last()
    liste_df <- kreis_df %>% 
      mutate(prozent = paste0(formatC(prozent,digits=1,format="f", 
                                      big.mark = ".",decimal.mark = ","),
                              " (",
                              ifelse(partei %in% v2018_v,
                                     formatC(differenz,digits=1,format="f", 
                                      flag="+", big.mark = ".",decimal.mark = ","),
                                     "?"),
                              ")")) %>% 
      select(partei,stimmen,prozent,wk)
    write_csv(liste_df,paste0("livedaten/",
                              fname,
                              ".csv"))
    wahlbeteiligung_str <- paste0("Abgegebene Stimmen: ",
                                  formatC(waehler,format="d",
                                          decimal.mark=",", big.mark="."),
                                  
                                  ", davon ungültig: ",
                                  formatC(ungueltig / waehler * 100,format="f",
                                          decimal.mark=",", digits = 1, big.mark="."),
                                  "%, ",
                                  ifelse(stimmbezirke == gezaehlt,
                                         paste0(
                                           "Wahlbeteiligung: ",
                                           formatC(waehler / wahlberechtigt * 100,format="f",
                                                   decimal.mark=",", digits = 1, big.mark="."),
                                           "%"),
                                         ""),
                                  "<br><br>"
    )
    # Metadaten einrichten: 
    title <- paste0("Wahlkreis ",wk,
                    " - ",wk_name,
                    ": Landesstimmen",
                    ifelse(stimmbezirke == gezaehlt,
                           " - ERGEBNIS",
                           " - TREND (bislang ausgezählte Stimmen)"))
    intro <- paste0(wahlbeteiligung_str,
                    "Zweitstimmen im Wahlkreis für die Parteien im neuen Landtag. ",
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018")
    notes <- notes_text_auszaehlung(gezaehlt,
                                    stimmbezirke, 
                                    ts,
                                    "in der Reihenfolge vom Stimmzettel<br><br>",
                                    ifelse(veraendert,
                                           "Der Zuschnitt des Wahlkreises hat sich seit der letzten Landtagswahlverändert. Ergebnisse 2018 umgerechnet auf den aktuellen Wahlkreiszuschnitt<br><br>",
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
    forced_meta[["describe"]][["source-name"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-url"]] <- "https://wahlen.hessen.de/landtagswahlen"
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
    waehler <- gemeinde_df %>% pull(waehler) %>% first()
    wahlberechtigt <- gemeinde_df %>% pull(wahlberechtigt) %>% first()
    ungueltig <- gemeinde_df %>% pull(ungueltig) %>% first()
    wk <- gemeinde_df %>% pull(wk) %>% unique() 
    g_name <- gemeinde_df %>% pull(g_name) %>% unique()
    fname <- datawrapper_ids_df %>%  
      filter(typ == "g") %>% 
      filter(id == g) %>% 
      pull(fname) %>%
      # direkt ist der erste der beiden möglichen Werte
      first()
    wahlbeteiligung_str <- paste0("Abgegebene Stimmen: ",
                                  formatC(waehler,format="d",
                                          decimal.mark=",", big.mark="."),
                                  
                                  ", davon ungültig: ",
                                  formatC(ungueltig / waehler * 100,format="f",
                                          decimal.mark=",", digits = 1, big.mark="."),
                                  "%, ",
                                  ifelse(stimmbezirke == gezaehlt,
                                         paste0(
                                           "Wahlbeteiligung: ",
                                           formatC(waehler / wahlberechtigt * 100,format="f",
                                                   decimal.mark=",", digits = 1, big.mark="."),
                                           "%"),
                                         ""),
                                  "<br><br>"
    )
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
                    ifelse(stimmbezirke == gezaehlt,
                           " - ERGEBNIS",
                           " - TREND (bislang ausgezählte Stimmen)"))
    intro <- paste0(wahlbeteiligung_str,
                    "Erststimmen in ",
                    g_name, 
                    " für die Direktkandidaten im Wahlkreis ",wk,". ",
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018"
                    )
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
    forced_meta[["describe"]][["source-name"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-url"]] <- "https://wahlen.hessen.de/landtagswahlen"
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
  fertig_push_str =tibble()
 
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
    stimmbezirke <- gemeinde_df %>% pull(stimmbezirke) %>% last()
    gezaehlt <- gemeinde_df %>% pull(gezaehlt) %>% first()
    waehler <- gemeinde_df %>% pull(waehler) %>% first()
    wahlberechtigt <- gemeinde_df %>% pull(wahlberechtigt) %>% first()
    ungueltig <- gemeinde_df %>% pull(ungueltig) %>% first()
    # Dran denken: Bei Städten ist es mehr als einer, 
    wk <- gemeinde_df %>% pull(wk) %>% unique() 
    g_name <- gemeinde_df %>% pull(g_name) %>% unique()
    fname <- datawrapper_ids_df %>%  
      filter(typ %in% c("g","s")) %>% 
      filter(id == g) %>% 
      pull(fname) %>%
      # landesstimmen ist der erste der beiden möglichen Werte
      last()
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
    wahlbeteiligung_str <- paste0("Abgegebene Stimmen: ",
                                  formatC(waehler,format="d",
                                          decimal.mark=",", big.mark="."),
                                  
                                  ", davon ungültig: ",
                                  formatC(ungueltig / waehler * 100,format="f",
                                          decimal.mark=",", digits = 1, big.mark="."),
                                  "%, ",
                                  ifelse(stimmbezirke == gezaehlt,
                                         paste0(
                                           "Wahlbeteiligung: ",
                                           formatC(waehler / wahlberechtigt * 100,format="f",
                                                   decimal.mark=",", digits = 1, big.mark="."),
                                           "%"),
                                         ""),
                                  "<br><br>"
    )
    title <- paste0(g_name,
                    ": Landesstimmen",
                    ifelse(stimmbezirke == gezaehlt,
                           " - ERGEBNIS",
                           " - TREND (bislang ausgezählte Stimmen)"))
    intro <- paste0(wahlbeteiligung_str,
                    "Zweitstimmen in ",g_name,
                    ", alle Wahllisten. ",
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018"
                    )
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
    forced_meta[["describe"]][["source-name"]] <- "Hessisches Statistisches Landesamt"
    forced_meta[["describe"]][["source-url"]] <- "https://wahlen.hessen.de/landtagswahlen"
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

#--- Grafikfunktionen ----
generiere_auszählungsbalken <- function(anz = gezaehlt,max_s = stimmbezirke_n,ts = ts) {
  fortschritt <- floor(anz/max_s*100)
  cat("generiere_ ",anz,max_s,ts,"\n")
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
#  cat(gezaehlt,stimmbezirke,ts,...,"\n")
  if (ts >= sd) {
    if (gezaehlt == stimmbezirke) {
      # Wenn alles ausgezählt: 
      return(paste0(...,"Auszählung beendet, alle ",stimmbezirke," Stimmbezirke ausgezählt"))
    } else {
      return(paste0(...,generiere_auszählungsbalken(anz = gezaehlt,
                                                    max_s = stimmbezirke,
                                                    ts)))
    }
  } else {
    return(paste0(...,
                  "Auszählung beginnt am ",
                  format(sd,"%A, %d. %B %Y, %H:%M Uhr")))
  }
}



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
    # Metadaten vom Vorbild holen!
    meta <- dw_retrieve_chart_metadata(dw_template1)
    viz <- meta$content$metadata$visualize
    # Balkengrafik
    viz$`custom-colors` <- farbliste
    viz$`x-grid-format` <- "0.0%"
    viz$`custom-range`[[1]] <- "0"
    viz$`custom-range`[[2]] <- "50"
    
    # Zweig pub holen
    meta <- dw_retrieve_chart_metadata(dw_id)
    pub <- meta$content$metadata$publish
    # Höhe setzen
    pub$`chart-height` <- 400
    
    # Visual-Metadaten hochladen
    dw_edit_chart(dw_id,visualize = viz,publish = pub)
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
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018"
                    )
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
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018"
                    )
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
    intro <- paste0("Erststimmen in ",gemeinde_name," für die Wahl des Direktkandidaten des Wahlkreises ", wk,". ",
                    "Prozentzahlen auf Zehntel gerundet; in Klammern: Vergleich zu 2018")
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

# Kopiere nur die Direktkandidaten der Kreise - 220 Dateien
aktualisiere_bucket_kreise <- function() {
  copy_time = NA
  if (SERVER) {
    n <- now()
    system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/wk*.* gs://d.data.gcp.cloud.hr.de/livedaten/')
    copy_time <- now()-n
    return(copy_time)
  } 
  fn_v <- list.files("livedaten/",pattern="wk*.*")
  if (!dir.exists("livedaten_backup/")) dir.create("livedaten_backup/")
  for (f in fn_v) {
    file.copy(paste0("livedaten/",f),
              paste0("livedaten_backup/",f))
    file.remove(paste0("livedaten/",f))
  }
  return(copy_time)
}

aktualisiere_bucket_gemeinden <- function() {
  copy_time = NA
  if (SERVER) {
    n <- now()
    system('gsutil -m -h "Cache-Control:no-cache, max_age=0" cp livedaten/g*.* gs://d.data.gcp.cloud.hr.de/livedaten/')
    copy_time <- now()-n
    return(copy_time)
  } 
  fn_v <- list.files("livedaten/",pattern="g*.*")
  if (!dir.exists("livedaten_backup/")) dir.create("livedaten_backup/")
  for (f in fn_v) {
    file.copy(paste0("livedaten/",f),
              paste0("livedaten_backup/",f))
    file.remove(paste0("livedaten/",f))
  }
  return(copy_time)
}


#---- Metadaten-Anpassungsfunktionen ----

#' metadaten_balken
#' 
#' Kopiert die Metadaten-Anpassungen aus der Parteien-Tabelle in die Balkengrafik, 
#' vor allem die Farben. 
#' 

# Vergleiche zwei DW-Metadaten-Sets

traverse_keys <- function(l) {
  # check if the current key is a list
  l_df <- tibble()
  l_v <- names(l)
  for (ll in l_v) {
    cat ("\n",ll," - ")
    if (is.list(l[[ll]])) {
      l_df <- l_df %>% bind_rows(tibble(name = ll,value =""))
      # Rekursiv mit Unterliste; an Tabelle anbinden
      l_df <- l_df %>% bind_rows(traverse_keys(l[[ll]]))
    } else {
      # Wert drucken 
      l_df <- l_df %>% bind_rows(tibble(name = ll,
                                        type = typeof(l[[ll]]),
                                        value = as.character(l[[ll]])
      )
      )
      cat(l[[ll]])
    }
  }
  return(l_df)
}

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

# Überträgt die geänderten Spaltennamen von Tabelle zu Tabelle
copy_column_names <- function(source_id,dw_id_v) {
  meta <- dw_retrieve_chart_metadata(source_id)
  dat_changes <- meta$content$metadata$data$changes
  for (id in dw_id_v) {
    meta <- dw_retrieve_chart_metadata(id)
    dat <- meta$content$metadtata$data
    dat$changes <- dat_changes
    dw_edit_chart(id, data = dat)
  }
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


fix_publishes <- function(id_v,h = 400) {
  for (did in id_v) {
    meta <- dw_retrieve_chart_metadata(did)
    pub <- meta$content$metadata$pub
    pub$blocks$blocks$`get-the-data` <- TRUE
    pub$blocks$enabled <- TRUE
    pub$blocks$`download-image` <- TRUE
    pub$`chart-height` <- h
    dw_edit_chart(chart_id = did, publish =  pub)
  }
}

fix_hide_wk <- function(id_v) {
  for (id in id_v) {
    meta <- dw_retrieve_chart_metadata(id)
    dat <- meta$content$metadata$data
    dat$`column-format`$wk$type <- 'auto'
    dat$`column-format`$wk$`number-append` <- ''
    dat$`column-format`$wk$`number-format` <- 'auto'
    dat$`column-format`$wk$`number-divisor` <- 0
    dat$`column-format`$wk$`number-prepend` <- ''
    
    dat$`column-format`$wk$ignore <- TRUE
    dw_edit_chart(chart_id = id, data = dat)
  }
}

#' republish()
#' 
#' Alle dw_id im Vektor nochmal neu veröffentlichen (was pro etwa 5-10s dauert)
republish <- function(id_v){
  for (id in id_v) {
    dw_publish_chart(chart_id = id)
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

#' fix_externalData
#' 
#' Löst ein Problem: Schreibt allen Grafiken den Verwieis auf die externen Daten
#' in die richtigen data-Keys und legt, vor allem, auf der obersten content-Ebene
#' den zusätzlichen Key externalData an
fix_dwcdn <- function(id_v) {
  for (did in id_v) {
    meta <- dw_retrieve_chart_metadata(did)
    fname <- datawrapper_ids_df %>% filter(dw_id == did) %>% pull(fname)
    if (length(fname) ==0 ) { stop ("Key ",id," nicht gefunden!") }
    ext_path <- paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                       fname,
                       ".csv")
    dat <- meta$content$metadata$data
    # Auf externes CDN umschalten
    dat$`use-datawrapper-cdn`<- FALSE
    # Keys für CSV- und JSON-Pfad anlegen/überschreiben
    dat$`upload-method`= "external-data"
    dat$`external-data` = ext_path
    dat$`external-metadata` = paste0("https://d.data.gcp.cloud.hr.de/livedaten/",
                                        fname,
                                        ".json")
    
    # Schreiben
    dw_edit_chart(chart_id = did, data = dat, externalData = ext_path)
  }
}

fix_parteifarben_landesstimmen <- function(id_v) {
  for (did in id_v) {
    fname <- datawrapper_ids_df %>% filter(dw_id == did) %>% pull(fname)
    if (length(fname) == 0) {warning (did," nicht in der Liste")} else {
      if (str_detect(fname,"direkt")) { warning(did," ist eine Direktmandat-Grafik!")}
    }
    meta <- dw_retrieve_chart_metadata(did)
    viz <- meta$content$metadata$visualize
    # Farbliste mit allen Partei-IDs - ruhig die lange
    farbliste <- setNames(as.list(parteien_idx_df$farbwert), 
                          parteien_idx_df$partei)
    viz[["columns"]][["stimmen"]][["customColorBarBackground"]] <- farbliste
    viz[["columns"]][["stimmen"]][["customBarColorBy"]] <- "partei"
    dw_edit_chart(chart_id = did, visualize = viz)
  }
}

change_to_table <- function(id_v) {
  for (did in id_v) {
    dw_edit_chart(chart_id = did, type = "tables")
  }
}

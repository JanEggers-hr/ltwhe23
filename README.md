# sophoRa  
Ein R-Package für das Erstellen und Bearbeiten von Sophora-Elementen und 
Artikeln.  

## Funktionsweise
Sophora-Artikel und Elemente werden als XML-Dokumente gespeichert und an eine
Import-API geschickt, die diese Dokumente dann in die Sophora-Datenbank einträgt.  
Dabei gibt es zwei Schnittstellen, sogenannte Importer.  
`dev` (die Entwicklungsumgebung und default-Einstellung in den Funktionen von 
`sophoRa`) und `prod` (die Produktionsumgebung).  
Hinter den Kulissen arbeitet in `sophoRa` das R-Package [xml2](https://xml2.r-lib.org/).  
Dabei wird ein XML-Dokument mit einem [`external_pointer`](https://cran.r-project.org/web/packages/future/vignettes/future-4-non-exportable-objects.html) genutzt. Das führt dazu, dass alle Änderungen an einem eingelesenen Dokument in
diesem `external_pointer` durchschlagen, auch wenn sie in einer Kopie des 
eingelesenen Dokuments geschehen, da diese Kopie ebenfalls auf den `external_pointer` 
verweist.    

Ein Sophora-Artikel besteht aus verschiedenen Absätzen (childNodes), die 
entweder selbst den Inhalt besitzen (Überschriften und Absätze) oder Referenzen
auf Sophora-Objekte haben (Bilder, Drittplattform-Elemente wie Datawrapper oder 
das Datenjournalismus-Element).  
Die Referenzen werden immer über deren Sophora-Id (external_id) erkannt und
eingebunden.  

### Erstellen von Sophora-Objekten  
Sophora-Objekte wie Bilder, Datawrapper-Elemente, Datenjournalismus-Elemente usw.
werden mit den `create_...()` Funktionen erstellt.  
Jedes Element wird dabei mit seiner eindeutigen `external_id` in die 
`prod-Tabelle` in der Bigquery-Datenbank geschrieben.  

#### Schreiben eines Sophora-XML
Mit der Funktion `write_sophora_xml()` kann ein XML-Dokument in eine lokale 
Datei geschrieben werden.  

#### Publizieren eines Sophora-XML  
Mit der Funktion `publish_to_sophora()` wird ein XML-Dokument zum Importer von
Sophora geschickt und damit in der Entwicklungs- oder Produktionsumgebung 
veröffentlicht.  
Jedes importierte Objekt hat dabei einen Publikationsstatus:  
* preview - Vorschau des Artikels  
* setOffline - Offline nehmen des Artikels  
* publish - Veröffentlichen des Artikels    

Der Publikationsstatus wird über die Funktion `set_published_state()` gesetzt.  
__ACHTUNG:__  
Jedes Dokument in Sophora besitzt eine eindeutige ID, die Sophora-ID oder 
`external_id`. Werden zwei unterschiedliche Dokumente mit demselben Artikelstamm
(idstem) veröffentlicht, wird die Zahl am Ende der Url um 2 hochgezählt, d.h. 
die Url lautet dann nicht mehr "idstem~100.html" sondern "idstem~102.html" usw.  

## Erstellen eines neuen Sophora-Artikels  
Ein neuer leerer Sophora-Artikel wird mit der Funktion `create_article()` 
erstellt.  
Damit wird ein leeres Template als XML-Dokuemnt `doc` geladen, dessen Metadaten
noch mit folgenden Funktionen gefüllt werden können:  
```
  # Setzt den Titel und die Seo-Url  
  doc <- set_headline(doc, headline)
  
  # Setzt die diversen Titel-Angaben in den Metadaten des Artikels  
  doc <- set_meta_headline(doc, headline)

  # Setzt den Text des Teasers und den Text auf Drittplattformen  
  doc <- set_teaser_text(doc, text)
  
  # Setzt die Dachzeile eines Artikels und dem zugehörigen Teaser  
  doc <- set_topline(doc, topline)
  
``` 
Weitere Meta-Angaben wie bspw. "placemark" oder "leadText" können mit der 
Funktion `set_meta_headline()` gesetzt werden:  
```
doc <- set_article_metafield(doc, "swr:placemark", "Baden-Baden")
```

## Bearbeiten eines Sophora-Artikels  
Um ein vorhandenen Artikel zu bearbeiten ist es notwendig seine Struktur zu 
kennen, da über die Position im XML-Baum die Elemente erkannt bzw. platziert 
werden. Hier wird bspw. der Artikel "test.xml" eingelesen und die 
Artikel-Struktur angezeigt:  
```
library(sophoRa)
doc <- xml2::read_xml("test.xml")
show_article_structure(doc)
```
![Artikel-Struktur](vignettes/img/show_article_structure.png)

### Url des Artikels ändern  
Die Url eines Artikels ist im XML als `idstem` hinterlegt. Mit der Funktion 
`set_idstem()` kann sie geschrieben und verändert werden. Besitzen zwei 
Dokumente denselben `idstem` wird die Url hochgezählt (siehe oben unter 
Publizieren).  

### Bearbeitungsdatum ändern  
Mit `set_editing_date()` wird das Bearbeitungsdatum des XML-Dokuments auf die 
aktuelle Systemzeit gesetzt.  
Dieser Zeitstempel wird auch als "Stand" im Sophora-Frontend ausgespielt.  

### Titel (Headline)  
Mit `set_headline()` wird der Titel und die Seo-Url festgelegt.

### Füge neue Elemente ein  
Mit den `add_...()` Funktionen werden die verschiedenen Elemente in den XML-Baum
eines Sophora-Artikels als "childNode" eingefügt.  
Standardmäßig werden diese Elemente immer am Ende eingefügt. Mit dem Argmuent 
`pos` kann eine spezifische Position (Absatz) im XML-Baum angegeben werden.  
So wird bspw. ein neuer Absatz an die dritte Stelle des XML-Baums eingefügt.
```
library(sophoRa)
doc <- xml2::read_xml("test.xml")
show_article_structure(doc)
text <- "Ich bin der Text eines neuen Absatzes mit viel Inhalt."
add_paragraph(doc, text, pos = 3)
```

### Lösche Elemente  
Um ein Absatz-Element (Bild, Datawrapper, Überschrift, Textabsatz etc.) aus 
einem Artikel zu entfernen wird die Funktion `remove_element_in_tree()` benutzt.  
Hier muss ebenfalls die Position des zu löschenden Elements als Parameter 
gesetzt werden.  
Hier wird bspw. das Element im zweiten Absatz aus dem Artikel entfernt.  
```
library(sophoRa)
doc <- xml2::read_xml("test.xml")
show_article_structure(doc)
remove_element_in_tree(doc, pos = 2)
```

### Elemente aktualisieren oder austauschen  
Um Elemente in Sophora-Objekten auszutauschen werden die `update_...()` 
Funktionen genutzt.  
Sie aktualisieren entweder direkt Sophora-Objekte (Drittplattform-Objekte, 
Bilder etc.) oder tauschen den Inhalt eines Absatzes aus (Überschriften, 
Textabsätze).  
Eine Überschrift im zweiten Absatz ändern:  
```
doc <- xml2::read_xml("test.xml")
show_article_structure(doc)
update_heading(doc, text = "TEST TEST TEST", level = 4, pos = 2)

```
Das bestehende Bild unter der Sophora-Id (external_id) `energy-plot-from-nuclear-source` 
wird durch das lokale `new_image.jpg` ausgetauscht:  
```
update_image_element("new_image.jpg",
                     external_id = "energy-plot-from-nuclear-source",
                     structurenode = "/swraktuell",
                     alt_text = "Anteil der Atomenergie an der Gesamtstromerzeugung am 20.11.2022")
```


## Referenzen  
Offizielle Importer Anleitung  
https://subshell.com/docs/4/importer/importxml/importer216.html  
Sophora Glossar von SWR Online  
https://swrlearnit-suedwestrundfunk.msappproxy.net/ilias.php?baseClass=ilGlossaryPresentationGUI&ref_id=217



<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📦 zhapir <a href="https://github.com/openZH/zhapir"><img src="man/figures/zhapir_hex.png" alt="zhapir Hex-Sticker" align="right" height="138"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/openZH/zhapir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openZH/zhapir/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

**zhapir** ist ein R-Paket für die automatisierte Erstellung und
Aktualisierung von **Datensätzen** und **Distributionen** in der
kantonalen *Metadatenverwaltung (MDV)*.  

Damit können Inhalte für [zh.ch/opendata](https://zh.ch/opendata) und
[opendata.swiss](https://opendata.swiss) effizient gepflegt werden.

## 🚀 Installation

Das Paket wird über GitHub installiert:

``` r
# install.packages("remotes")
remotes::install_github("openZH/zhapir")
```

## 🔑 API Key einrichten

Ein API Key ist Voraussetzung, um mit der MDV zu arbeiten.  
Diesen erzeugst du in der GUI:

1.  🔐 **API Key generieren**

    ![](man/figures/Token_generieren.png)

    Hier könnt ihr einen beliebigen Namen für den Token wählen. Es ist
    möglich einen Token ohne Ablaufdatum zu genieren (einfach
    Ablaufdatum frei lassen).

2.  In `.Renviron` eintragen:

    ``` r
    # Dies öffnet deinen .Renviron file
    usethis::edit_r_environ()
    ```

    Den API-Key als ZHAPIR_API_KEY eintragen. Hier ist der Namen
    wichtig, da der Key automatisch vom Package ausgelesen wird.

    ``` r
    ZHAPIR_API_KEY="xxxxxxxxxxxxxxxx"
    ```

    ❗**Danach die R-Session neu starten, damit die Änderung wirksam
    wird.** ❗

## ⚠️ Wichtig: Entwicklungs- vs. Produktions-Umgebung

Standardmässig verwendet `zhapir` die **Entwicklungs-URL**
(`use_dev = TRUE`).  
Für produktive Änderungen muss explizit `use_dev = FALSE` gesetzt
werden:

``` r
ds <- zhapir::create_dataset(
  title           = "Prod Datensatz",
  organisation_id = 14,
  description     = "In PROD erstellt",
  use_dev         = FALSE   # 👉 PROD statt DEV --> zeigt auf "https://dev.mdv.statistik.zh.ch"
)
```

> [!NOTE] 
> Wenn mit dem DEV-Umgebung des MDV gearbeitet werden soll
> (z.B. um Änderungen zu an Dataset/Distribution zu testen), muss der
> DEV-API Key explizit mitgegeben werden. Wenn `api_key` leer gelassen
> wird, versucht die Funktion im Hintergrund `ZHAPIR_API_KEY` aus dem
> `.Renviron` zu lesen. Hier empfehlen wir den API-Key für die
> Produktivumgebung abzulegen. DEV und PROD haben unterschiedliche
> API-Keys. Dies führt zu einem Fehler, wenn `use_dev = FALSE` und der
> PROD API-Key angezogen wird.

## ✨ Beispiele

### Datensatz erstellen

``` r
ds <- zhapir::create_dataset(
  title           = "Beispiel Datensatz",
  organisation_id = 14,
  description     = "Automatisiert erstellt mit zhapir",
  contact_email   = "team@example.org",
  theme_ids       = c("Verkehr"),
  periodicity_id  = "Jährlich",
  start_date = "2020-01-01", # 🟢 Startdatum ist für Veröffentlichung erforderlich
  use_dev         = FALSE
)
```

ℹ️ Neue Datensätze sind in der MDV **immer „Entwurf“**.  
Eine Publikation ist nur über die grafische Oberfläche möglich und
erfolgt immer erst nach der Prüfung durch die Data Guides. Neue
Distributionen anlegen oder bestehende Distributionen updaten lässt sich
aber vollständig über die API/R erledigen.

ℹ️ Es ist nicht notwendig das Ergebnis der Funktionen (z.B.
`zhapir::create_distribution()`) per `<-` zuzuweisen. Wir nutzen dies
hier, um mit der ID eines Datensatzes oder einer Distribution
weiterzuarbeiten.

🟢 Nach dem Erstellen oder Aktualisieren prüft `zhapir` automatisch, ob
der Datensatz valid für den nächsten Status ist (z. B. ob Pflichtfelder
wie `start_date` oder `keywords` gesetzt sind). Fehlende Felder werden
im CLI mit entsprechenden Hinweisen ausgegeben.

### Distribution hinzufügen

``` r
# Temporär CSV erstellen 
tmpfile <- base::tempfile(fileext = ".csv")
utils::write.csv(data.frame(a = 1:3), tmpfile, row.names = FALSE)

# Neue Distribution mit angehängter Datei hochladen
dist <- zhapir::create_distribution(
  title       = "Beispiel Distribution",
  dataset_id  = ds$id, # Das funktioniert, weil wir oben ds erstellt haben - sonst einfach im GUI die ID (=Nummer) heraussuchen.
  file_path   = tmpfile,
  ogd_flag    = TRUE,
  zh_web_flag = TRUE,
  license_id = 1, # 🟢 Lizenz-ID (siehe unten)
  use_dev     = FALSE
)
```

🟢 Lizenztypen (license_id)

| ID | Bedeutung | Entspricht |
|----|----|----|
| 1 | kommerzielle & nicht-kommerzielle Nutzung **mit Quellenangabe** | ≈ CC BY 4.0 |
| 2 | kommerzielle & nicht-kommerzielle Nutzung **ohne Quellenangabe** | ≈ CC0 1.0 Public Domain |

👉 **Kniff:** Über `update_distribution()` kannst du auch **Parameter
auf Dataset-Ebene** anpassen, ohne separat `update_dataset()` aufzurufen
– z. B. `end_date` oder `modified_next`:

``` r
dist <- zhapir::update_distribution(
  id            = dist$id,
  modified_next = "2026-01-01", # nächstes geplantes Update
  end_date      = "2025-12-31", # Ende der Zeitspanne
  use_dev       = FALSE
)
```

### Datensatz aktualisieren

``` r
ds <- zhapir::update_dataset(
  id            = ds$id,
  description   = "Neue Beschreibung",
  modified_next = "2026-01-01",
  use_dev       = FALSE
)
```

### Distribution aktualisieren

``` r
dist <- zhapir::update_distribution(
  id          = dist$id,
  description = "Neue Beschreibung Distribution",
  use_dev     = FALSE
)
```

## 🔍 IDs: Labels ↔︎ Codes

Viele Argumente akzeptieren **Labels** (z. B. `"Bevölkerung"`) oder
**IDs** (z. B. `41`):

``` r
# via Label
ds1 <- zhapir::create_dataset(
  title           = "Per Label",
  organisation_id = 14,
  theme_ids       = "Verkehr",
  use_dev         = FALSE
)

# via ID
ds2 <- zhapir::create_dataset(
  title           = "Per ID",
  organisation_id = 14,
  theme_ids       = 41,
  use_dev         = FALSE
)
```

Bei Tippfehlern gibt `zhapir` klare Fehlermeldungen mit Hinweisen, z.
B.:

``` r
x befölkerung not valid
• run get_themes()
```

Folgende Argumente akzeptieren **Labels** und **IDs**:

- `keyword_ids`
- `zh_web_datacatalog_ids`
- `theme_ids`
- `periodicity_id`
- `status_id`
- `file_format_id`

Folgende Argumente akzeptieren ausschliesslich IDs:

- `organisation_id`
- `dataset_id`
- `license_id`

Mit den `get_[Argument]`-Funktionen können die verfügbaren Labels sowie
die dazugehörigen IDs aufgerufen werden, z.B. für `themes`:

``` r
# Finde alle `themes`
zhapir::get_themes()

# Finde alle `themes`, welche den den String "Bevölkerung" enthalten (können auch mehrere Strings sein)
zhapir::get_themes("Bevölkerung")

# Finde alle `themes`, welche die ID 41 enthalten (können auch mehrere IDs sein)
zhapir::get_themes(41)
```

> **Wichtig**: `get_organisations()` gibt ausschliesslich alle
> Organisationen zurück und erlaubt keine Strings oder ID als Input.

## 🚫 Einschränkungen

Das Paket kann **nicht**:

- den **Status** eines Datensatzes setzen (neue Datensätze sind immer
  „Entwurf“)

- eine **Publikation** anstossen

Diese Schritte erfolgen ausschliesslich über die grafische Oberfläche
des Datenkatalogs.

## 📄 Lizenz

Codelizenz: © 2025 Statistisches Amt Kanton Zürich (siehe `LICENSE`)

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(data.table)
library(slider)
library(kableExtra)
library(formattable)
library(readxl)
library(writexl)
library(stringi)
library(collapse)
library(shinyalert)
library(shinydisconnect)
library(fst)

options(shiny.maxRequestSize = 1000 * 1024 ^ 2,
        scipen = 999)

setDTthreads(threads = 0L)

zaokraglij_daty_do_sezonow <- function(daty) {
  as.Date(ifelse(month(daty) %in% c(10L, 11L, 12L),
                 as_date(ISOdate(year(daty) + 1L, 9L, 30L)),
                 as_date(ISOdate(year(daty), 9L, 30L))),
          origin = "1970-01-01")
}

wartosci_zmiennej <- function(zmienna, baza) {
    wartosci <- baza$wartość[baza$zmienna == zmienna]
    if (any(stri_detect_regex(wartosci, "[a-zA-Z]"))) {
        stri_c(stri_c('"', sort(unique(wartosci)), '"', sep = ""), collapse = ", ")
    } else {
        wartosci <- as.numeric(wartosci)
        stri_c("min. ", min(wartosci), ", max. ", max(wartosci), collapse = "")
    }
}

wypelnij_braki_danych <- function(zmienne_do_brakow, baza_rolnikow, baza_kombinacje) {
    baza_rolnikow <- baza_rolnikow[grep(zmienne_do_brakow, zmienna_data)]
    baza_kombinacje <- baza_kombinacje[grep(zmienne_do_brakow, zmienna_data)]
    baza_cala <- baza_rolnikow[baza_kombinacje, roll = TRUE, rollends = c(TRUE, TRUE)]
    baza_cala
}


sprawdzenie_numeric <- function(plik) {
  if (!all(is.na(plik))) {
    plik <- plik[!is.na(plik)]
    all(stri_detect_regex(plik, "^\\d+$"))
  } else {
    FALSE
  }
}

filtruj_czesci_bazy <- function(kolumny_lista, baza_rolnikow, filtr) {
    baza_rolnikow <- baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[grep(stri_c(c(kolumny_lista, "^telefon_0$"), collapse = "|"), names(baza_rolnikow))]]
    
    names(baza_rolnikow)[grep("_\\d{4}_\\d{2}_\\d{2}$", names(baza_rolnikow))] <- stri_replace_all_regex(names(baza_rolnikow)[grep("_\\d{4}_\\d{2}_\\d{2}$", names(baza_rolnikow))], "_\\d{4}_\\d{2}_\\d{2}$", "")
    
    baza_zaakceptowana <- baza_rolnikow[eval(parse(text = filtr))]
    baza_zaakceptowana <- baza_zaakceptowana[, rezultat := TRUE]
    baza_odrzucona <- baza_rolnikow[!eval(parse(text = filtr))]
    baza_odrzucona <- baza_odrzucona[, rezultat := FALSE]
    
    baza <- rbindlist(list(baza_zaakceptowana, baza_odrzucona), use.names = FALSE)
    baza
}

czy_brak_dziur_w_kolumnach_z_telefonami <- function(kolumny_z_telefonami) {
  najpozniejszy_ktory_nie_ma_braku <- max(which(stri_detect_regex(kolumny_z_telefonami, "^brak$", negate = TRUE)))
  najwczesniejszy_ktory_ma_brak <- min(which(stri_detect_regex(kolumny_z_telefonami, "^brak$")))
  if (najwczesniejszy_ktory_ma_brak < najpozniejszy_ktory_nie_ma_braku) {
    FALSE
  } else {
    TRUE
  }
}

czy_brak_dubli_w_telefonach_dla_tego_samego_kontaktu <- function(kolumny_z_telefonami) {
  kolumny_z_telefonami <- kolumny_z_telefonami[stri_detect_regex(kolumny_z_telefonami, "^brak$", negate = TRUE)]
  kolumny_z_telefonami <- stri_sub(kolumny_z_telefonami, 1, 9)
  if (any(duplicated(kolumny_z_telefonami))) {
    kolumny_z_telefonami[1]
  } else {
    NULL
  }
}

ustal_ile_dni_do_uzycia_dla_importowanej_bazy <- function(wczytana_baza_telefon_0, baza_rolnikow_unikalne) {
  baza_rolnikow_unikalne <- baza_rolnikow_unikalne[telefon_0 == wczytana_baza_telefon_0]
  if (baza_rolnikow_unikalne[, .N] > 0) {
    if (baza_rolnikow_unikalne$ile_dni_do_użycia == 0L) {
      0L
    } else {
      baza_rolnikow_unikalne$ile_dni_do_użycia
    }
  } else {
    0L
  }
}

zredukuj_liste_zdublowanych_id <- function(zdublowane_id_jedno, zdublowane_id_wszystkie) {
  zdublowane_id_jedno_wektor <- unlist(stri_split_fixed(zdublowane_id_jedno, ", "), use.names = FALSE)
  zdublowane_id_calosc <- map(zdublowane_id_jedno_wektor, ~ ifelse(stri_detect_regex(zdublowane_id_wszystkie, stri_c("^", ., ",", "|", "\\s", ., ",", "|", "\\s", ., "$")), stri_c(stri_c(zdublowane_id_jedno_wektor, collapse = ", "), zdublowane_id_wszystkie, sep = ", "), NA))
  zdublowane_id_calosc <- sort(unique(unlist(stri_split_fixed(unlist(zdublowane_id_calosc, use.names = FALSE), ", "), use.names = FALSE)))
  zdublowane_id_calosc <- stri_c(zdublowane_id_calosc[!is.na(zdublowane_id_calosc)], collapse = ", ")
  zdublowane_id_calosc
}

ujednolic_informacje_dla_zdublowanych_id <- function(baza_rolnikow_telefony_id_duble, baza_rolnikow) {
  id_do_filtrowania <- unlist(stri_split_fixed(baza_rolnikow_telefony_id_duble, ", "), use.names = FALSE)
  baza_rolnikow_zdublowane <- baza_rolnikow[id %in% id_do_filtrowania]
  baza_rolnikow_zdublowane[, ile_dni_do_użycia := if (all(ile_dni_do_użycia == 0L)) 0L else max(unlist(ile_dni_do_użycia, use.names = FALSE))]
  baza_rolnikow_zdublowane <- unique(baza_rolnikow_zdublowane, by = "zmienna_data")
  baza_rolnikow_zdublowane_nazwy <- unique(unlist(lapply(baza_rolnikow_zdublowane[, .SD, .SDcols = names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^nazwa$")]], function(x) x), use.names = FALSE))
  baza_rolnikow_zdublowane_nazwy <- baza_rolnikow_zdublowane_nazwy[baza_rolnikow_zdublowane_nazwy != "brak"]
  if (length(baza_rolnikow_zdublowane_nazwy) == 0L) {
    baza_rolnikow_zdublowane_nazwy <- "brak"
  } else if (length(baza_rolnikow_zdublowane_nazwy) > 1L) {
    baza_rolnikow_zdublowane_nazwy <- first(baza_rolnikow_zdublowane_nazwy)
  } else {
    baza_rolnikow_zdublowane_nazwy <- baza_rolnikow_zdublowane_nazwy
  }
  baza_rolnikow_zdublowane_maile <- unique(unlist(lapply(baza_rolnikow_zdublowane[, .SD, .SDcols = names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^email$")]], function(x) x), use.names = FALSE))
  baza_rolnikow_zdublowane_maile <- baza_rolnikow_zdublowane_maile[baza_rolnikow_zdublowane_maile != "brak"]
  if (length(baza_rolnikow_zdublowane_maile) == 0L) {
    baza_rolnikow_zdublowane_maile <- "brak"
  } else if (length(baza_rolnikow_zdublowane_maile) > 1L) {
    baza_rolnikow_zdublowane_maile <- first(baza_rolnikow_zdublowane_maile)
  } else {
    baza_rolnikow_zdublowane_maile <- baza_rolnikow_zdublowane_maile
  }
  baza_rolnikow_zdublowane[, nazwa := baza_rolnikow_zdublowane_nazwy]
  baza_rolnikow_zdublowane[, email := baza_rolnikow_zdublowane_maile]
  wszystkie_telefony <- unique(unlist(lapply(baza_rolnikow_zdublowane[, .SD, .SDcols = names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^telefon_[1-9]+$")]], function(x) x), use.names = FALSE))
  wszystkie_telefony <- wszystkie_telefony[wszystkie_telefony != "brak"]
  roznica <- length(wszystkie_telefony) - length(names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^telefon_[1-9]+$")])
  if (roznica > 0) {
    wszystkie_telefony <- wszystkie_telefony[1:length(names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^telefon_[1-9]+$")])]
    baza_rolnikow_zdublowane[, names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^telefon_[1-9]+$")] := lapply(wszystkie_telefony, function(x) rep(x, baza_rolnikow_zdublowane[, .N]))]
  } else {
    roznica <- abs(roznica)
    braki <- rep("brak", roznica)
    wszystkie_telefony <- c(wszystkie_telefony, braki)
    baza_rolnikow_zdublowane[, names(baza_rolnikow_zdublowane)[stri_detect_regex(names(baza_rolnikow_zdublowane), "^telefon_[1-9]+$")] := lapply(wszystkie_telefony, function(x) rep(x, baza_rolnikow_zdublowane[, .N]))]
  }
  baza_rolnikow_zdublowane[, telefon_0 := as.integer(stri_sub(telefon_1, 1L, 9L))]
  wojewodztwa <- unique(pull(baza_rolnikow_zdublowane[zmienna == "województwo"], wartość))
  wojewodztwa <- wojewodztwa[wojewodztwa != "brak"]
  if (length(wojewodztwa) > 0) {
    baza_rolnikow_zdublowane <- baza_rolnikow_zdublowane[(zmienna == "województwo" & data == max(pull(baza_rolnikow_zdublowane[zmienna == "województwo" & wartość != "brak"], data))) |  zmienna != "województwo"]
  }
  baza_rolnikow_zdublowane_statusy <- baza_rolnikow_zdublowane[(zmienna == "status" & data %in% head(sort(pull(baza_rolnikow_zdublowane[zmienna == "status"], data), decreasing = TRUE), 20L))]
  baza_rolnikow_zdublowane <- baza_rolnikow_zdublowane[zmienna != "status"]
  baza_rolnikow_zdublowane <- baza_rolnikow_zdublowane[order(-data)]
  baza_rolnikow_zdublowane <- unique(baza_rolnikow_zdublowane, by = "zmienna_sezon")
  baza_rolnikow_zdublowane_statusy <- baza_rolnikow_zdublowane_statusy[order(-data)]
  baza_rolnikow_zdublowane_statusy <- unique(baza_rolnikow_zdublowane_statusy, by = "zmienna_data")
  baza_rolnikow_zdublowane_statusy <- first(baza_rolnikow_zdublowane_statusy, 20L)
  baza_rolnikow_zdublowane <- rbindlist(list(baza_rolnikow_zdublowane, baza_rolnikow_zdublowane_statusy), use.names = FALSE)
  baza_rolnikow_zdublowane
}

baza_rolnikow <- reactiveVal()

wczytaj_baze <- reactive({
  invalidateLater(100)
  if (is.null(baza_rolnikow())) {
    if (file.exists("Baza_danych/Baza/baza_rolnikow.fst")) {
      baza_rolnikow <- read_fst("Baza_danych/Baza/baza_rolnikow.fst", as.data.table = TRUE)
      baza_rolnikow(baza_rolnikow)
    } else {
      baza_rolnikow <- data.table(telefon_0 = 0L,
                                  telefon_1 = "0",
                                  nazwa = "0",
                                  email = "0",
                                  zmienna = "0",
                                  opis = "0",
                                  data = ymd("1970-01-01"),
                                  sezon = ymd("1970-01-01"),
                                  zmienna_data = "0",
                                  zmienna_sezon = "0",
                                  wartość = "0",
                                  ile_dni_do_użycia = 0L,
                                  zmienna_bez_historii = FALSE)
      setkey(baza_rolnikow, telefon_0)
      baza_rolnikow <- baza_rolnikow[FALSE]
      write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
      baza_rolnikow(baza_rolnikow)
    }
  }
})

observe(wczytaj_baze())

lista_zmiennych <- reactive({
  baza_rolnikow <- baza_rolnikow()
  zmienne <- pull(baza_rolnikow[, .(zmienna = sort(unique(zmienna)))], zmienna)
  zmienne <- zmienne[!zmienne %chin% c("województwo", "status")]
  zmienne
})

observe(lista_zmiennych())

stworz_kopie_automatyczne <- reactive({
    invalidateLater(1000*60*60*23*1)
    obecna_data <- Sys.Date()
    kopie <- ymd(dir("Baza_danych/Kopia_automatyczna"))
    if (length(kopie) == 0) {
        ostatnia_kopia <- obecna_data - days(14)
    } else if (is.na(max(kopie))) {
        ostatnia_kopia <- obecna_data - days(14)
    } else {
        ostatnia_kopia <- max(kopie)
    }
    if (as.numeric(as.duration(interval(ostatnia_kopia, obecna_data)), "days") >= 14) {
      if (file.exists("Baza_danych/Baza/baza_rolnikow.fst")) {
        dir.create(path = stri_c("Baza_danych/Kopia_automatyczna/", obecna_data), showWarnings = FALSE)
        file.copy("Baza_danych/Baza/baza_rolnikow.fst", stri_c("Baza_danych/Kopia_automatyczna/", obecna_data))
      }
    }
    unlink(stri_c("Baza_danych/Kopia_automatyczna/", kopie[kopie < obecna_data - months(6, abbreviate = FALSE)]))
})

observe(stworz_kopie_automatyczne())

 odejmuj_dni_zawieszenia <- reactive({
   invalidateLater(1000*60*60*23*1)
   if (!file.exists("Baza_danych/data_nie_usuwac.txt")) {
     data <- data.frame(data = Sys.Date())
     write_tsv(data, "Baza_danych/data_nie_usuwac.txt")
   }
   data_z_pliku <- read_tsv("Baza_danych/data_nie_usuwac.txt")
   data_z_pliku <- data_z_pliku$data
   data_dzisiejsza <- Sys.Date()
   if (data_z_pliku < data_dzisiejsza) {
     roznica <- as.integer(data_dzisiejsza - data_z_pliku)
     baza_rolnikow_odejmuj <- copy(baza_rolnikow())
     baza_rolnikow_odejmuj[ile_dni_do_użycia > 0L, ile_dni_do_użycia := ile_dni_do_użycia - roznica]
     baza_rolnikow_odejmuj[ile_dni_do_użycia < 0L, ile_dni_do_użycia := 0L]
     write_fst(baza_rolnikow_odejmuj, "Baza_danych/Baza/baza_rolnikow.fst")
    baza_rolnikow(baza_rolnikow_odejmuj)
   }
   data_dzisiejsza <- data.frame(data = data_dzisiejsza)
   write_tsv(data_dzisiejsza, "Baza_danych/data_nie_usuwac.txt")
 })

observe(odejmuj_dni_zawieszenia())

data_kopii_uzytkownika <- reactive({
  invalidateLater(100)
  as.character(file.mtime("Baza_danych/Kopia_uzytkownika/baza_rolnikow.fst"))
})

observe(data_kopii_uzytkownika())

ui <- fluidPage(theme = shinytheme(theme = "cosmo"),
                disconnectMessage(
                  text = "Wystąpił błąd. Spróbuj ponownie lub skontaktuj się z osobą odpowiedzialną za aplikację.",
                  refresh = "Odśwież",
                  background = "#C2BC66",
                  colour = "#F8F8F8",
                  refreshColour = "#F74A7E",
                  overlayColour = "#70B2BD",
                  overlayOpacity = 1,
                  width = 450,
                  top = "center",
                  size = 24,
                  css = "font-family: Lucida;"
                ),
                useShinyalert(),
                titlePanel(
                    fluidRow(
                        column(10, h1("Baza rolników", id = "title"))
                    ), windowTitle = "Baza rolników"
                ),
                tabsetPanel(
                    tabPanel("Odczyt",
                             h2("Wyszukiwanie zmiennych"),
                             fluidRow(
                                 column(6, htmlOutput(outputId = "lista_zmiennych")),
                                 column(4, downloadButton(outputId = "opis_zmiennych_pobierz", label = "Pobierz opis zmiennych", style = "background-color:#68936C"))
                             ),
                             br(),
                             h2("Filtrowanie i odczyt bazy"),
                             fluidRow(
                                 column(6, textAreaInput(inputId = "filtrowanie_bazy", label = "Warunek logiczny na wartości zmiennych bez braków danych", width = "100%", placeholder = "& - i,\n| - lub,\n== - jest równe,\n!= - nie jest równe,\n>, >=, <, <= - relacje nierówności", rows = 5)),
                                 column(6, htmlOutput("filtrowanie_bazy_braki_danych"))
                             ),
                             fluidRow(
                                 column(3, actionButton(inputId = "sprawdzenie_filtrowanie_bazy", label = "Sprawdź poprawność warunku", style = "background-color:#478398")),
                                 column(9, htmlOutput(outputId = "sprawdzenie_filtrowanie_bazy_rezultat"))
                             ),
                             br(),
                             fluidRow(
                                 column(12, checkboxGroupInput(inputId = "wojewodztwa_filtr_do_bazy", label = "Województwa", 
                                                               choices = c("brak", "dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", "małopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie", "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"), 
                                                               selected = c("brak", "dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", "łódzkie", "małopolskie", "mazowieckie", "opolskie", "podkarpackie", "podlaskie", "pomorskie", "śląskie", "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"), 
                                                               inline = TRUE, width = "100%"))
                             ),
                             fluidRow(
                                 column(12, checkboxGroupInput(inputId = "statusy_filtr_do_bazy", label = "Statusy", 
                                                               choices = c("Brak odpowiedniej osoby", "Kwota wyczerpana", "Mieszkanie prywatne/firma", "Nie ma takiego numeru", "Nieefektywny", "Nikt nie odbiera", "Odmowa udziału w jakichkolwiek badaniach", "Odmowa-respondent", "Odmowa-sekretariat", "Poczta głosowa/automatyczna sekretarka/fax", "Powtórzony numer", "Przełożony", "Przerwany", "Wymogi", "Wywiad", "Zajęte"), 
                                                               width = "100%", inline = TRUE, selected = c("Brak odpowiedniej osoby", "Kwota wyczerpana", "Odmowa-respondent", "Odmowa-sekretariat", "Przełożony", "Przerwany", "Wymogi", "Wywiad")))
                             ),
                             fluidRow(
                               column(4, textInput(inputId = "odsetek_statusow", label = "Jaki co najmniej odsetek mają stanowić te statusy?", value = 0.5, placeholder = "Używaj kropki. Nie mniej niż 0, nie więcej niż 1.")),
                               column(2, htmlOutput(outputId = "odsetek_statusow_sprawdzenie")),
                             ),
                             fluidRow(
                               column(4, htmlOutput(outputId = "data_sezonu_badania_przyszlego_")),
                               column(4, offset = 2, selectInput(inputId = "do_kiedy_poprzedni_obecny_sezon", label = "Do którego sezonu będą odnosić się pytania?", choices = c("Obecnego", "Poprzedniego"), selected = "Poprzedniego", width = "100%"))
                             ),
                             fluidRow(
                                 column(4, textInput(inputId = "data_do_filtru_odczyt_bazy", label = "Od kiedy przeszukiwać kontakty?", placeholder = "rrrr-mm-dd", width = "100%")),
                                 column(2, htmlOutput(outputId = "data_do_filtru_odczyt_bazy_sprawdzenie_daty")),
                                 column(4, htmlOutput(outputId = "sledzenie_kontaktu_data")),
                                 column(2, htmlOutput(outputId = "sledzenie_kontaktu_data_sprawdzenie_daty"))
                             ),
                             br(),
                             fluidRow(
                                 column(2, actionButton(inputId = "zastosuj_filtr_odczyt_bazy", label = "Zastosuj filtr", style = "background-color:#CFA07A")),
                                 column(3, htmlOutput(outputId = "wynik_filtru_odczyt_bazy")),
                                 column(3, offset = 2, htmlOutput("wynik_filtru_odczyt_bazy_braki_danych"))
                             ),
                             br(),
                             fluidRow(
                                 column(5, htmlOutput(outputId = "liczebnosc_podzial_na_wojewodztwa")),
                                 column(5, offset = 2, htmlOutput(outputId = "liczebnosc_podzial_na_wojewodztwa_braki_danych"))
                             ),
                             br(),
                             fluidRow(
                                 column(2, checkboxInput(inputId = "uwzglednij_braki_danych", label = "Uwzględnij braki danych")),
                                 column(2, textInput(inputId = "losowanie_kontaktow",label = NULL, placeholder = "Wylosuj n-rekordów")),
                                 column(2, downloadButton(outputId = "pobierz_baze", label = "Pobierz bazę", style = "background-color:#AF835D"))
                             ),
                             br(),
                             br(),
                             fluidRow(
                                 column(12, dataTableOutput(outputId = "podglad_bazy"))
                             ),
                             br()
                    ),
                    tabPanel("Modyfikacja kontaktów",
                             h2("Import danych do bazy"),
                             fluidRow(
                                 column(4, fileInput(inputId = "import_bazy", label = "Baza do importu", buttonLabel = "Plik .xlsx", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", placeholder = "Nie wybrano")),
                                 column(2, textInput(inputId = "data_badania", label = "Data badania", placeholder = "rrrr-mm-dd")),
                                 column(2, htmlOutput(outputId = "data_badania_sprawdzenie_daty"))
                             ),
                             fluidRow(
                                 column(6, actionButton(inputId = "importuj_przycisk", label = "Importuj", width = "100%", style = "background-color:#478398"))
                             ),
                             br(),
                             h2("Zawieszanie kontaktów"),
                             fluidRow(
                                 column(6, checkboxGroupInput(inputId = "statusy_do_zawieszenia", label = "Statusy", 
                                                              choices = c("Brak odpowiedniej osoby", "Kwota wyczerpana", "Mieszkanie prywatne/firma", "Nie ma takiego numeru", "Nieefektywny", "Nikt nie odbiera", "Odmowa udziału w jakichkolwiek badaniach", "Odmowa-respondent", "Odmowa-sekretariat", "Poczta głosowa/automatyczna sekretarka/fax", "Powtórzony numer", "Przełożony", "Przerwany", "Wymogi", "Wywiad", "Zajęte"), 
                                                              inline = TRUE, width = "100%")),
                                 column(3, offset = 2, textAreaInput(inputId = "zawieszenie_kontaktu", label = "Kontakty do zawieszenia", placeholder = "Telefony (tylko jeden telefon dla kontaktu!) oddzielane nową linią (enterem) \ni bez przecinka na końcu", rows = 5)),
                             ),
                             fluidRow(
                                 column(3, numericInput(inputId = "statusy_do_zawieszenia_liczba_statusow", label = "Ile statusów przeszukiwać?", value = 20, min = 1, max = 20)),
                                 column(2, numericInput(inputId = "statusy_do_zawieszenia_liczba_dni", label = "Na ile dni zawiesić?", value = 1, min = 1)),
                                 column(3, offset = 3, numericInput(inputId = "kontakty_do_zawieszenia_liczba_dni", label = "Na ile dni zawiesić?", value = 1, min = 1))
                             ),
                             fluidRow(
                                 column(5, actionButton(inputId = "statusy_do_zawieszenia_przycisk", label = "Zawieś kontakty", style = "background-color:#478398", width = "100%")),
                                 column(3, offset = 3, actionButton(inputId = "kontakty_do_zawieszenia", label = "Zawieś kontakty", style = "background-color:#478398", width = "100%"))
                             ),
                             br(),
                             h4("Pobieranie kontaktów zawieszonych i ich przywracanie"),
                             fluidRow(
                                 column(3, downloadButton(outputId = "pobierz_kontakty_zawieszone", label = "Pobierz kontakty zawieszone",  style = "background-color:#68936C"))
                             ),
                             br(),
                             fluidRow(
                                 column(4, fileInput(inputId = "przywracanie_kontaktow_zawieszonych", label = "Kontakty zawieszone do ręcznego przywrócenia", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", buttonLabel = "Plik .xlsx", placeholder = "Nie wybrano"))
                             ),
                             fluidRow(
                                 column(2, actionButton(inputId = "przywracanie_kontaktow_zawieszonych_przycisk", label = "Przywróć kontakty", style = "background-color:#478398"))
                             ),
                             br(),
                             h2("Usuwanie kontaktów i telefonów"),
                             fluidRow(
                                 column(8, checkboxGroupInput(inputId = "statusy_do_usuniecia", label = "Statusy", 
                                                              choices = c("Brak odpowiedniej osoby", "Kwota wyczerpana", "Mieszkanie prywatne/firma", "Nie ma takiego numeru", "Nieefektywny", "Nikt nie odbiera", "Odmowa udziału w jakichkolwiek badaniach", "Odmowa-respondent", "Odmowa-sekretariat", "Poczta głosowa/automatyczna sekretarka/fax", "Powtórzony numer", "Przełożony", "Przerwany", "Wymogi", "Wywiad", "Zajęte"), 
                                                              inline = TRUE, width = "100%"))
                             ),
                             fluidRow(
                                 column(3, numericInput(inputId = "statusy_do_usuniecia_liczba_statusow", label = "Ile statusów przeszukiwać?", value = 20, min = 1, max = 20))
                             ),
                             fluidRow(
                                 column(2, actionButton(inputId = "statusy_do_usuniecia_przycisk", label = "Usuń kontakty", style = "background-color:#478398"))
                             ),
                             br(),
                             fluidRow(
                                 column(4, textAreaInput(inputId = "usuwanie_telefonow", label = "Telefony do usunięcia", placeholder = "Telefony oddzielane nową linią (enterem) \ni bez przecinka na końcu", rows = 5)),
                                 column(4, textAreaInput(inputId = "usuwanie_kontaktow", label = "Kontakty do usunięcia", placeholder = "Telefony (tylko jeden telefon dla kontaktu!) oddzielane nową linią (enterem) \ni bez przecinka na końcu", rows = 5))
                             ),
                             fluidRow(
                                 column(2, actionButton(inputId = "usuwanie_telefonow_przycisk", label = "Usuń telefony", style = "background-color:#478398")),
                                 column(2, offset = 2, actionButton(inputId = "usuwanie_kontaktow_przycisk", label = "Usuń kontakty", style = "background-color:#478398"))
                             ),
                             br()
                    ),
                    tabPanel("Modyfikacja zmiennych",
                             h2("Dodawanie/zmienianie opisu zmiennych"),
                             fluidRow(
                                 column(12, textAreaInput(inputId = "dodawanie_zmienianie_opisu_zmiennych", label = "Zmienne wraz z opisem", placeholder = "nazwa_zmiennej = opis \nkolejne pary oddzielane nową linią (enterem) i bez przecinka na końcu", width = "100%", rows = 3))
                             ),
                             fluidRow(
                                 column(4, actionButton(inputId = "dodawanie_zmienianie_opisu_zmiennych_przycisk", label = "Dodaj/zmień opis zmiennych", style = "background-color:#478398"))
                             ),
                             br(),
                             h2("Zmienianie nazw zmiennych"),
                             fluidRow(
                                 column(12, textAreaInput(inputId = "zmienianie_nazw_zmiennych", label = "Stare i nowe nazwy zmiennych", placeholder = "stara_nazwa_zmiennej = nowa_nazwa_zmiennej \nkolejne pary oddzielane nową linią (enterem) i bez przecinka na końcu", width = "100%", rows = 3))
                             ),
                             fluidRow(
                                 column(4, actionButton(inputId = "zmienianie_nazw_zmiennych_przycisk", label = "Zmień nazwy zmiennych", style = "background-color:#478398"))
                             ),
                             br(),
                             h2("Rekodowanie odpowiedzi"),
                             fluidRow(
                                 column(12, textAreaInput(inputId = "rekodowanie_odpowiedzi", placeholder = "nazwa_zmiennej_zmienianej, wartość_zmieniana = nowa_nazwa_tej_samej_zmiennej_dla_zmienianych_wartości, nowa_wartość \nkolejne pary oddzielane nową linią (enterem) i bez przecinka na końcu", rows = 3, label = NULL))
                             ),
                             fluidRow(
                                 column(1, actionButton(inputId = "rekodowanie_odpowiedzi_przycisk", label = " Rekoduj odpowiedzi", style = "background-color:#478398"))
                             ),
                             br(),
                             h2("Dodawanie i usuwanie zmiennych bez historii"),
                             fluidRow(
                               column(6, htmlOutput(outputId = "zmienne_bez_historii_lista")),
                               column(6, htmlOutput(outputId = "zmienne_bez_historii_dodaj_usun"))
                             ),
                             fluidRow(
                               column(2, offset = 6, actionButton(inputId = "zmienne_bez_historii_dodaj_przycisk", label = "Dodaj", style = "background-color:#478398", width = "100%")),
                               column(2, actionButton(inputId = "zmienne_bez_historii_usun_przycisk", label = "Usuń", style = "background-color:#478398", width = "100%"))
                             ),
                             br(),
                             h2("Usuwanie danych na podstawie zmiennej i daty"),
                             fluidRow(
                                 column(3, downloadButton(outputId = "pobierz_zmienne_daty", label = "Pobierz zmienne i daty", style = "background-color:#68936C"))
                             ),
                             br(),
                             fluidRow(
                                 column(3, fileInput(inputId = "usuwanie_zmiennych_dat", label = "Zmienne wraz z datami do usunięcia", accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", buttonLabel = "Plik .xlsx", placeholder = "Nie wybrano"))
                             ),
                             fluidRow(
                                 column(3, actionButton(inputId = "usuwanie_zmiennych_dat_przycisk", label = "Usuń dane", style = "background-color:#478398"))
                             ),
                             br()
                    ),
                    tabPanel("Kopia",
                             h2("Zapisywanie kopii"),
                             fluidRow(
                                 column(2, actionButton(inputId = "zapisuj_kopie_przycisk", label = "Zapisz kopię użytkownika", style = "background-color:#478398"))
                             ),
                             br(),
                             h2("Przywracanie kopii"),
                             fluidRow(
                                 column(1, offset = 1, textOutput(outputId = "data_kopii_uzytkownika")),
                                 column(3, actionButton(inputId = "przywracaj_kopie_uzytkownika_przycisk", label = "Przywróć kopię użytkownika", style = "background-color:#478398", width = "100%"))
                             ),
                             br(),
                             fluidRow(
                                 column(2, htmlOutput(outputId = "daty_kopii_automatycznych")),
                                 column(3, actionButton(inputId = "przywracaj_kopie_automatyczne_przycisk", label = "Przywróć kopię automatyczną", style = "background-color:#478398", width = "100%"))
                             )
                    ),
                    tabPanel("Pomoc",
                             h2("Instrukcja"),
                             fluidRow(
                                 column(4, downloadButton(outputId = "instrukcja_pobierz", label = "Pobierz instrukcję", style = "background-color:#68936C"))
                             )
                    )
                ),
                tags$style("
             .btn-file {  
             background-color:#BF6D93;
             }
             .btn-file:hover {
                background-color: #BF6D93;
             }
             .progress-bar {
             background-color: #81ADC1;
             }
             * {
             font-family: Lucida Bright;
             font-size: 15px;
             }
             "),
                tags$head(tags$style(
                    HTML('#title {
            font-family: helvetica;
           color: #8585A4;
           font-size: 40px;
           font-style: bold;
          }
                          
                          
                  .tabbable > .nav > li > a {
                                 color:#A1A360;
                                 }
               .shiny-notification {
              height: 50px;
              width: 400px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 200px);;
            }'
                    ))),
                tags$style(HTML("                  
  .shiny-input-container:not(.shiny-input-container-inline) {
  width: 100%;
}"))
)

server <- function(input, output, session) {
  
    output$lista_zmiennych <- renderUI({
        zmienne <- lista_zmiennych()
        selectInput(inputId = "lista_zmiennych", label = NULL, choices = zmienne, selectize = TRUE)
    })
    
    output$opis_zmiennych_pobierz <- downloadHandler(
        filename = function() {
            "Opis_zmiennych.xlsx"
        },
        content = function(file) {
            withProgress(message = "Opisywanie zmiennych", {
                baza_rolnikow <- baza_rolnikow()
                zmienne <- lista_zmiennych()
                opis_zmiennych <- data.table(zmienna = zmienne,
                                         etykieta = map_chr(zmienne, ~ first(baza_rolnikow$opis[baza_rolnikow$zmienna == .])),
                                         wartości = map_chr(zmienne, wartosci_zmiennej, baza = baza_rolnikow))
                write_xlsx(opis_zmiennych, file)
            })
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    output$filtrowanie_bazy_braki_danych <- renderUI({
        textAreaInput(inputId = "filtrowanie_bazy_braki_danych_", label = "Warunek logiczny na wartości zmiennych wraz z brakami danych", value = input$filtrowanie_bazy, rows = 5, placeholder = "! - zaprzeczenie (nieprawda, że),\nis.na(nazwa_zmiennej) - zmienna ma braki danych,\n%in% - wartości zmiennej znajdują się w zbiorze c(\"wartość\", \"wartość\"), np. na_kogo_zagłosowano_w_wyborach %in% c(\"Kandydat A\", \"Kandydat B\"),\n(, ) - zamykanie warunków w nawiasy")
    })
    
    sprawdz_filtr <- eventReactive(input$sprawdzenie_filtrowanie_bazy, {
        filtrowanie_bazy <- isolate(input$filtrowanie_bazy)
        filtrowanie_bazy_braki_danych_ <- isolate(input$filtrowanie_bazy_braki_danych_)
        if (filtrowanie_bazy != "" || filtrowanie_bazy_braki_danych_ != "") {
          baza_rolnikow <- isolate(baza_rolnikow())
          zmienne <- isolate(lista_zmiennych())
          baza_najnowsze_kolumny <- baza_rolnikow[, .SD[1L], zmienna]
          baza_najnowsze_kolumny <- baza_najnowsze_kolumny[, .SD, .SDcols = c("zmienna", "wartość", "telefon_0")] [!zmienna %chin% c("status", "województwo")]
          baza_najnowsze_kolumny <- spread(baza_najnowsze_kolumny, key = zmienna, value = wartość)
          baza_najnowsze_kolumny <- select(baza_najnowsze_kolumny, -telefon_0)
          tryCatch({
          if (filtrowanie_bazy != "" && filtrowanie_bazy_braki_danych_ != "") {
            filtrowanie_bazy <- filter(baza_najnowsze_kolumny, eval(parse(text = filtrowanie_bazy)))
            filtrowanie_bazy_braki_danych <- filter(baza_najnowsze_kolumny, eval(parse(text = filtrowanie_bazy_braki_danych_)))
            list(filtrowanie_bazy = filtrowanie_bazy, filtrowanie_bazy_braki_danych = filtrowanie_bazy_braki_danych)
          } else if (filtrowanie_bazy_braki_danych_ != "" && filtrowanie_bazy == "") {
            filtrowanie_bazy_braki_danych <- filter(baza_najnowsze_kolumny, eval(parse(text = filtrowanie_bazy_braki_danych_)))
            list(filtrowanie = filtrowanie_bazy_braki_danych)
          } else if (filtrowanie_bazy_braki_danych_ == "" && filtrowanie_bazy != "") {
            filtrowanie_bazy <- filter(baza_najnowsze_kolumny, eval(parse(text = filtrowanie_bazy)))
            list(filtrowanie = filtrowanie_bazy)
          }
          }, error = function(e) "Błąd! Nazwa zmiennej jest niepoprawna lub zmienne nie są poprawnie łączone spójnikami.")
        }
    })
    
    output$sprawdzenie_filtrowanie_bazy_rezultat <- renderUI({
        test <- sprawdz_filtr()
        if (typeof(test) == "character") {
          p(test, style = "color:red")
        } else if (typeof(test) == "list") {
          p("OK", style = "color:#438F3B")
        }
    })
    
    output$odsetek_statusow_sprawdzenie <- renderUI({
      odsetek <- round(as.numeric(input$odsetek_statusow), 3)
      if (is.na(odsetek) || odsetek > 1 || odsetek < 0) {
        p("Błędna liczba!", style = "color:red")
      }
    })
    
    output$sledzenie_kontaktu_data <- renderUI({
        data <- ymd(input$data_do_filtru_odczyt_bazy)
        if (input$data_do_filtru_odczyt_bazy == "" || is.na(data)) {
            etykieta <- "Do kiedy śledzić historię?"
            data <- ""
        } else {
            etykieta <- "Do kiedy śledzić historię? Nie dalej niż "
        }
        textInput(inputId = "sledzenie_kontaktu_data_", label = stri_c(etykieta, data), placeholder = "rrrr-mm-dd", value = Sys.Date())
    })
    
    output$data_do_filtru_odczyt_bazy_sprawdzenie_daty <- renderUI({
        data_1 <- input$data_do_filtru_odczyt_bazy
        data <- ymd(input$data_do_filtru_odczyt_bazy)
        if (data_1 == "") {
            ""
        } else if (is.na(data)) {
            p("Błąd w dacie!", style = "color:red")
        }
    })
    
    output$sledzenie_kontaktu_data_sprawdzenie_daty <- renderUI({
        data_1 <- input$sledzenie_kontaktu_data_
        data <- ymd(input$sledzenie_kontaktu_data_)
        if (!is.null(data_1)) {
          if (data_1 == "") {
            ""
          } else if (is.na(data)) {
            p("Błąd w dacie!", style = "color:red")
          }
        }
    })
    
    output$data_sezonu_badania_przyszlego_ <- renderUI({
        obecny_sezon <- zaokraglij_daty_do_sezonow(Sys.Date())
        przyszly_sezon <- obecny_sezon + years(1)
        selectInput(inputId = "data_sezonu_badania_przyszlego", label = "Data końca sezonu, w którym odbędzie się badanie", choices = c(obecny_sezon, przyszly_sezon), selected = obecny_sezon, width = "100%")
    })
    
    generuj_baze_wyfiltrowana <- eventReactive(input$zastosuj_filtr_odczyt_bazy, {
      withProgress(message = "Przeszukiwanie zmiennych", {
        baza_rolnikow <- baza_rolnikow()
        baza_rolnikow <- baza_rolnikow[ile_dni_do_użycia == 0L]
        do_kiedy_przeszukiwac_tekst <- input$do_kiedy_poprzedni_obecny_sezon
        data_sezonu_badania_przyszlego <- ymd(input$data_sezonu_badania_przyszlego)
        obecny_sezon <- zaokraglij_daty_do_sezonow(Sys.Date())
        if (data_sezonu_badania_przyszlego > obecny_sezon && do_kiedy_przeszukiwac_tekst == "Poprzedniego") {
          do_kiedy_przeszukiwac <- obecny_sezon
          data_sezonu_badania_przyszlego <- obecny_sezon
        } else if (data_sezonu_badania_przyszlego == obecny_sezon && do_kiedy_przeszukiwac_tekst == "Poprzedniego") {
          do_kiedy_przeszukiwac <- obecny_sezon - years(1)
          data_sezonu_badania_przyszlego <- obecny_sezon - years(1)
        } else {
          do_kiedy_przeszukiwac <- obecny_sezon
        }
        data_zmienne <- ymd(input$data_do_filtru_odczyt_bazy)
        data_zmienne_sledzenie <- ymd(input$sledzenie_kontaktu_data_)
        if (!is.na(data_zmienne_sledzenie) && !is.na(data_zmienne)) {
          if (data_zmienne_sledzenie < data_zmienne) {
            data_zmienne_sledzenie <- data_zmienne
          }
        }

        if (is.na(data_zmienne)) {
          data_zmienne <- ymd("1970-01-01")
        }

        if (is.na(data_zmienne_sledzenie)) {
          data_zmienne_sledzenie <- Sys.Date()
        }
        data_zmienne <- zaokraglij_daty_do_sezonow(data_zmienne)
        data_zmienne_sledzenie <- zaokraglij_daty_do_sezonow(data_zmienne_sledzenie)
        filtr <- input$filtrowanie_bazy
        filtrowanie_bazy_braki_danych_ <- input$filtrowanie_bazy_braki_danych_
        zmienne <- lista_zmiennych()
        zmienne_w_filtrze <- zmienne[stri_detect_regex(filtr, stri_c("\\b", zmienne, "\\b"))]
        zmienne_bez_historii <- unique(pull(baza_rolnikow[zmienna_bez_historii == TRUE], zmienna))
        statusy_filtr_do_bazy <- input$statusy_filtr_do_bazy
        wojewodztwa_filtr_do_bazy <- input$wojewodztwa_filtr_do_bazy
        odsetek_statusow <- round(as.numeric(input$odsetek_statusow), 3)
        if (is.na(odsetek_statusow) || odsetek_statusow > 1 || odsetek_statusow < 0) {
          odsetek_statusow <- 0.5
        }
        test_filtr_braki_danych <- filtrowanie_bazy_braki_danych_ != "" && all(zmienne[stri_detect_regex(filtr, stri_c("\\b", zmienne, "\\b"))] %in% zmienne[stri_detect_regex(filtrowanie_bazy_braki_danych_, stri_c("\\b", zmienne, "\\b"))]) && all(zmienne[stri_detect_regex(filtrowanie_bazy_braki_danych_, stri_c("\\b", zmienne, "\\b"))] %in% zmienne[stri_detect_regex(filtr, stri_c("\\b", zmienne, "\\b"))]) && stri_detect_regex(filtrowanie_bazy_braki_danych_, "is\\.na\\(")
        if (baza_rolnikow[, .N] == 0 && !test_filtr_braki_danych) {
          return(list(baza_gotowa_bez_brakow_danych = baza_rolnikow))
        } else if (baza_rolnikow[, .N] == 0 && test_filtr_braki_danych) {
          return(list(baza_gotowa_bez_brakow_danych = baza_rolnikow, baza_gotowa_lacznie_z_brakami_danych = baza_rolnikow))
        }
        if (filtr != "" && !stri_detect_regex(filtr, "is\\.na\\(") && length(zmienne_w_filtrze %in% zmienne) == length(zmienne_w_filtrze) && length(zmienne_w_filtrze) > 0) {
          baza_rolnikow_wojewodztwa <- select(baza_rolnikow, matches("^telefon_\\d+$"), nazwa, email, zmienna, data, zmienna_data, wartość)
          baza_rolnikow_wojewodztwa <- baza_rolnikow_wojewodztwa[zmienna == "województwo"][wartość %chin% wojewodztwa_filtr_do_bazy][, .(telefon_0, wartość)]
          baza_rolnikow <- baza_rolnikow[zmienna != "województwo"]
          incProgress(0.1)
          baza_same_statusy <- select(baza_rolnikow, matches("^telefon_\\d+$"), nazwa, email, zmienna, data, zmienna_data, wartość)
          baza_same_statusy <- baza_same_statusy[zmienna == "status"]
          setkey(baza_same_statusy, telefon_0)
          baza_same_statusy <- baza_same_statusy[telefon_0 %in% baza_rolnikow_wojewodztwa$telefon_0]
          baza_same_statusy <- baza_same_statusy[, wartość_statusy := mean(wartość %chin% statusy_filtr_do_bazy), by = telefon_0] [wartość_statusy >= odsetek_statusow]
          baza_same_statusy[, c("zmienna_data", "wartość_statusy", "zmienna") := NULL]
          baza_rolnikow_wojewodztwa <- baza_rolnikow_wojewodztwa[telefon_0 %in% baza_same_statusy$telefon_0]
          
          baza_rolnikow <- baza_rolnikow[zmienna != "status"]
          baza_rolnikow <- select(baza_rolnikow, matches("^telefon_\\d+$"), nazwa, email, zmienna, data = sezon, zmienna_data = zmienna_sezon, wartość, zmienna_bez_historii)
          baza_rolnikow <- baza_rolnikow[telefon_0 %in% baza_same_statusy$telefon_0]
          telefony_wszystkie <- baza_rolnikow[, .(telefon_0 = unique(telefon_0))]
          baza_rolnikow <- baza_rolnikow[zmienna %chin% zmienne_w_filtrze & data >= data_zmienne]
          
          if (baza_rolnikow[, .N] == 0 && test_filtr_braki_danych) {
            zmienne_kolumny <- c(zmienne_w_filtrze, stri_c("data_", zmienne_w_filtrze))
            baza_rolnikow <- baza_rolnikow_wojewodztwa
            baza_rolnikow[, (zmienne_kolumny) := NA]
            names(baza_rolnikow)[names(baza_rolnikow) == "wartość"] <- "województwo"
            baza_rolnikow[, wyniki_najnowsze := "Tak"]
            return(list(baza_gotowa_bez_brakow_danych = baza_rolnikow[FALSE], baza_gotowa_lacznie_z_brakami_danych = baza_rolnikow, baza_same_statusy = baza_same_statusy))
          } else if (baza_rolnikow[, .N] == 0 && !test_filtr_braki_danych) {
            return(list(baza_gotowa_bez_brakow_danych = baza_rolnikow))
          }
          
          baza_rolnikow[zmienna_bez_historii == TRUE, `:=`(data = do_kiedy_przeszukiwac,
                                                           zmienna_data = stri_c(zmienna, "_", stri_replace_all_fixed(do_kiedy_przeszukiwac, "-", "_")))][, zmienna_bez_historii := NULL]
          
          zmienne_do_kombinacji <- pull(baza_rolnikow[, .(zmienne = stri_c(unique(zmienna_data), collapse = ", ")), by = zmienna], zmienne)
          
          zmienne_do_brakow <- pull(baza_rolnikow[, .(zmienna = unique(zmienna))], zmienna)
          baza_kombinacje <- CJ(telefon_0 = telefony_wszystkie$telefon_0, zmienna_data = baza_rolnikow$zmienna_data, unique = TRUE)
          baza_rolnikow <- baza_rolnikow[order(zmienna_data)]
          baza_kombinacje <- baza_kombinacje[order(zmienna_data)]
          
          setkey(baza_rolnikow, telefon_0, zmienna_data)
          setkey(baza_kombinacje, telefon_0, zmienna_data)
          
          baza_rolnikow <- map_dfr(zmienne_do_brakow, wypelnij_braki_danych, baza_rolnikow = baza_rolnikow, baza_kombinacje = baza_kombinacje)
          incProgress(0.3)
          baza_rolnikow[, zmienna := NULL]
          
          baza_rolnikow <- dcast.data.table(baza_rolnikow, telefon_0 ~ zmienna_data, value.var = c("wartość", "data"))
          
          nazwy_obecny_sezon <- names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), stri_c("_", stri_replace_all_fixed(obecny_sezon, "-", "_"), "$"))]
          nazwy_obecny_sezon <- nazwy_obecny_sezon[!is.na(nazwy_obecny_sezon)]
          if (do_kiedy_przeszukiwac < obecny_sezon && length(nazwy_obecny_sezon) > 0) {
            nazwy_poprzedni_sezon <- names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), stri_c("_", stri_replace_all_fixed(obecny_sezon - years(1), "-", "_"), "$"))]
            if (length(nazwy_poprzedni_sezon) > 0) {
              baza_rolnikow <- baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[!names(baza_rolnikow) %in% nazwy_obecny_sezon]]
            }
          }
          
          baza_rolnikow <- as.data.table(map_if(baza_rolnikow, sprawdzenie_numeric, as.numeric))
          
          kolumny_ze_zmiennymi <- names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^wartość_")]
          kolumny_ze_zmiennymi <- stri_replace_all_regex(kolumny_ze_zmiennymi, "^wartość_", "")
          kolumny_ze_zmiennymi <- stri_replace_all_regex(kolumny_ze_zmiennymi, "_\\d{4}_\\d{2}_\\d{2}$", "")
          zmienne_w_filtrze_ktorych_nie_ma_w_bazie <- zmienne_w_filtrze[!zmienne_w_filtrze %in% unique(kolumny_ze_zmiennymi)]
          if (length(zmienne_w_filtrze_ktorych_nie_ma_w_bazie) > 0) {
            zmienne_do_kombinacji <- c(zmienne_do_kombinacji, zmienne_w_filtrze_ktorych_nie_ma_w_bazie)
          }
          incProgress(0.1)
          zmienne_do_kombinacji <- map(zmienne_do_kombinacji, ~ unlist(stri_split_fixed(., ", "), use.names = FALSE))
          kombinacje <- do.call(CJ, zmienne_do_kombinacji)
          kombinacje <- kombinacje[dim(kombinacje)[1]:1]
          kolumny_lista <- slide(kombinacje, ~ .)
          incProgress(0.1)
          
          if (length(zmienne_w_filtrze_ktorych_nie_ma_w_bazie) > 0) {
            zmienne_w_filtrze_ktorych_nie_ma_w_bazie_nazwy_kolumn_z_datami <- stri_c("data_", zmienne_w_filtrze_ktorych_nie_ma_w_bazie)
            zmienne_w_filtrze_ktorych_nie_ma_w_bazie_nazwy_kolumn <- c(zmienne_w_filtrze_ktorych_nie_ma_w_bazie, zmienne_w_filtrze_ktorych_nie_ma_w_bazie_nazwy_kolumn_z_datami)
            baza_rolnikow[, (zmienne_w_filtrze_ktorych_nie_ma_w_bazie_nazwy_kolumn) := NA]
          }
          names(baza_rolnikow)[grep("^wartość_", names(baza_rolnikow))] <- stri_replace_all_regex(names(baza_rolnikow)[grep("^wartość_", names(baza_rolnikow))], "^wartość_", "")
          incProgress(0.1)
          if (test_filtr_braki_danych) {
            filtr <- filtrowanie_bazy_braki_danych_
          }
          baza_rolnikow <-  map_dfr(kolumny_lista, filtruj_czesci_bazy, baza_rolnikow = baza_rolnikow, filtr = filtr)
          incProgress(0.1)
          baza_rolnikow_zaakceptowana <- baza_rolnikow[rezultat == TRUE]
          baza_rolnikow_odrzucona <- baza_rolnikow[rezultat == FALSE]
          setkey(baza_rolnikow_zaakceptowana, telefon_0)
          setkey(baza_rolnikow_odrzucona, telefon_0)
          baza_rolnikow_zaakceptowana <- unique(baza_rolnikow_zaakceptowana, by = "telefon_0")
          baza_rolnikow_odrzucona <- unique(baza_rolnikow_odrzucona, by = "telefon_0")
          baza_gotowa_1 <- baza_rolnikow_zaakceptowana[!telefon_0 %in% baza_rolnikow_odrzucona$telefon_0][, wyniki_najnowsze := "Tak"]
          
          baza_rolnikow_zaakceptowana <- baza_rolnikow_zaakceptowana[telefon_0 %in% baza_rolnikow_odrzucona$telefon_0]
          baza_rolnikow_odrzucona <- baza_rolnikow_odrzucona[telefon_0 %in% baza_rolnikow_zaakceptowana$telefon_0]
          baza_rolnikow_zaakceptowana_dluga <- baza_rolnikow_zaakceptowana[, .SD, .SDcols = names(baza_rolnikow_zaakceptowana)[grep(stri_c(c("^data_", "^telefon_0$"), collapse = "|"), names(baza_rolnikow_zaakceptowana))]]
          baza_rolnikow_zaakceptowana_dluga <- melt.data.table(baza_rolnikow_zaakceptowana_dluga, id.vars = "telefon_0", variable.name = "zmienna", value.name = "data_zaakceptowania", variable.factor = FALSE)
          baza_rolnikow_odrzucona_dluga <- baza_rolnikow_odrzucona[, .SD, .SDcols = names(baza_rolnikow_odrzucona)[grep(stri_c(c("^data_", "^telefon_0$"), collapse = "|"), names(baza_rolnikow_odrzucona))]]
          baza_rolnikow_odrzucona_dluga <- melt.data.table(baza_rolnikow_odrzucona_dluga, id.vars = "telefon_0", variable.name = "zmienna", value.name = "data_odrzucenia", variable.factor = FALSE)
          baza_rolnikow_zaakceptowana_dluga[, data_odrzucenia := baza_rolnikow_odrzucona_dluga$data_odrzucenia]
          incProgress(0.1)
          baza_rolnikow_zaakceptowana_dluga <- baza_rolnikow_zaakceptowana_dluga[data_odrzucenia < data_sezonu_badania_przyszlego][, `:=`(wynik_data = fifelse(data_zaakceptowania < data_odrzucenia & data_zaakceptowania < data_zmienne_sledzenie, "Nie", "Tak"),
                                                                                                                                          wyniki_najnowsze_temp = fifelse(data_zaakceptowania < data_odrzucenia, "Nie", "Tak"))] [, `:=`(wynik = mean(wynik_data == "Tak"),
                                                                                                                                                                                                                                         wyniki_najnowsze = mean(wyniki_najnowsze_temp == "Tak")), by = telefon_0][wynik == 1L][, wyniki_najnowsze_rezultat := fifelse(wyniki_najnowsze == 1L, "Tak", "Nie"), by = telefon_0][, .(telefon_0, wyniki_najnowsze_rezultat)]
          
          baza_rolnikow_zaakceptowana_dluga <- unique(baza_rolnikow_zaakceptowana_dluga, by = "telefon_0")
          
          baza_gotowa_2 <- baza_rolnikow_zaakceptowana[telefon_0 %in% baza_rolnikow_zaakceptowana_dluga$telefon_0][, wyniki_najnowsze := baza_rolnikow_zaakceptowana_dluga$wyniki_najnowsze_rezultat]
          
          baza_gotowa <- rbindlist(list(baza_gotowa_1, baza_gotowa_2), use.names = FALSE)
          setkey(baza_gotowa, telefon_0)
          setkey(baza_rolnikow_wojewodztwa, telefon_0)
          
          baza_rolnikow_wojewodztwa <- baza_rolnikow_wojewodztwa[telefon_0 %in% baza_gotowa$telefon_0][, telefon_0 := NULL]
          baza_gotowa[, województwo := baza_rolnikow_wojewodztwa$wartość]
          incProgress(0.1)
          baza_gotowa[, rezultat := NULL]
          zmienne_w_filtrze_bez_historii <- zmienne_w_filtrze[zmienne_w_filtrze %in% zmienne_bez_historii]
          if (length(zmienne_w_filtrze_bez_historii) > 0) {
            zmienne_w_filtrze_bez_historii_nazwy_kolumn_z_datami <- stri_c("data_", zmienne_w_filtrze_bez_historii)
            baza_gotowa[, (zmienne_w_filtrze_bez_historii_nazwy_kolumn_z_datami) := NA]
          }

            if (test_filtr_braki_danych) {
              filtr_bez_brakow_danych <- input$filtrowanie_bazy
              baza_gotowa_bez_brakow_danych <- baza_gotowa[eval(parse(text = filtr_bez_brakow_danych))]
              baza_gotowa <- list(baza_gotowa_bez_brakow_danych = baza_gotowa_bez_brakow_danych, baza_gotowa_lacznie_z_brakami_danych = baza_gotowa, baza_same_statusy = baza_same_statusy)
            } else {
              baza_gotowa <- list(baza_gotowa_bez_brakow_danych = baza_gotowa, baza_same_statusy = baza_same_statusy)
            }
        } else {
          baza_rolnikow <- baza_rolnikow[zmienna == "województwo"][, .(telefon_0, zmienna, wartość)]
          baza_rolnikow <- dcast.data.table(baza_rolnikow, telefon_0 ~ zmienna, value.var = "wartość")
          list(baza_gotowa_bez_brakow_danych = baza_rolnikow)
        }
      })
    })
    
    output$wynik_filtru_odczyt_bazy <- renderUI({
        baza <- generuj_baze_wyfiltrowana()
        filtr <- input$filtrowanie_bazy
        zmienne <- lista_zmiennych()
        zmienne_w_filtrze <- zmienne[stri_detect_regex(filtr, stri_c("\\b", zmienne, "\\b"))]
        if (!(filtr != "" && !stri_detect_regex(filtr, "is\\.na\\(") && length(zmienne_w_filtrze %in% zmienne) == length(zmienne_w_filtrze) && length(zmienne_w_filtrze) > 0)) {
          HTML(stri_c("Całkowita wielkość bazy: <br> ", format(baza$baza_gotowa_bez_brakow_danych[, .N], scientific = FALSE, big.mark = " "), sep = ""))
        } else {
          HTML(stri_c("Wielkość bazy bez braków danych: <br> ", format(baza$baza_gotowa_bez_brakow_danych[, .N], scientific = FALSE, big.mark = " "), sep = ""))
        }
    })
    
    output$liczebnosc_podzial_na_wojewodztwa <- renderText({
        baza <- generuj_baze_wyfiltrowana()
        if (baza$baza_gotowa_bez_brakow_danych[, .N] != 0) {
          baza$baza_gotowa_bez_brakow_danych[, .(Liczebność = .N), by = "województwo"][,  Udział := percent(Liczebność / sum(Liczebność), digits = 0)][order(Udział, województwo)][, `:=`(Udział = color_bar("#D3BC4E")(Udział),
                                                                                                                                                                                        Liczebność = format(Liczebność, scientific = FALSE, big.mark = " "))] %>%
            rename(Województwo = województwo) %>%
            kable(escape = FALSE) %>%
            kable_styling(bootstrap_options = c("striped", "condensed"))
        }
    })
    
    output$wynik_filtru_odczyt_bazy_braki_danych <- renderUI({
      baza <- generuj_baze_wyfiltrowana()
      if (length(baza) == 3 || length(baza) == 2 && names(baza)[2] == "baza_gotowa_lacznie_z_brakami_danych") {
        HTML(stri_c("Wielkość bazy wraz z brakami danych: <br> ", format(baza$baza_gotowa_lacznie_z_brakami_danych[, .N], scientific = FALSE, big.mark = " "), sep = ""))
      }
    })
    
    output$liczebnosc_podzial_na_wojewodztwa_braki_danych <- renderText({
        baza <- generuj_baze_wyfiltrowana()
        if (length(baza) == 3) {
          if (baza$baza_gotowa_lacznie_z_brakami_danych[, .N] != 0) {
            baza$baza_gotowa_lacznie_z_brakami_danych[, .(Liczebność = .N), by = "województwo"][,  Udział := percent(Liczebność / sum(Liczebność), digits = 0)][order(Udział, województwo)][, `:=`(Udział = color_bar("#D3BC4E")(Udział),
                                                                                                                                                                                                 Liczebność = format(Liczebność, scientific = FALSE, big.mark = " "))] %>%
              rename(Województwo = województwo) %>%
              kable(escape = FALSE) %>%
              kable_styling(bootstrap_options = c("striped", "condensed"))
          }
        }
    })
    
    output$pobierz_baze <- downloadHandler(
        filename = function() {
            "Wygenerowana_baza.xlsx"
        },
        content = function(file) {
            withProgress(message = "Tworzenie pliku", {
                baza_gotowa <- generuj_baze_wyfiltrowana()
                if (length(baza_gotowa) >= 2) {
                    zmienne <- lista_zmiennych()
                    baza_same_statusy <- baza_gotowa$baza_same_statusy
                    filtrowanie_bazy_braki_danych_ <- input$filtrowanie_bazy_braki_danych_
                    filtrowanie_bazy <- input$filtrowanie_bazy
                    if (input$uwzglednij_braki_danych && filtrowanie_bazy_braki_danych_ != "" && all(zmienne[stri_detect_regex(filtrowanie_bazy, stri_c("\\b", zmienne, "\\b"))] %in% zmienne[stri_detect_regex(filtrowanie_bazy_braki_danych_, stri_c("\\b", zmienne, "\\b"))]) && all(zmienne[stri_detect_regex(filtrowanie_bazy_braki_danych_, stri_c("\\b", zmienne, "\\b"))] %in% zmienne[stri_detect_regex(filtrowanie_bazy, stri_c("\\b", zmienne, "\\b"))]) && stri_detect_regex(filtrowanie_bazy_braki_danych_, "is\\.na\\(")) {
                        baza_gotowa <- baza_gotowa$baza_gotowa_lacznie_z_brakami_danych
                    } else {
                        baza_gotowa <- baza_gotowa$baza_gotowa_bez_brakow_danych
                    }
                    
                    if (baza_gotowa[, .N] > 0) {
                      losowanie <- as.integer(input$losowanie_kontaktow)
                      if (!is.na(losowanie) && losowanie > 0 && losowanie < baza_gotowa[, .N]) {
                        baza_gotowa <- baza_gotowa[sample(.N, losowanie)]
                      }
                      setkey(baza_same_statusy, telefon_0)
                      setkey(baza_gotowa, telefon_0)
                      
                      baza_same_statusy <- baza_same_statusy[telefon_0 %in% baza_gotowa$telefon_0]
                      
                      baza_same_statusy[, `:=`(najczęstszy_status = fmode(wartość),
                                               ostatni_status = wartość[which.max(data)],
                                               data_ostatniego_statusu = max(data)), by = telefon_0] [, c("data", "wartość") := NULL]
                      
                      incProgress(0.4)
                      baza_same_statusy <- unique(baza_same_statusy, by = "telefon_0")
                      incProgress(0.4)
                      baza_same_statusy[, telefon_0 := NULL]
                      
                      baza_gotowa <- cbind.data.frame(baza_gotowa, baza_same_statusy)
                      baza_gotowa <- as.data.table(baza_gotowa)
                      incProgress(0.2)
                      baza_gotowa <- select(baza_gotowa, matches("^telefon_\\d+$"), nazwa, email, województwo, everything())
                      telefony_same_braki <- which(map_lgl(baza_gotowa[, stri_detect_regex(names(baza_gotowa), "^telefon_\\d+$")], ~ all(stri_detect_regex(., "^brak$"))))
                      if (length(telefony_same_braki) > 0) {
                        baza_gotowa <- baza_gotowa[, .SD, .SDcols = names(baza_gotowa)[-telefony_same_braki]]
                      }
                      baza_gotowa[, telefon_0 := NULL]
                      baza_gotowa <- as.data.table(map_if(baza_gotowa, sprawdzenie_numeric, as.numeric))
                      write_xlsx(baza_gotowa, file)
                      updateCheckboxInput(session = session, inputId = "uwzglednij_braki_danych", value = FALSE)
                      updateTextInput(session = session, inputId = "losowanie_kontaktow", value = NULL)
                    } else {
                      shinyalert(title = "", text = "Próbujesz pobrać pustą bazę, co nie jest możliwe.", type = "info", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                    }
                } else {
                    shinyalert(title = "", text = "Pobranie bazy bez żadnego filtru nie jest możliwe.", type = "info", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                }
            })
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    output$data_badania_sprawdzenie_daty <- renderUI({
        data_1 <- input$data_badania
        data <- ymd(input$data_badania)
        if (data_1 == "") {
            ""
        } else if (is.na(data)) {
            p("Błąd w dacie!", style = "color:red")
        }
    })
    
    wczytaj_baze_plik <- reactive({
        plik <- input$import_bazy
        if (is.null(plik)) {
            return(NULL)
        }
        read_excel(plik$datapath, col_types = "text")
    })
    
    observeEvent(input$importuj_przycisk, {
        data_badania <- ymd(input$data_badania)
        if (!is.na(data_badania)) {
          if (data_badania > Sys.Date()) {
            data_badania <- NA
          }
        }
        if (!is.na(data_badania)) {
            withProgress(message = "Importowanie bazy", {
                wczytana_baza <- wczytaj_baze_plik()
                if (file.exists("Baza_danych/Baza/baza_rolnikow.fst")) {
                  baza_rolnikow <- copy(baza_rolnikow())
                } else {
                  baza_rolnikow <- data.table(telefon_0 = 0L,
                                              telefon_1 = "0",
                                              nazwa = "0",
                                              email = "0",
                                              zmienna = "0",
                                              opis = "0",
                                              data = ymd("1970-01-01"),
                                              sezon = ymd("1970-01-01"),
                                              zmienna_data = "0",
                                              zmienna_sezon = "0",
                                              wartość = "0",
                                              ile_dni_do_użycia = 0L,
                                              zmienna_bez_historii = FALSE)
                  setkey(baza_rolnikow, telefon_0)
                  baza_rolnikow <- baza_rolnikow[FALSE]
                  write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
                }
                if (!is.null(wczytana_baza)) {
                    if (!data_badania %in% baza_rolnikow$data) {
                      names(wczytana_baza) <- stri_trim_both(names(wczytana_baza))
                      if (!any(stri_detect_regex(names(wczytana_baza), "^ma_email \\(\\)$|^ma_email\\(\\)$|^ma_email\\(\\d{4}-\\d{2}-\\d{2}\\)$|^ma_email \\(\\d{4}-\\d{2}-\\d{2}\\)$")) && any(stri_detect_regex(names(wczytana_baza), "^email$"))) {
                        wczytana_baza$`ma_email ()` <- ifelse(stri_detect_fixed(wczytana_baza$email, "@"), "Tak", "Nie")
                      }
                      wczytana_baza_daty <- names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), " \\(.*\\)")]
                      if (length(wczytana_baza_daty > 0)) {
                        wczytana_baza_daty <- unlist(stri_split_fixed(wczytana_baza_daty, " "), use.names = FALSE)
                        wczytana_baza_daty <- stri_trim_both(wczytana_baza_daty)
                        wczytana_baza_daty <- wczytana_baza_daty[stri_detect_regex(wczytana_baza_daty, "^\\(\\)$|^\\(\\d{4}-09-30\\)$")]
                        wczytana_baza_daty <- stri_replace_all_regex(wczytana_baza_daty, "\\(|\\)", "")
                        for (i in 1:length(wczytana_baza_daty)) {
                          if (wczytana_baza_daty[i] == "") {
                            wczytana_baza_daty[i] <- as.character(data_badania)
                          }
                        }
                        obecny_sezon <- zaokraglij_daty_do_sezonow(Sys.Date())
                        if (data_badania != obecny_sezon) {
                          for (i in 1:length(wczytana_baza_daty)) {
                            if (ymd(wczytana_baza_daty[i]) >= obecny_sezon || ymd(wczytana_baza_daty[i]) > data_badania) {
                              wczytana_baza_daty[i] <- "usun"
                            }
                          }
                        }
                        wczytana_baza_daty <- wczytana_baza_daty[stri_detect_fixed(wczytana_baza_daty, "usun", negate = TRUE)]
                        wczytana_baza_daty <- ymd(wczytana_baza_daty)
                        wczytana_baza_daty <- wczytana_baza_daty[!is.na(wczytana_baza_daty)]
                      }
                      names(wczytana_baza) <- stri_replace_all_regex(names(wczytana_baza), "\\s+\\(.*\\)$", "")
                      names(wczytana_baza) <- stri_trim_both(names(wczytana_baza))
                      incProgress(0.05)
                      wczytana_baza <- as.data.table(wczytana_baza)
                      if (all(stri_detect_regex(names(wczytana_baza), "\\n", negate = TRUE)) && all(stri_detect_regex(names(wczytana_baza), "^\\w+$"))) {
                        if (!(any(stri_detect_regex(names(wczytana_baza), "^telefon_0$")) || any(stri_detect_regex(names(wczytana_baza), "^telefon_\\d{2,}$")))) {
                          if (length(wczytana_baza_daty) == length(names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$|^status$|^województwo$|^nazwa$|^email$", negate = TRUE)])) {
                            if (!any(is.na(wczytana_baza[, .SD, .SDcols = names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$|^nazwa$|^email$|^status$|^województwo$")]]))) {
                              test_braki <- sapply(wczytana_baza[, .SD, .SDcols = names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$|^nazwa$|^email$|^status$|^województwo$", negate = TRUE)]], stri_detect_regex, pattern = "^brak$")
                              if (!any(test_braki[!is.na(test_braki)])) {
                                if (any(names(wczytana_baza) == "telefon_1") && any(names(wczytana_baza) == "status") && any(names(wczytana_baza) == "nazwa") && any(names(wczytana_baza) == "email") && any(names(wczytana_baza) == "województwo")) {
                                  if (all(stri_detect_regex(wczytana_baza$telefon_1, "^[0-9]{9}$|^[0-9]{9}\\s"))) {
                                    if (!any(duplicated(stri_sub(wczytana_baza$telefon_1, 1, 9)))) {
                                      if (all(sapply(wczytana_baza[, .SD, .SDcols = names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$")]], stri_detect_regex, pattern = "^[0-9]{9}$|^[0-9]{9}\\s|^brak$"))) {
                                        liczba_kolumn_z_telefonem_wczytana_baza <- length(names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$")])
                                        liczba_kolumn_z_telefonem_baza_rolnikow <- length(names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_[1-9]+$")])
                                        if (liczba_kolumn_z_telefonem_wczytana_baza >= liczba_kolumn_z_telefonem_baza_rolnikow) {
                                          kolumny_z_telefonami_kolejnosc_wczytana_baza <- names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$")]
                                          oczekiwana_kolejnosc <- 1:length(kolumny_z_telefonami_kolejnosc_wczytana_baza)
                                          rzeczywista_kolejnosc <- stri_sub(kolumny_z_telefonami_kolejnosc_wczytana_baza, 9)
                                          if (all(rzeczywista_kolejnosc == oczekiwana_kolejnosc)) {
                                            if (all(slide_lgl(wczytana_baza[, .SD, .SDcols = names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$")]], czy_brak_dziur_w_kolumnach_z_telefonami))) {
                                              duble_w_telefonach_dla_tego_samego_kontaktu <- unlist(slide(as.data.frame(wczytana_baza[, .SD, .SDcols = names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$")]]), czy_brak_dubli_w_telefonach_dla_tego_samego_kontaktu), use.names = FALSE)
                                              if (is.null(duble_w_telefonach_dla_tego_samego_kontaktu)) {
                                                if (all(stri_detect_regex(wczytana_baza$status, "^Brak odpowiedniej osoby$|^Mieszkanie prywatne/firma$|^Nie ma takiego numeru$|^Nieefektywny$|^Nikt nie odbiera$|^Odmowa udziału w jakichkolwiek badaniach$|^Odmowa-respondent$|^Odmowa-sekretariat$|^Poczta głosowa/automatyczna sekretarka/fax$|^Powtórzony numer$|^Przerwany$|^Wymogi$|^Wywiad$|^Zajęte$"))) {
                                                  wczytana_baza[, województwo := stri_trans_tolower(województwo)]
                                                  if (all(stri_detect_regex(wczytana_baza$województwo, "^brak$|^dolnośląskie$|^kujawsko-pomorskie$|^lubelskie$|^lubuskie$|^łódzkie$|^małopolskie$|^mazowieckie$|^opolskie$|^podkarpackie$|^podlaskie$|^pomorskie$|^śląskie$|^świętokrzyskie$|^warmińsko-mazurskie$|^wielkopolskie$|^zachodniopomorskie$"))) {
                                                    incProgress(0.05)
                                                    names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$|^status$|^województwo$|^nazwa$|^email$", negate = TRUE)] <- stri_c(names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^telefon_\\d+$|^status$|^województwo$|^nazwa$|^email$", negate = TRUE)], "_", stri_replace_all_fixed(wczytana_baza_daty, "-", "_"))
                                                    names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^status$|^województwo$")] <- stri_c(names(wczytana_baza)[stri_detect_regex(names(wczytana_baza), "^status$|^województwo$")], "_", stri_replace_all_fixed(data_badania, "-", "_"))
                                                    incProgress(0.1)
                                                    wczytana_baza <- melt.data.table(wczytana_baza, measure.vars = patterns(".+\\d{4}_\\d{2}_\\d{2}$"), variable.name = "zmienna_data", value.name = "wartość", variable.factor = FALSE)
                                                    wczytana_baza <- na.omit(wczytana_baza)
                                                    baza_rolnikow_unikalne <- baza_rolnikow[, .(telefon_0, ile_dni_do_użycia)]
                                                    baza_rolnikow_unikalne <- unique(baza_rolnikow, by = "telefon_0")
                                                    zmienne_bez_historii <- unique(pull(baza_rolnikow[zmienna_bez_historii == TRUE], zmienna))
                                                    wczytana_baza[, `:=`(zmienna = stri_replace_all_regex(zmienna_data, "_\\d{4}_\\d{2}_\\d{2}$", ""),
                                                                         data = ymd(stri_replace_all_fixed(stri_sub(zmienna_data, -10L), "_", "-")),
                                                                         opis = "",
                                                                         telefon_0 = as.integer(stri_sub(telefon_1,  1L, 9L)))][, sezon := zaokraglij_daty_do_sezonow(data)][, zmienna_sezon := stri_c(zmienna, "_", stri_replace_all_fixed(sezon, "-", "_"))][, ile_dni_do_użycia := unlist(lapply(telefon_0, ustal_ile_dni_do_uzycia_dla_importowanej_bazy, baza_rolnikow_unikalne = baza_rolnikow_unikalne), use.names = FALSE)][, zmienna_bez_historii := fifelse(zmienna %in% zmienne_bez_historii, TRUE, FALSE)]
                                                    wczytana_baza <- select(wczytana_baza, telefon_0, matches("^telefon_\\d+$"), nazwa, email, zmienna, opis, data, sezon, zmienna_data, zmienna_sezon, wartość, ile_dni_do_użycia, zmienna_bez_historii)
                                                    incProgress(0.2)
                                                    zmienne_wczytana_baza <- unique(wczytana_baza$zmienna)
                                                    for (i in 1:length(zmienne_wczytana_baza)) {
                                                      if (zmienne_wczytana_baza[i] %in% unique(baza_rolnikow$zmienna)) {
                                                        wczytana_baza[zmienna == zmienne_wczytana_baza[i], opis := first(baza_rolnikow[zmienna == zmienne_wczytana_baza[i], opis])]
                                                      }
                                                    }
                                                    kolumny_dodatkowe_z_telefonami <- kolumny_z_telefonami_kolejnosc_wczytana_baza[!kolumny_z_telefonami_kolejnosc_wczytana_baza %chin% names(baza_rolnikow)]
                                                    if (length(kolumny_dodatkowe_z_telefonami) > 0) {
                                                      baza_rolnikow[, (kolumny_dodatkowe_z_telefonami) := "brak"]
                                                    }
                                                    baza_rolnikow <- select(baza_rolnikow, telefon_0, matches("^telefon_\\d+$"), nazwa, email, zmienna, opis, data, sezon, zmienna_data, zmienna_sezon, wartość, ile_dni_do_użycia, zmienna_bez_historii)
                                                    baza_rolnikow <- rbindlist(list(baza_rolnikow, wczytana_baza), use.names = FALSE)
                                                    
                                                    baza_rolnikow_wojewodztwa <- baza_rolnikow[zmienna == "województwo"]
                                                    baza_rolnikow <- baza_rolnikow[zmienna != "województwo"]
                                                    baza_rolnikow_wojewodztwa <- baza_rolnikow_wojewodztwa[, licz_wojewodztwa := seq_len(.N), by = telefon_0][, wartosc_1 := fifelse(licz_wojewodztwa > 1L & wartość == "brak", "usun", "ok")][wartosc_1 == "ok"][, `:=`(licz_wojewodztwa = NULL,
                                                                                                                                                                                                                                                                           wartosc_1 = NULL)]
                                                    baza_rolnikow_wojewodztwa <- baza_rolnikow_wojewodztwa[order(-data)]
                                                    baza_rolnikow_wojewodztwa <- unique(baza_rolnikow_wojewodztwa, by = "telefon_0")
                                                    baza_rolnikow_statusy <- baza_rolnikow[zmienna == "status"]
                                                    baza_rolnikow <- baza_rolnikow[zmienna != "status"]
                                                    baza_rolnikow_statusy <- baza_rolnikow_statusy[order(-data)][, licz_statusy := seq_len(.N), by = telefon_0][licz_statusy <= 20L][, licz_statusy := NULL]
                                                    baza_rolnikow <- baza_rolnikow[, najnowsze := max(data), by = .(telefon_0, zmienna_sezon)][data == najnowsze][, najnowsze := NULL]
                                                    incProgress(0.1)
                                                    baza_rolnikow <- rbindlist(list(baza_rolnikow, baza_rolnikow_wojewodztwa, baza_rolnikow_statusy), use.names = FALSE)

                                                    baza_rolnikow[, id := .GRP, by = telefon_0]
                                                    incProgress(0.1)
                                                    baza_rolnikow_telefony_id <- baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^id$|^telefon_[1-9]+$")]]
                                                    baza_rolnikow_telefony_id[, names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")] := lapply(.SD, stri_sub, from = 1L, to = 9L), .SDcols = names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")]]
                                                    baza_rolnikow_telefony_id <- unique(baza_rolnikow_telefony_id)
                                                    baza_rolnikow_telefony_id <- melt.data.table(baza_rolnikow_telefony_id, id.vars = "id", variable.name = "rodzaj_telefonu", value.name = "telefon", variable.factor = FALSE)
                                                    baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[, rodzaj_telefonu := NULL]
                                                    baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[telefon != "brak"]
                                                    baza_rolnikow_telefony_id[, telefon := as.integer(telefon)]
                                                    baza_rolnikow_telefony_id[, duble := .GRP, by = telefon]
                                                    baza_rolnikow_telefony_id[, zdublowane_id := stri_c(id, collapse = ", "), by = duble]
                                                    baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[, .(zdublowane_id)]
                                                    baza_rolnikow_telefony_id_duble <- baza_rolnikow_telefony_id[, test := lapply(zdublowane_id,  stri_detect_fixed, pattern = ", ")]
                                                    baza_rolnikow_telefony_id_duble <- baza_rolnikow_telefony_id_duble[test == TRUE]
                                                    baza_rolnikow_telefony_id_duble[, test := NULL]
                                                    baza_rolnikow_telefony_id_duble[, zdublowane_id_temp := lapply(zdublowane_id, function(x) stri_c(sort(unlist(stri_split_fixed(x, ", "), use.names = FALSE)), collapse = ", "))]
                                                    baza_rolnikow_telefony_id_duble <- baza_rolnikow_telefony_id_duble[, .(zdublowane_id = unlist(zdublowane_id_temp, use.names = FALSE))]
                                                    incProgress(0.1)
                                                    if (baza_rolnikow_telefony_id_duble[, .N] > 0) {
                                                      baza_rolnikow_telefony_id_duble <- unique(baza_rolnikow_telefony_id_duble, by = "zdublowane_id")
                                                      baza_rolnikow_telefony_id_duble <- baza_rolnikow_telefony_id_duble[, zdublowane_id := fifelse(stri_detect_fixed(zdublowane_id, ","), zdublowane_id, NA_character_)][!is.na(zdublowane_id)]
                                                      baza_rolnikow_telefony_id_duble <- baza_rolnikow_telefony_id_duble[, .(zdublowane_id = unlist(lapply(zdublowane_id, zredukuj_liste_zdublowanych_id, zdublowane_id_wszystkie = zdublowane_id), use.names = FALSE))]
						      baza_rolnikow_telefony_id_duble <- unique(baza_rolnikow_telefony_id_duble, by = "zdublowane_id")
                                                      same_id <- unique(unlist(map(baza_rolnikow_telefony_id_duble$zdublowane_id, ~ as.integer(unlist(stri_split_fixed(., ", "), use.names = FALSE)))))
                                                      liczba_kolumn_z_telefonami <- length(names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_[1-9]+$")])
                                                      liczba_kolumn_z_telefonami_do_dziewieciu <- 9 - liczba_kolumn_z_telefonami
                                                      if (liczba_kolumn_z_telefonami_do_dziewieciu > 0) {
                                                        liczenie_od_liczby <- 9 - liczba_kolumn_z_telefonami_do_dziewieciu
                                                        nazwy_kolumn_z_telefonami_do_dziewieciu <- stri_c("telefon_", (liczenie_od_liczby + 1):9, sep = "")
                                                        baza_rolnikow[, (nazwy_kolumn_z_telefonami_do_dziewieciu) := "brak"]
                                                      }
                                                      baza_rolnikow_zdublowane_id <- map_dfr(baza_rolnikow_telefony_id_duble$zdublowane_id, ujednolic_informacje_dla_zdublowanych_id, baza_rolnikow = baza_rolnikow)
                                                      
                                                      baza_rolnikow <- baza_rolnikow[!id %in% same_id]
                                                      baza_rolnikow <- rbindlist(list(baza_rolnikow, baza_rolnikow_zdublowane_id), use.names = FALSE)
                                                    }
                                                    baza_rolnikow[, id := NULL]
                                                    baza_rolnikow <- select(baza_rolnikow, matches("^telefon_\\d+$"), nazwa, email, zmienna, opis, data, sezon, zmienna_data, zmienna_sezon, wartość, ile_dni_do_użycia, zmienna_bez_historii)
                                                    ktore_telefony_z_samymi_brakami <- unlist(map(baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$")]], ~ all(stri_detect_regex(., "^brak$"))), use.names = FALSE)
                                                    if (length(ktore_telefony_z_samymi_brakami) > 0) {
                                                      baza_rolnikow <- baza_rolnikow[, .SD, .SDcols = c(names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$")][!ktore_telefony_z_samymi_brakami], names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$", negate = TRUE)])]
                                                    }
                                                    baza_rolnikow <- baza_rolnikow[order(-data)]
                                                    baza_rolnikow[, names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$|^nazwa$|^email$")] := lapply(.SD, first), by = telefon_0, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$|^nazwa$|^email$")]]
                                                    baza_rolnikow_bez_historii <- baza_rolnikow[zmienna_bez_historii == TRUE]
                                                    baza_rolnikow <- baza_rolnikow[zmienna_bez_historii == FALSE]
                                                    baza_rolnikow_bez_historii[, `:=`(data = zaokraglij_daty_do_sezonow(Sys.Date()),
                                                                                      sezon = zaokraglij_daty_do_sezonow(Sys.Date()))][, `:=`(zmienna_data = stri_c(zmienna, "_", stri_replace_all_fixed(data, "-", "_")),
                                                                                                                                              zmienna_sezon = stri_c(zmienna, "_", stri_replace_all_fixed(sezon, "-", "_")))]
                                                    baza_rolnikow_bez_historii <- unique(baza_rolnikow_bez_historii, by = c("telefon_0", "zmienna"))
                                                    baza_rolnikow <- rbindlist(list(baza_rolnikow, baza_rolnikow_bez_historii), use.names = FALSE)
                                                    setkey(baza_rolnikow, telefon_0)
													                          baza_rolnikow <- unique(baza_rolnikow)
                                                    write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
                                                    baza_rolnikow(baza_rolnikow)
                                                    baza_rolnikow(baza_rolnikow)
                                                    incProgress(0.3)
                                                    updateTextAreaInput(session = session, inputId = "data_badania", value = "")
                                                  } else {
                                                    shinyalert(title = "Niepoprawne dane!", text = "Nie wszystkie kontakty mają określone województwo z puli dozwolonych województw.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                                  }
                                                } else {
                                                  shinyalert(title = "Niepoprawne dane!", text = "Nie wszystkie kontaty mają określony status z puli dozwolonych statusów.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                                }
                                              } else {
                                                shinyalert(title = "Niepoprawne dane!", text = stri_c("W kontaktach o następujących telefonach_1:", stri_c(duble_w_telefonach_dla_tego_samego_kontaktu, collapse = ", "), " na kolejnych kolumnach z numerami są duble względem innych numerów dla tego samego kontaktu.", sep = " "), type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                              }
                                            } else {
                                              shinyalert(title = "Niepoprawne dane!", text = "We wszystkich kolumnach z telefonami musi być tak, że jeśli w danej kolumnie występuje \"brak\", to wszystkie dalsze kolumny z telefonami też muszą posiadać słowo \"brak\", a nie numer telefonu. Nie jest to zachowane w importowanej bazie.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                            }
                                          } else {
                                            shinyalert(title = "Niepoprawne dane!", text = "W importowanej bazie kolejność kolumn z telefonem nie jest rosnąca lub jest rosnąca, ale liczba określająca kolejny numer telefonu nie wzrasta co 1.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                          }
                                        } else {
                                          shinyalert(title = "Niepoprawne dane!", text = stri_c("W importowanej bazie jest ", liczba_kolumn_z_telefonem_wczytana_baza, " kolumn z telefonami, natomiast w bazie rolników jest", liczba_kolumn_z_telefonem_baza_rolnikow, "kolumn z telefonami. W importowanej bazie może być więcej lub tyle samo kolumn z telefonami, ale nie mniej.", sep = " "), type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                        }
                                      } else {
                                        shinyalert(title = "Niepoprawne dane!", text = "W kolumnach z drugimi numerami telefonów i kolejnymi są dozwolone tylko: same numery dziewięciocyfrowe nieoddzielane żadnym znakiem, komórki zaczynające się od dziewięciocyfrowych numerów nieoddzielanych żadnych znakiem, po których następuje spacja (dalej może być dowolna treść) lub samo słowo \"brak\". Nie jest to zachowane w importowanej bazie.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                      }
                                    } else {
                                      shinyalert(title = "Niepoprawne dane!", text = "W kolumnie \"telefon_1\" są zdublowane numery.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                    }
                                  } else {
                                    shinyalert(title = "Niepoprawne dane!", text = "W kolumnie \"telefon_1\" są dozwolone tylko: same numery dziewięciocyfrowe nieoddzielane żadnym znakiem lub komórki zaczynające się od dziewięciocyfrowych numerów nieoddzielanych żadnych znakiem, po których następuje spacja (dalej może być dowolna treść). Nie jest to zachowane w importowanej bazie.", type = "warning", closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonCol = "#955251")
                                  }
                                } else {
                                  shinyalert(title = "Niepoprawne dane!", text = "Wśród nazw kolumn nie ma kolumny \"status\" lub \"telefon_1\" lub \"nazwa\" lub \"email\" lub \"województwo\".", type = "warning", confirmButtonCol = "#955251", closeOnClickOutside = TRUE, closeOnEsc = TRUE)
                                }
                              } else {
                                shinyalert(title = "Niepoprawne dane!", text = "W zmiennych stworzonych przez użytkownika nie może być słowa \"brak\". Jeśli ma być brak danych, to komórka powinna pozostać pusta.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                              }
                            } else {
                              shinyalert(title = "Niepoprawne dane!", text = "W importowanej bazie w kolumnach z telefonami, nazwą, emailem, statusem i województwem nie może być pustych komórek. Można wpisać \"brak\", jeśli ma być brak danych, ale braki danych nie są dozwolone w kolumnach \"status\" i \"telefon_1\".", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                            }
                          } else {
                            shinyalert(title = "Niepoprawne dane!", text = "Błędnie określono daty w importowanym pliku - być może dopisano puste nawiasy lub nawiasy z datami przy zmiennych określających telefony, statusy, nazwy, emaile lub województwa; lub daty zostały wpisane przy prawidłowych zmiennych, ale nie miały odpowiedniego formatu (rrrr-mm-dd, gdzie w \"mm\" może być tylko \"09\", a w \"dd\" \"30\") lub przy którejś zmiennej wpisano datę końca sezonu, który w momencie przeprowadzania badania jeszcze się nie skończył, datę końca sezonu z przyszłości bądź datę równą lub większą niż data badania.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                          }
                        } else {
                          shinyalert(title = "Niepoprawne dane!", text = "W importowanej bazie nie może być kolumny o nazwie \"telefon_0\" ani jakakolwiek kolumna z telefonem nie może mieć w nazwie dwucyfrowego numeru. Jeden z tych błędów lub oba wystepują w importowanej bazie.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                        }
                      } else {
                        shinyalert(title = "Niepoprawne dane!", text = "W importowanej bazie nazwy kolumny mogą zawierać tylko litery, cyfry i podkreślenia oraz nie może być w nich twardych spacji. Któryś z tych warunków nie jest zachowany.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                      }
                    } else {
                        shinyalert(title = "Niepoprawne dane!", text = "Już zaimportowano badanie o takiej dacie przeprowadzenia, wpisz inną datę.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                    }
                } else {
                    shinyalert(title = "Niepoprawne dane!", text = "Nie wybrano pliku.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
                }
            })
        } else {
            shinyalert(title = "Niepoprawne dane!", text = "Nie określono prawidłowo daty badania, w tym być może wpisano datę badania większą niż dzisiejsza data.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
        }
    })
    
    observeEvent(input$statusy_do_zawieszenia_przycisk, {
        withProgress(message = "Zawieszanie", {
          baza_rolnikow <- copy(baza_rolnikow())
          statusy_do_zawieszenia <- input$statusy_do_zawieszenia
          liczba_statusow_do_przeszukania <- as.integer(input$statusy_do_zawieszenia_liczba_statusow)
          liczba_dni_do_zawieszenia <- as.integer(input$statusy_do_zawieszenia_liczba_dni)
          incProgress(0.2)
          if (!(is.na(liczba_statusow_do_przeszukania) || is.na(liczba_dni_do_zawieszenia) || is.null(statusy_do_zawieszenia))) {
            telefony <- baza_rolnikow[zmienna == "status"][order(-data)]
            incProgress(0.2)
            telefony <- unique(pull(telefony[telefony[, .I[1:liczba_statusow_do_przeszukania], by = telefon_0]$V1][, wynik := all(wartość %chin% statusy_do_zawieszenia), by = telefon_0][wynik == TRUE], telefon_0))
            incProgress(0.2)
            baza_rolnikow[telefon_0 %in% telefony, ile_dni_do_użycia := liczba_dni_do_zawieszenia]
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.4)
            updateCheckboxGroupInput(session = session, inputId = "statusy_do_zawieszenia", selected = FALSE)
            updateNumericInput(session = session, inputId = "statusy_do_zawieszenia_liczba_statusow", value = 20)
            updateNumericInput(session = session, inputId = "statusy_do_zawieszenia_liczba_dni", value = 1)
          } else {
            shinyalert(title = "Niepoprawne dane!", text = "Liczba statusów do przeszukania lub liczba dni do zawieszenia nie jest liczbą lub też nie wybrano żadnego statusu. Możliwe, że liczba dni do zawieszenia jest zbyt duża, większa niż 1999999999.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
          }
        })
    })
    
    observeEvent(input$kontakty_do_zawieszenia, {
        withProgress(message = "Zawieszanie", {
          baza_rolnikow <- copy(baza_rolnikow())
          kontakty_do_zawieszenia <- input$zawieszenie_kontaktu
          liczba_dni_do_zawieszenia <- as.integer(input$kontakty_do_zawieszenia_liczba_dni)
          if (kontakty_do_zawieszenia != "" && !is.na(liczba_dni_do_zawieszenia)) {
            baza_rolnikow[, id := .GRP, by = telefon_0]
            incProgress(0.2)
            baza_rolnikow_telefony_id <- baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^id$|^telefon_[1-9]+$")]]
            baza_rolnikow_telefony_id <- unique(baza_rolnikow_telefony_id, by = "id")
            baza_rolnikow_telefony_id[, names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")] := lapply(.SD, stri_sub, from = 1L, to = 9L), .SDcols = names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")]]
            incProgress(0.2)
            baza_rolnikow_telefony_id <- melt.data.table(baza_rolnikow_telefony_id, id.vars = "id", variable.name = "rodzaj_telefonu", value.name = "telefon", variable.factor = FALSE)
            baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[, rodzaj_telefonu := NULL]
            baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[telefon != "brak"]
            incProgress(0.2)
            kontakty_do_zawieszenia <- as.integer(stri_replace_all_fixed(unlist(stri_split_fixed(kontakty_do_zawieszenia, "\n"), use.names = FALSE), ",", replacement = ""))
            kontakty_do_zawieszenia <- kontakty_do_zawieszenia[!is.na(kontakty_do_zawieszenia)]
            id_do_zawieszenia <- ifelse(kontakty_do_zawieszenia %in% baza_rolnikow_telefony_id$telefon, baza_rolnikow_telefony_id$id[kontakty_do_zawieszenia == baza_rolnikow_telefony_id$telefon], NA)
            baza_rolnikow[id %in% id_do_zawieszenia, ile_dni_do_użycia := liczba_dni_do_zawieszenia][, id := NULL]
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.4)
            updateTextAreaInput(session = session, inputId = "zawieszenie_kontaktu", value = "")
            updateNumericInput(session = session, "kontakty_do_zawieszenia_liczba_dni", value = 1)
          } else{
            shinyalert(title = "Niepoprawne dane!", text = "Nie podano żadnych numerów telefonów lub liczba dni do zawieszenia nie jest liczbą. Możliwe, że liczba dni do zawieszenia jest zbyt duża, większa niż 1999999999.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
          }
        })
    })
    
    output$pobierz_kontakty_zawieszone <- downloadHandler(
      filename = function() {
        "Kontakty_zawieszone.xlsx"
      },
      content = function(file) {
        withProgress(message = "Pobieranie kontaktów", {
          baza_rolnikow <- baza_rolnikow()
          kontakty_zawieszone <- unique(baza_rolnikow[ile_dni_do_użycia > 0L, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_[1-9]+$|^nazwa$|^email$|^ile_dni_do_użycia$")]], by = "telefon_1")
          write_xlsx(kontakty_zawieszone, file)
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    wczytaj_kontakty_do_przywrocenia <- reactive({
      plik <- input$przywracanie_kontaktow_zawieszonych
      if (is.null(plik)) {
        return(NULL)
      }
      read_excel(plik$datapath, col_types = "text")
    })
    
    observeEvent(input$przywracanie_kontaktow_zawieszonych_przycisk, {
        wczytany_plik <- wczytaj_kontakty_do_przywrocenia()
        if (!is.null(wczytany_plik)) {
          if (names(wczytany_plik)[1] == "telefon_1" && nrow(wczytany_plik) > 0) {
            withProgress(message = "Przywracanie", {
              telefony <- na.omit(as.integer(stri_sub(wczytany_plik$telefon_1, 1L, 9L)))
              incProgress(0.5)
              baza_rolnikow <- copy(baza_rolnikow())
              baza_rolnikow[telefon_0 %in% telefony, ile_dni_do_użycia := 0L]
              write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
              baza_rolnikow(baza_rolnikow)
              incProgress(0.5)
            })
          } else {
            shinyalert("Niepoprawne dane!", "Wczytywany plik musi posiadać kolumnę \"telefon_1\" jako pierwszą oraz mieć jakiekolwiek dane oprócz nagłówków kolumn.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
          }
        } else {
          shinyalert("Niepoprawne dane!", "Nie wybrano pliku.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
        }
    })
    
    observeEvent(input$statusy_do_usuniecia_przycisk, {
        withProgress(message = "Usuwanie", {
          baza_rolnikow <- baza_rolnikow()
          statusy_do_usuniecia <- input$statusy_do_usuniecia
          liczba_statusow_do_przeszukania <- as.integer(input$statusy_do_usuniecia_liczba_statusow)
          incProgress(0.2)
          if (!(is.na(liczba_statusow_do_przeszukania) || is.null(statusy_do_usuniecia))) {
            telefony <- baza_rolnikow[zmienna == "status"][order(-data)]
            incProgress(0.2)
            telefony <- unique(pull(telefony[telefony[, .I[1:liczba_statusow_do_przeszukania], by = telefon_0]$V1][, wynik := all(wartość %chin% statusy_do_usuniecia), by = telefon_0][wynik == TRUE], telefon_0))
            incProgress(0.2)
            baza_rolnikow <-  baza_rolnikow[!telefon_0 %in% telefony]
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.4)
            updateCheckboxGroupInput(session = session, inputId = "statusy_do_usuniecia", selected = FALSE)
            updateNumericInput(session = session, inputId = "statusy_do_usuniecia_liczba_statusow", value = 20)
          } else {
            shinyalert(title = "Niepoprawne dane!", text = "Liczba statusów do przeszukania nie jest liczbą lub też nie wybrano żadnego statusu.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
          }
        })
    })
    
    observeEvent(input$usuwanie_telefonow_przycisk, {
        withProgress(message = "Usuwanie", {
          baza_rolnikow <- copy(baza_rolnikow())
          telefony_do_usuniecia <- input$usuwanie_telefonow
          if (telefony_do_usuniecia != "") {
            telefony_do_usuniecia <- as.integer(stri_replace_all_fixed(unlist(stri_split_fixed(telefony_do_usuniecia, "\n"), use.names = FALSE), ",", replacement = ""))
            telefony_do_usuniecia <- telefony_do_usuniecia[!is.na(telefony_do_usuniecia)]
            baza_rolnikow[, id := .GRP, by = telefon_0]
            incProgress(0.2)
            baza_rolnikow_telefony_id <- baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^id$|^telefon_[1-9]+$")]]
            baza_rolnikow_telefony_id <- unique(baza_rolnikow_telefony_id, by = "id")
            baza_rolnikow_telefony_id[, names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")] := lapply(.SD, stri_sub, from = 1L, to = 9L), .SDcols = names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")]]
            incProgress(0.2)
            baza_rolnikow_telefony_id <- melt.data.table(baza_rolnikow_telefony_id, id.vars = "id", variable.name = "rodzaj_telefonu", value.name = "telefon", variable.factor = FALSE)
            baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[telefon != "brak"]
            incProgress(0.1)
            id_rodzaj_telefonu <- ifelse(telefony_do_usuniecia %in% baza_rolnikow_telefony_id$telefon, stri_c(baza_rolnikow_telefony_id$id[telefony_do_usuniecia == baza_rolnikow_telefony_id$telefon], baza_rolnikow_telefony_id$rodzaj_telefonu[telefony_do_usuniecia == baza_rolnikow_telefony_id$telefon], sep = ", "), NA)
            id_rodzaj_telefonu <- id_rodzaj_telefonu[!is.na(id_rodzaj_telefonu)]
            for (i in seq_along(id_rodzaj_telefonu)) {
              oddzielone <- unlist(stri_split_fixed(id_rodzaj_telefonu[i], ", "), use.names = FALSE)
              oddzielone_1 <- as.integer(oddzielone[1])
              oddzielone_2 <- oddzielone[2]
              baza_rolnikow[id == oddzielone_1, (oddzielone_2) := "brak"]
              wszystkie_telefony <- unlist(slide(unique(baza_rolnikow[id == oddzielone_1, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "telefon_[1-9]+$")]]), ~ .), use.names = FALSE)
              wszystkie_telefony <- c(wszystkie_telefony[wszystkie_telefony != "brak"], wszystkie_telefony[wszystkie_telefony == "brak"])
              baza_rolnikow[id == oddzielone_1, names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_[1-9]+$")] := lapply(wszystkie_telefony, function(x) rep(x, baza_rolnikow[id == oddzielone_1, .N]))]
              baza_rolnikow[id == oddzielone_1, telefon_0 := as.integer(stri_sub(telefon_1, 1L, 9L))]
            }
            baza_rolnikow[, id := NULL]
            ktore_telefony_z_samymi_brakami <- unlist(map(baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$")]], ~ all(stri_detect_regex(., "^brak$"))), use.names = FALSE)
            if (length(ktore_telefony_z_samymi_brakami) > 0) {
              baza_rolnikow <- baza_rolnikow[, .SD, .SDcols = c(names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$")][!ktore_telefony_z_samymi_brakami], names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^telefon_\\d+$", negate = TRUE)])]
            }
            baza_rolnikow <- baza_rolnikow[!is.na(telefon_0)]
            setkey(baza_rolnikow, telefon_0)
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.5)
            updateTextAreaInput(session = session, inputId = "usuwanie_telefonow", value = "")
          } else {
            shinyalert(title = "Niepoprawne dane!", text = "Nie podano żadnych numerów telefonów.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
          }
        })
    })
    
    observeEvent(input$usuwanie_kontaktow_przycisk, {
        withProgress(message = "Usuwanie", {
          baza_rolnikow <- copy(baza_rolnikow())
          kontakty_do_usuniecia <- input$usuwanie_kontaktow
          if (kontakty_do_usuniecia != "") {
            baza_rolnikow[, id := .GRP, by = telefon_0]
            incProgress(0.2)
            baza_rolnikow_telefony_id <- baza_rolnikow[, .SD, .SDcols = names(baza_rolnikow)[stri_detect_regex(names(baza_rolnikow), "^id$|^telefon_[1-9]+$")]]
            baza_rolnikow_telefony_id <- unique(baza_rolnikow_telefony_id, by = "id")
            baza_rolnikow_telefony_id[, names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")] := lapply(.SD, stri_sub, from = 1L, to = 9L), .SDcols = names(baza_rolnikow_telefony_id)[stri_detect_regex(names(baza_rolnikow_telefony_id), "^telefon_\\d+$")]]
            incProgress(0.2)
            baza_rolnikow_telefony_id <- melt.data.table(baza_rolnikow_telefony_id, id.vars = "id", variable.name = "rodzaj_telefonu", value.name = "telefon", variable.factor = FALSE)
            baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[, rodzaj_telefonu := NULL]
            baza_rolnikow_telefony_id <- baza_rolnikow_telefony_id[telefon != "brak"]
            incProgress(0.2)
            kontakty_do_usuniecia <- as.integer(stri_replace_all_fixed(unlist(stri_split_fixed(kontakty_do_usuniecia, "\n"), use.names = FALSE), ",", replacement = ""))
            kontakty_do_usuniecia <- kontakty_do_usuniecia[!is.na(kontakty_do_usuniecia)]
            id_do_usuniecia <- ifelse(kontakty_do_usuniecia %in% baza_rolnikow_telefony_id$telefon, baza_rolnikow_telefony_id$id[kontakty_do_usuniecia == baza_rolnikow_telefony_id$telefon], NA)
            baza_rolnikow <- baza_rolnikow[!id %in% id_do_usuniecia]
            baza_rolnikow[, id := NULL]
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.4)
            updateTextAreaInput(session = session, inputId = "usuwanie_kontaktow", value = "")
          } else{
            shinyalert(title = "Niepoprawne dane!", text = "Nie podano żadnych numerów telefonów.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
          }
        })
    })
    
    observeEvent(input$dodawanie_zmienianie_opisu_zmiennych_przycisk, {
        withProgress(message = "Dodawanie/zmienianie opisu", {
          polecenie <- input$dodawanie_zmienianie_opisu_zmiennych
          if (polecenie != "") {
            polecenie <- unlist(stri_split_fixed(polecenie, "\n"), use.names = FALSE)
            baza_rolnikow <- copy(baza_rolnikow())
            incProgress(0.5)
            for (i in 1:length(polecenie)) {
              zmienna_opis <- unlist(stri_split_fixed(polecenie[i], "=", n = 2), use.names = FALSE)
              zmienna_opis <- stri_trim_both(zmienna_opis)
              if (!(stri_detect_regex(zmienna_opis[1], "^status$|^województwo$"))) {
                baza_rolnikow[zmienna == zmienna_opis[1], opis := zmienna_opis[2]]
              }
            }
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.5)
            updateTextAreaInput(session = session, inputId = "dodawanie_zmienianie_opisu_zmiennych", value = "")
          } else {
            shinyalert("Niepoprawne dane!", "Nic nie zostało wpisane w danym polu.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
          }
        })
    })
    
    observeEvent(input$zmienianie_nazw_zmiennych_przycisk, {
        withProgress(message = "Zmienianie nazw zmiennych", {
          polecenie <- input$zmienianie_nazw_zmiennych
          if (polecenie != "") {
            polecenie <- unlist(stri_split_fixed(polecenie, "\n"), use.names = FALSE)
            baza_rolnikow <- copy(baza_rolnikow())
            incProgress(0.5)
            for (i in 1:length(polecenie)) {
              stare_nowe_zmienne <- unlist(stri_split_fixed(polecenie[i], "="), use.names = FALSE)
              stare_nowe_zmienne <- stri_trim_both(stare_nowe_zmienne)
              if (length(stare_nowe_zmienne) == 2) {
                if (!(any(stri_detect_regex(stare_nowe_zmienne, "^status$|^województwo$"))) && all(stri_detect_regex(stare_nowe_zmienne[2], "^\\w+$")) && !stare_nowe_zmienne[1] == "ma_email") {
                  baza_rolnikow[zmienna == stare_nowe_zmienne[1], `:=`(zmienna = stare_nowe_zmienne[2],
                                                                       zmienna_data = stri_c(stare_nowe_zmienne[2], "_", stri_replace_all_fixed(data, "-", '_')),
                                                                       zmienna_sezon = stri_c(stare_nowe_zmienne[2], "_", stri_replace_all_fixed(sezon, "-", '_')))]
                } else {
                  shinyalert("Niepoprawne dane!", "Wśród nazw znajduje się słowo \"status\" lub \"województwo\" bądź nadano niedozwoloną nazwę zmiennej - można używać cyfr, liter, podkreślenia. Nie można także zmienić nazwy zmiennej \"ma_email\"", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
                }
              }
              baza_rolnikow <- baza_rolnikow[order(-data)]
              baza_rolnikow <- unique(baza_rolnikow, by = c("telefon_0", "zmienna_sezon"))
              setkey(baza_rolnikow, telefon_0)
              write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
              baza_rolnikow(baza_rolnikow)
              incProgress(0.5)
              updateTextAreaInput(session = session, inputId = "zmienianie_nazw_zmiennych", value = "")
            }
          } else {
            shinyalert("Niepoprawne dane!", "Nic nie zostało wpisane w danym polu.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
          }
        })
    })
    
    observeEvent(input$rekodowanie_odpowiedzi_przycisk, {
        withProgress(message = "Rekodowanie", {
          polecenie <- input$rekodowanie_odpowiedzi
          if (polecenie != "") {
            polecenie <- unlist(stri_split_fixed(polecenie, "\n"), use.names = FALSE)
            baza_rolnikow <- copy(baza_rolnikow())
            incProgress(0.5)
            for (i in 1:length(polecenie)) {
              pary_do_rekodowania <- unlist(stri_split_fixed(polecenie[i], "=", n = 2), use.names = FALSE)
              pary_do_rekodowania_stare <- unlist(stri_split_fixed(pary_do_rekodowania[1], ",", n = 2), use.names = FALSE)
              pary_do_rekodowania_nowe <- unlist(stri_split_fixed(pary_do_rekodowania[2], ",", n = 2), use.names = FALSE)
              pary_do_rekodowania_stare <- stri_trim_both(pary_do_rekodowania_stare)
              pary_do_rekodowania_nowe <- stri_trim_both(pary_do_rekodowania_nowe)
              if (length(pary_do_rekodowania_stare) == 2 && length(pary_do_rekodowania_nowe) == 2) {
                if (!any(stri_detect_regex(c(pary_do_rekodowania_stare[1], pary_do_rekodowania_nowe[1]), "^status$|^województwo$")) && all(stri_detect_regex(pary_do_rekodowania_nowe[1], "^\\w+$")) && !any(stri_detect_regex(c(pary_do_rekodowania_stare[2], pary_do_rekodowania_nowe[2]), "\"|'")) && !(pary_do_rekodowania_stare[1] == "ma_email" && pary_do_rekodowania_nowe[1] != "ma_email")) {
                  baza_rolnikow[zmienna == pary_do_rekodowania_stare[1] & wartość == pary_do_rekodowania_stare[2], `:=`(zmienna = pary_do_rekodowania_nowe[1],
                                                                                                                        zmienna_data = stri_c(pary_do_rekodowania_nowe[1], "_", stri_replace_all_fixed(data, "-", '_')),
                                                                                                                        zmienna_sezon = stri_c(pary_do_rekodowania_nowe[1], "_", stri_replace_all_fixed(sezon, "-", '_')),
                                                                                                                        wartość = pary_do_rekodowania_nowe[2])]
                } else {
                  shinyalert("Niepoprawne dane!", "Wśród nazw zmiennych znajduje się słowo \"status\" lub \"województwo\" bądź nadano niedozwoloną nazwę zmiennej - można używać cyfr, liter, podkreślenia. Oprócz tego, nie można zmienić nazwy zmiennej \"ma_email\". W nazwach wartości nie można również używać cudzysłowu.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
                }
              }
            }
            baza_rolnikow <- baza_rolnikow[order(-data)]
            baza_rolnikow <- unique(baza_rolnikow, by = c("telefon_0", "zmienna_sezon"))
            setkey(baza_rolnikow, telefon_0)
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.5)
            updateTextAreaInput(session = session, inputId = "rekodowanie_odpowiedzi", value = "")
          } else {
            shinyalert("Niepoprawne dane!", "Nic nie zostało wpisane w danym polu.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
          }
        })
    })
    
    output$zmienne_bez_historii_lista <- renderUI({
        baza_rolnikow <- baza_rolnikow()
        zmienne <- sort(unique(pull(baza_rolnikow[zmienna_bez_historii == TRUE], zmienna)))
        selectInput(inputId = "zmienne_bez_historii_lista_", label = "Zmienne bez historii", choices = zmienne, multiple = FALSE)
    })
    
    output$zmienne_bez_historii_dodaj_usun <- renderUI({
        zmienne <- lista_zmiennych()
        selectInput(inputId = "zmienne_bez_historii_dodaj_usun_", label = "Wszystkie zmienne", choices = zmienne, multiple = TRUE)
    })
    
    observeEvent(input$zmienne_bez_historii_dodaj_przycisk, {
        withProgress(message = "Dodawanie", {
          baza_rolnikow <- copy(baza_rolnikow())
          incProgress(0.2)
          wybrane_zmienne <- input$zmienne_bez_historii_dodaj_usun_
          if (!is.null(wybrane_zmienne)) {
            baza_rolnikow[zmienna %in% wybrane_zmienne, zmienna_bez_historii := TRUE]
            baza_rolnikow_bez_historii <- baza_rolnikow[zmienna_bez_historii == TRUE]
            baza_rolnikow <- baza_rolnikow[zmienna_bez_historii == FALSE]
            incProgress(0.3)
            baza_rolnikow_bez_historii[, `:=`(data = zaokraglij_daty_do_sezonow(Sys.Date()),
                                              sezon = zaokraglij_daty_do_sezonow(Sys.Date()))][, `:=`(zmienna_data = stri_c(zmienna, "_", stri_replace_all_fixed(data, "-", "_")),
                                                                                                      zmienna_sezon = stri_c(zmienna, "_", stri_replace_all_fixed(sezon, "-", "_")))]
            baza_rolnikow_bez_historii <- unique(baza_rolnikow_bez_historii, by = c("telefon_0", "zmienna"))
            baza_rolnikow <- rbindlist(list(baza_rolnikow, baza_rolnikow_bez_historii), use.names = FALSE)
            setkey(baza_rolnikow, telefon_0)
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.5)
            updateSelectInput(session = session, inputId = "zmienne_bez_historii_dodaj_usun_", selected = "")
          } else {
            shinyalert("Niepoprawne dane!", "Nie wybrano żadnych zmiennych.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
          }
        })
    })
    
    observeEvent(input$zmienne_bez_historii_usun_przycisk, {
        withProgress(message = "Usuwanie", {
          baza_rolnikow <- copy(baza_rolnikow())
          incProgress(0.5)
          wybrane_zmienne <- input$zmienne_bez_historii_dodaj_usun_
          if (!is.null(wybrane_zmienne)) {
            baza_rolnikow[zmienna %in% wybrane_zmienne, zmienna_bez_historii := FALSE]
            write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
            baza_rolnikow(baza_rolnikow)
            incProgress(0.5)
            updateSelectInput(session = session, inputId = "zmienne_bez_historii_dodaj_usun_", selected = "")
          } else {
            shinyalert("Niepoprawne dane!", "Nie wybrano żadnych zmiennych.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
          }
        })
    })
    
    output$pobierz_zmienne_daty <- downloadHandler(
        filename = function() {
            "zmienne_daty.xlsx"
        },
        content = function(file) {
            zmienne_daty <- baza_rolnikow()
            zmienne_daty <- zmienne_daty[, .(zmienna, sezon, zmienna_sezon)]
            zmienne_daty <- unique(zmienne_daty, by = "zmienna_sezon")
            zmienne_daty <- zmienne_daty %>%
                filter(!zmienna %in% c("status", "województwo")) %>%
                arrange(zmienna, sezon) %>%
                select(-zmienna_sezon) %>%
                write_xlsx(file)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    
    wczytaj_zmienne_daty_plik <- reactive({
        plik <- input$usuwanie_zmiennych_dat
        if (is.null(plik)) {
            return(NULL)
        }
        read_excel(plik$datapath, col_types = "text")
    })
    
    observeEvent(input$usuwanie_zmiennych_dat_przycisk, {
        dane_do_usuniecia <- wczytaj_zmienne_daty_plik()
        if (!is.null(dane_do_usuniecia)) {
          withProgress(message = "Usuwanie danych", {
            if (ncol(dane_do_usuniecia) == 2 && colnames(dane_do_usuniecia) == c("zmienna", "sezon") && nrow(dane_do_usuniecia) != 0) {
              baza_rolnikow <- baza_rolnikow()
              incProgress(0.5)
              dane_do_usuniecia <- dane_do_usuniecia %>% 
                filter(!zmienna %in% c("status", "województwo")) %>%
                mutate(zmienna_sezon = stri_c(zmienna, "_", stri_replace_all_fixed(sezon, "-", "_")))
              baza_rolnikow <- baza_rolnikow[!zmienna_sezon %chin% dane_do_usuniecia$zmienna_sezon]
              write_fst(baza_rolnikow, "Baza_danych/Baza/baza_rolnikow.fst")
              baza_rolnikow(baza_rolnikow)
              incProgress(0.5)
            } else {
              shinyalert("Niepoprawne dane!", "Wczytywany plik jest niepoprawny - musi się składać z dwóch kolumn o nazwach w podanej kolejności: \"zmienna\" i \"sezon\" oraz mieć jakiekolwiek dane oprócz nagłówków kolumn.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
            }
          })
        } else {
          shinyalert("Niepoprawne dane!", "Nie wybrano pliku.", type = "warning", closeOnClickOutside = TRUE, confirmButtonCol = "#955251", closeOnEsc = TRUE)
        }
    })
    
    observeEvent(input$zapisuj_kopie_przycisk, {
        withProgress(message = "Zapisywanie", file.copy("Baza_danych/Baza/baza_rolnikow.fst", "Baza_danych/Kopia_uzytkownika/baza_rolnikow.fst", overwrite = TRUE))
    })
    
    observeEvent(input$przywracaj_kopie_uzytkownika_przycisk, {
        withProgress(message = "Przywracanie", {
          file.copy("Baza_danych/Kopia_uzytkownika/baza_rolnikow.fst", "Baza_danych/Baza/baza_rolnikow.fst", overwrite = TRUE)
          baza_rolnikow <- read_fst("Baza_danych/Baza/baza_rolnikow.fst", as.data.table = TRUE)
          baza_rolnikow(baza_rolnikow)
          })
    })
    
    output$data_kopii_uzytkownika <- renderText({
        data_kopii <- data_kopii_uzytkownika()
        if (is.na(data_kopii)) {
          ""
        } else {
          data_kopii
        }
    })
    
    output$daty_kopii_automatycznych <- renderUI({
        selectInput(inputId = "daty_kopii_automatycznych_wybor", choices = c("Wybierz datę", sort(dir("Baza_danych/Kopia_automatyczna"), decreasing = TRUE)), selected = "Wybierz datę", label = NULL)
    })
    
    observeEvent(input$przywracaj_kopie_automatyczne_przycisk,{
        if (input$daty_kopii_automatycznych_wybor != "Wybierz datę") {
            withProgress(message = "Przywracanie", {
              file.copy(stri_c("Baza_danych/Kopia_automatyczna/", input$daty_kopii_automatycznych_wybor, "/", "baza_rolnikow.fst"), "Baza_danych/Baza/baza_rolnikow.fst", overwrite = TRUE)
              baza_rolnikow <- read_fst("Baza_danych/Baza/baza_rolnikow.fst", as.data.table = TRUE)
              baza_rolnikow(baza_rolnikow)
              updateSelectInput(session = session, inputId = "daty_kopii_automatycznych_wybor", selected = "Wybierz datę")
              })
        } else {
            shinyalert(title = "Niepoprawne dane!", text = "Nie wybrano daty.", type = "warning", closeOnEsc = TRUE, closeOnClickOutside = TRUE, confirmButtonCol = "#955251")
        }
    })
    
    output$instrukcja_pobierz <- downloadHandler(
        filename = function() {
            "Instrukcja_baza_rolników.pdf"
        },
        content = function(file) {
            file.copy("Instrukcja/Instrukcja_baza_rolnikow.pdf", file)
        },
        contentType = "application/pdf"
    )
}

shinyApp(ui = ui, server = server)

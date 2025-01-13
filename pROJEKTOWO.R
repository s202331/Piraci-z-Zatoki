# Piraci-z-Zatoki sklep rowerowy
### Wstep Klienci sklepu rowerowego wzięli udział w ankiecie, w której dostarczyli szczegółowe informacje na swój temat, takie jak status cywilny, płeć, poziom dochodów, liczba dzieci, poziom wykształcenia, wykonywany zawód, status posiadania domu, liczba samochodów, odległość do miejsca pracy, region zamieszkania oraz wiek. W badaniu uwzględniono również informację, czy klient zakupił rower. Celem analizy jest określenie, które z tych czynników mają największy wpływ na decyzję o zakupie roweru.
## Data Wrangling
### zaladowanie potrzebnych pakietów 
# Instalacja i załadowanie wszystkich wymaganych pakietów
install.packages(c("readr", "naniar", "dplyr", "tidyr", "ggplot2", "mice", "rpart","ggcorrplot"))
library(readr)
library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mice)
library(rpart)
library(ggcorrplot)

# Podstawowa Analiza braków
n_miss(sklep_rowerowy) # Sprawdzamy ilość NA w pliku

vis_miss(sklep_rowerowy) # wizualizacja NA

miss_var_summary(sklep_rowerowy) # Podsumowanie braków w kolumnach

braki_procent <- sklep_rowerowy %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Kolumna", values_to = "Procent")

ggplot(braki_procent, aes(x = reorder(Kolumna, -Procent), y = Procent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Procent brakujących danych w kolumnach",
       x = "Kolumna", y = "% braków") +
  theme_minimal()      # wizualizacja procentowa braków w każdej kolumnie

vis_miss(sklep_rowerowy, cluster = TRUE) # Klasteryzacja braków dla porównania różnych kolumn


str(sklep_rowerowy)

sklep_rowerowy <- sklep_rowerowy %>%
  mutate(
    `Marital.Status` = na_if(`Marital.Status`, ""),
    Gender = na_if(Gender, ""),
    `Home.Owner` = na_if(`Home.Owner`, ""),
    `Marital.Status` = factor(`Marital.Status`),
    Gender = factor(Gender),
    Education = factor(Education),
    Occupation = factor(Occupation),
    `Home.Owner` = factor(`Home.Owner`),
    `Commute.Distance` = factor(`Commute.Distance`),
    Region = factor(Region),
    `Purchased.Bike` = factor(`Purchased.Bike`)
  )
str(sklep_rowerowy)

### Zmienne liczbowe - średnia adaptacyjna
sklep_rowerowy <- sklep_rowerowy %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE, trim = 0.1), .)))

### Zmienne kategoryczne - imputacja metodą `pmm`

imputed_data <- mice(sklep_rowerowy, m = 5, method = 'pmm', seed = 123)
sklep_rowerowy <- complete(imputed_data)

sklep_rowerowy

n_miss(sklep_rowerowy)

## Wizualizacja braków danych po imputacji
vis_miss(sklep_rowerowy) +
  labs(title = "Braki danych po imputacji")


--------------------------------------------------------------------------------
  sklep_rowerowy %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Rozkład zmiennych liczbowych", x = "Wartość", y = "Częstość") +
  theme_minimal()   # wizualizacja zmiennych liczbowych




sklep_rowerowy %>%
  select(where(is.factor)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Rozkład zmiennych kategorycznych", x = "Kategorie", y = "Liczba obserwacji") +
  theme_minimal()  #wizualizacja zmiennych kategorycznych


## Korelacja zmiennych liczbowych
cor_matrix <- cor(sklep_rowerowy %>% select(where(is.numeric)), use = "complete.obs")
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)


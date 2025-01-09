---
editor_options: 
title: "Sklep rowerowy"
author: "Jagoda Chęcińska, Piotr Łukowski, Tomasz Kotliński"
  markdown: 
    wrap: 72
---

# Piraci-z-Zatoki sklep rowerowy
### Wstep Klienci sklepu rowerowego wzięli udział w ankiecie, w której dostarczyli szczegółowe informacje na swój temat, takie jak status cywilny, płeć, poziom dochodów, liczba dzieci, poziom wykształcenia, wykonywany zawód, status posiadania domu, liczba samochodów, odległość do miejsca pracy, region zamieszkania oraz wiek. W badaniu uwzględniono również informację, czy klient zakupił rower. Celem analizy jest określenie, które z tych czynników mają największy wpływ na decyzję o zakupie roweru.
## Data Wrangling
### zaladowanie potrzebnych pakietów 
# Instalacja i załadowanie wszystkich wymaganych pakietów
install.packages(c("readr", "naniar", "dplyr", "tidyr", "ggplot2", "mice", "rpart"))
library(readr)
library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mice)
library(rpart)
### sprawdzamy ile mamy NA w pliku
number_of_NA <- sum(is.na(sklep_rowerowy)) 
(number_of_NA)

###sprawdzamy które wiersze są brakujące 
brakujace_wiersze <-sklep_rowerowy[complete.cases(sklep_rowerowy) == FALSE, ]
(brakujace_wiersze)

### Obliczenie liczby kolumn

liczba_kolumn <- ncol(sklep_rowerowy) 
### Wyświetlenie liczby kolumn
(liczba_kolumn)

### Sprawdzamy liczbę braków w każdej kolumnie

braki_w_kolumnach <- colSums(is.na(sklep_rowerowy))
(braki_w_kolumnach) 
### Instalujemy pakiet naniar
install.packages("naniar") 
library(naniar)

### sprawdzamy ile jest NA w pliku 
n_miss(sklep_rowerowy)

###Proporcja wartoci,które są NA prop_miss(sklep_rowerowy)
print(paste("Proporcja NA (prop_miss):", prop_miss(sklep_rowerowy)))

### Procent wartości, które są NA pct_miss(sklep_rowerowy)
print(paste("Procent NA (pct_miss):", pct_miss(sklep_rowerowy)))

### Tabela podsumowująca liczby NA w tabeli.
miss_var_summary(sklep_rowerowy)

### Tabela podsumowująca NA według przypadku (obserwacji)
miss_case_table(sklep_rowerowy)

### Wizualizacja lokalizacji NA jako obiektu ggplot
vis_miss(sklep_rowerowy)

###Tworzy mapę cieplną liczby NA według grupy Purchased_Bike
gg_miss_fct(sklep_rowerowy, fct = Purchased.Bike)

### Używamy UpSet plot do wizualizacji przecięć NA (tj.nakładania się NA lub współwystępowania) między zmiennymi.

gg_miss_upset(sklep_rowerowy, nsets=12)

### dodajemy lokalizacje wartości NA wzdłuż osi ggplot

vis_miss(sklep_rowerowy) + geom_miss_point()

### usunięcie pierwszego wiersza if (names(sklep_rowerowy)[1] == "ID") {
if (names(sklep_rowerowy)[1] == "ID") {
  sklep_rowerowy <- sklep_rowerowy[-1, ]
}

### instalacja i wczytanie pakietów install.packages("mice")
install.packages(c("mice", "lattice"))
library(mice)
library(lattice)
# Imputacja Danych
### Wyświetlenie podglądu braków przed imputacją

md.pattern(sklep_rowerowy) 


imputed_data <- mice(sklep_rowerowy, method = "pmm", m = 5, maxit = 50,
seed = 123)

### Podgląd wyników imputacji

summary(imputed_data)

### Uzyskanie kompletnego zestawu danych po imputacji

completed_data <- complete(imputed_data, 1) 
### 1 oznacza pierwszyzestaw imputowanych danych

### Wyświetlenie pierwszych kilku wierszy danych po imputacji

print(head(completed_data))
###sprawdzenie kompletnosci danych
sum(is.na(completed_data))


# Wizualizacje 


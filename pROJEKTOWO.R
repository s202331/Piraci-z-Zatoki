---
  title: "Sklep rowerowy Analysis"
author: "Jagoda Chęcińska, Piotr Łukowski, Tomasz Kotliński"
date: "`r Sys.Date()`"
output: html_document
---

---
# Piraci-z-Zatoki sklep rowerowy
### Wstep Klienci sklepu rowerowego wzięli udział w ankiecie, w której dostarczyli szczegółowe informacje na swój temat, takie jak status cywilny, płeć, poziom dochodów, liczba dzieci, poziom wykształcenia, wykonywany zawód, status posiadania domu, liczba samochodów, odległość do miejsca pracy, region zamieszkania oraz wiek. W badaniu uwzględniono również informację, czy klient zakupił rower. Celem analizy jest określenie, które z tych czynników mają największy wpływ na decyzję o zakupie roweru.
## Data Wrangling
### zaladowanie potrzebnych pakietów 
# Instalacja i załadowanie wszystkich wymaganych pakietów
install.packages(c("readr", "naniar", "dplyr", "tidyr", "ggplot2", "mice", "rpart","ggcorrplot","rpart.plot" , "factoextra"))
library(readr)
library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mice)
library(rpart)
library(ggcorrplot)
library(rpart.plot)
library(factoextra)
---
  
---
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
---
  
---
### Zmienne liczbowe - średnia adaptacyjna
sklep_rowerowy <- sklep_rowerowy %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE, trim = 0.1), .)))

### Zmienne kategoryczne - imputacja metodą `pmm`

imputed_data <- mice(sklep_rowerowy, m = 5, method = 'pmm', seed = 123)
sklep_rowerowy <- complete(imputed_data)

sklep_rowerowy

n_miss(sklep_rowerowy)

---
  
  
---
## Wizualizacja braków danych po imputacji
vis_miss(sklep_rowerowy) +
  labs(title = "Braki danych po imputacji")


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


---

---
#Poniższe wykresy przedstawiają rozkłady danych dla wybranych zmiennych kategorycznych.

# Marital Status
ggplot(sklep_rowerowy, aes(x = `Marital.Status`)) +
  geom_bar() +
  labs(title = "Rozkład stanu cywilnego", x = "Stan cywilny", y = "Liczba osób")
# Gender 
ggplot(sklep_rowerowy, aes(x = `Gender`)) +
  geom_bar() +
  labs(title = "Rozkład płci", x = "Płeć", y = "Liczba osób")
#  Home Owner
ggplot(sklep_rowerowy, aes(x = `Home.Owner`)) +
  geom_bar() +
  labs(title = "Rozkład własności domu", x = "Czy posiada dom", y = "Liczba osób")


---


---
## Korelacja zmiennych liczbowych
cor_matrix <- cor(sklep_rowerowy %>% select(where(is.numeric)), use = "complete.obs")
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)


---
  
---
## Model drzewa decyzyjnego
set.seed(123)
train_index <- sample(seq_len(nrow(sklep_rowerowy)), size = 0.7 * nrow(sklep_rowerowy))
train_data <- sklep_rowerowy[train_index, ]
test_data <- sklep_rowerowy[-train_index, ]

# Budowa drzewa decyzyjnego
tree_model <- rpart(`Purchased.Bike` ~ ., data = train_data, method = "class")
tree_model
# Wizualizacja drzewa decyzyjnego z ulepszeniami
rpart.plot(tree_model, type = 4, extra = 104, fallen.leaves = TRUE, 
           box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

# Przewidywanie na zbiorze testowym
tree_predictions <- predict(tree_model, test_data, type = "class")

# Macierz pomyłek
conf_matrix <- table(Predicted = tree_predictions, Actual = test_data$`Purchased.Bike`)
(conf_matrix)

# Obliczenie dokładności modelu
accuracy <- mean(tree_predictions == test_data$`Purchased.Bike`)
cat("📊 Dokładność modelu drzewa decyzyjnego:", round(accuracy * 100, 2), "%\n")
---
---  
## Segmentacja klientów (Klasteryzacja K-średnich)
cluster_data <- sklep_rowerowy %>% select(where(is.numeric))
cluster_data_scaled <- scale(cluster_data)

fviz_nbclust(cluster_data_scaled, kmeans, method = "wss")

set.seed(123)
kmeans_model <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

fviz_cluster(kmeans_model, data = cluster_data_scaled, geom = "point") +
  labs(title = "Segmentacja klientów - Klasteryzacja K-średnich")

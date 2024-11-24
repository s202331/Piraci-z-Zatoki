# Piraci-z-Zatoki

#SZANTA TO PIEKNA PIESN MORSKA
# sprawdzamy ile mamy NA w pliku
number_of_NA <- sum(is.na(rowery2))
print(number_of_NA)
#mamy 4 NA w pliku
#sprawdzamy które wiersze są brakujące
brakujace_wiersze <- rowery2[complete.cases(rowery2) == FALSE, ]
print(brakujace_wiersze)

# Obliczenie liczby kolumn 
liczba_kolumn <- ncol(rowery2) 
# Wyświetlenie liczby kolumn 
print(liczba_kolumn)


# Sprawdzamy liczbę braków w każdej kolumnie
braki_w_kolumnach <- colSums(is.na(rowery2))
print(braki_w_kolumnach)
#Instalujemy pakiet naniar
install.packages("naniar")
library(naniar)

#sprawdzamy ile jest NA w pliku
n_miss(rowery2)

#Proporcja wartoci,które są NA
prop_miss(rowery2)

#Procent wartości, które są NA
pct_miss(rowery2)

#Tabela podsumowująca liczby NA w tabeli.
miss_var_summary(rowery2)

#Tabela podsumowująca NA według przypadku (obserwacji)
miss_case_table(rowery2)

#Wizualizacja lokalizacji NA jako obiektu ggplot
vis_miss(rowery2)

#Tworzy mapę cieplną liczby NA według grupy Country
gg_miss_fct(rowery2, fct = Country)

# Używamy UpSet plot do wizualizacji przecięć NA (tj. nakładania się NA lub współwystępowania) między zmiennymi.
gg_miss_upset(rowery2, nsets=17)

#dodajemy lokalizacje wartości NA wzdłuż osi ggplot

geom_miss_point()

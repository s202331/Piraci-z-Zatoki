# Piraci-z-Zatoki

#SZANTA TO PIEKNA PIESN MORSKA
# sprawdzamy ile mamy NA w pliku
number_of_NA <- sum(is.na(sklep_rowerowy))
print(number_of_NA)

#sprawdzamy które wiersze są brakujące
brakujace_wiersze <- sklep_rowerowy[complete.cases(sklep_rowerowy) == FALSE, ]
print(brakujace_wiersze)

# Obliczenie liczby kolumn 
liczba_kolumn <- ncol(sklep_rowerowy) 
# Wyświetlenie liczby kolumn 
print(liczba_kolumn)


# Sprawdzamy liczbę braków w każdej kolumnie
braki_w_kolumnach <- colSums(is.na(sklep_rowerowy))
print(braki_w_kolumnach)
#Instalujemy pakiet naniar
install.packages("naniar")
library(naniar)

#sprawdzamy ile jest NA w pliku
n_miss(sklep_rowerowy)

#Proporcja wartoci,które są NA
prop_miss(sklep_rowerowy)

#Procent wartości, które są NA
pct_miss(sklep_rowerowy)

#Tabela podsumowująca liczby NA w tabeli.
miss_var_summary(sklep_rowerowy)

#Tabela podsumowująca NA według przypadku (obserwacji)
miss_case_table(sklep_rowerowy)

#Wizualizacja lokalizacji NA jako obiektu ggplot
vis_miss(sklep_rowerowy)

#Tworzy mapę cieplną liczby NA według grupy Purchased_Bike
gg_miss_fct(sklep_rowerowy, fct = Purchased_Bike)

# Używamy UpSet plot do wizualizacji przecięć NA (tj. nakładania się NA lub współwystępowania) między zmiennymi.
gg_miss_upset(sklep_rowerowy, nsets=12)

#dodajemy lokalizacje wartości NA wzdłuż osi ggplot

geom_miss_point()

#usuniecie pierwszego wiersza
if (names(sklep_rowerowy)[1]  == "ID") {
  sklep_rowerowy <- sklep_rowerowy[-1]
}
sklep_rowerowy

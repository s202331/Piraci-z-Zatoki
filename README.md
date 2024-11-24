# Piraci-z-Zatoki

#SZANTA TO PIEKNA PIESN MORSKA
# sprawdzamy ile mamy NA w pliku
number_of_NA <- sum(is.na(rowery2))
print(number_of_NA)
#mamy 4 NA w pliku
#sprawdzamy które wiersze są brakujące
brakujace_wiersze <- rowery2[complete.cases(rowery2) == FALSE, ]
print(brakujace_wiersze)


# Sprawdzamy liczbę braków w każdej kolumnie
braki_w_kolumnach <- colSums(is.na(rowery2))
print(braki_w_kolumnach)








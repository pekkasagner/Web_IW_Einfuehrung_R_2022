# Autor: Pekka Sagner
# Zuletzt bearbeitet: 8.10.2021
# Inhalt: Übungsblatt 1 - Musterlösung

#Aufgabe 6 
#b)
1 + 2
(1 + 2) * 3
(1 + 2) * 3 - 6
((1 + 2) * 3 - 6) / 4
log(((1 + 2) * 3 - 6) / 4) 

#c)
vektor_1 <- c(2, 9, -4, 0)
vektor_1

#d)
#Hilfsfunktion wird mit ? vor dem Funktionsnamen genutzt.
#Beispiel:
?c()
#weiteres Beispiel:
?mean()

#e)
#Arithmetisches Mittel:
mean_vektor_1 <- mean(vektor_1)
#Minimum:
min_vektor_1 <- min(vektor_1)
#Maximum
max_vektor_1 <- max(vektor_1)
#Median
median_vektor_1 <- median(vektor_1)
#betrachten der Objekte:
mean_vektor_1
min_vektor_1
max_vektor_1
median_vektor_1

#f)
v_2 <- c(2, 9, NA, -4, 0)
#Mittelwert von v_2 bestimmen:
mean(v_2)
#NA steht für Leere Werte in R.
#Um dennoch den Durchschnitt zu bestimmen, nutzt man na.rm = TRUE
?mean()
#Funktion mit na.rm anwenden
mean(v_2, na.rm = TRUE)


#Aufgabe 7:
#Erstellen der Skalare:
vektor_drei <- 3
vektor_pi <- pi
vektor_euler <- exp(1)
vektor_log10 <- log(10)

#Logischer Test, Beispiele:
vektor_drei == vektor_pi
vektor_euler > vektor_log10
vektor_pi < vektor_log10


#Aufgabe 8
#a) Verbinden der Skalare zu einem Vektor
vektor_verbunden <- c(vektor_drei, vektor_pi, vektor_euler, vektor_log10)
vektor_verbunden

#b) Datentyp
typeof(vektor_verbunden)

#c)
vektor_verbunden_char <- c(vektor_verbunden, "hallo")
vektor_verbunden_char
#typ:
typeof(vektor_verbunden_char)

#d)
#double:
double_vector <- c(4.3, 2.8, 4, 5.3)
#character:
char_vector <- c("hallo", "schlechtes", "wetter", "heute")
#logical:
log_vector <- c(TRUE, FALSE, T, F)

#prüfen Datentypen:
typeof(log_vector)

#e) 
factor_vector <- factor(char_vector)
factor_vector

#mit typeof prüfen:
typeof(factor_vector)

#Klasse prüfen:
class(factor_vector)

#f)
factor_gender <- factor(c("diverse", "female", "male"))
factor_gender

#Levels Reihenfolge ändern
factor_gender_reordered <- factor(factor_gender, 
                                  levels = c("female", "male", "diverse"))
factor_gender_reordered





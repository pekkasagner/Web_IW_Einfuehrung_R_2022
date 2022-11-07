#Autor: Pekka Sagner
#Übung 1: Erste Schritte mit R und R-Studio
#----------------------------------------------------

#Aufgabe 6: a/b)
#i)
ergebnis_1 <- 1+2

#ii)
ergebnis_2 <- ergebnis_1*3
#Alternative
(1+2)*3

#iii)
ergebnis_3 <- ergebnis_2-6

#iv)
ergebnis_4 <- ergebnis_3/4

#v)
ergebnis_final <- log(ergebnis_4)

#6c)
vektor_1 <- c(2, 9, -4, 0)

#6d)
?c()

#6e)
mean_vektor_1 <- mean(vektor_1)
mean_vektor_1

min_vektor_1 <- min(vektor_1)
min_vektor_1

max_vektor_1 <- max(vektor_1)
max_vektor_1

median_vektor_1 <- median(vektor_1)
median_vektor_1

#f)
v_2 <- c(2, 9, NA, -4, 0)
          
mean(v_2, na.rm = TRUE)
min(v_2, na.rm = T)
max(v_2, na.rm = T)
median(v_2, na.rm = T)

#Aufgabe 7
#a)
vektor_3 <- 3
vektor_pi <- pi
(vektor_euler <- exp(1))
vektor_log <- log(10)

#b)logische Tests durchführen
vektor_log > vektor_euler

vektor_pi < vektor_3

vektor_euler > vektor_log & vektor_euler < vektor_pi

vektor_pi > vektor_3

vektor_pi != vektor_3

vektor_log == vektor_3

#Aufgabe 8
#a)
vektor_kombi <- c(vektor_3 , vektor_pi, vektor_euler, vektor_log)
#b)
typeof(vektor_kombi)
#c)
vektor_kombi_skalar <- c(vektor_kombi, "hallo")
typeof(vektor_kombi_skalar)
vektor_kombi_skalar <- as.numeric(vektor_kombi_skalar)

#d)
double_vektor <- c(5, 1.2, 30, 9.5)
character_vektor <- c("hallo", "tschüss", "hochschule", "april")
logical_vektor <- c(TRUE, FALSE, F, T)

length(double_vektor)
#Datentyp prüfen
typeof(double_vektor)
typeof(character_vektor)
typeof(logical_vektor)

#e)
character_vektor_as_factor <- factor(character_vektor) 
typeof(character_vektor_as_factor)
class(character_vektor_as_factor)

#f)
gender_vektor <- factor(c("diverse", "female", "male"))

gender_vektor_rearranged <- factor(gender_vektor, levels = c("female", "male", "diverse"))

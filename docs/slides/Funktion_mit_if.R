library(tidyverse)

(daten <- tibble(x = c(1, NA, 2, 1),
                 y = c(3, 4, 4, 4),
                 z = c(NA, NA, "A", "B"))
)


#b)

my_share_function <- function(data, vector) {
          data |> 
                    count({{ vector }}) |> 
                    mutate(share = n/sum(n))
}


daten |> 
          my_share_function(x)



#c)
#Vektor numerisch?
my_num_check <- function(data, vector) {
          if(is.numeric(data |> 
                        pull({{ vector }})) == TRUE) {
                    print("Vektor ist numerisch.")
          }
          
          if(is.numeric(data |> 
                        pull({{ vector }} )) == FALSE) {
                    print("Vektor ist nicht numerisch.")
          }
}

my_num_check(daten, z)


is.numeric(daten |> 
                     pull(x) )





my_mean_function <- function(data, vector) {
          
          if(any(is.na(data |> pull( {{ vector }} ))) == TRUE) {print("Achtung, Missings im Vektor! na.rm = T angewendet.")
                    
                    result <- data |>
                              summarise(mean = mean( {{ vector }}, na.rm = T) ) 
                    return(result) }
          
          if(all(!is.na(data |>
                       pull({{ vector}}))) == TRUE) {print("Keine Missings im Vektor. na.rm = T nicht notwendig.")

                    result <- data |>
                              summarise(mean = mean( {{ vector }}) ) 
                    return(result) }
}

my_mean_function(daten, y)


daten |> 
          my_mean_function(x)




my_check_function <- function(data, vector) {
          
          if(is.numeric(data |> 
                        pull({{ vector }})) == TRUE) {
                    print("Vektor ist numerisch.")
          
          
                    if(any( data |> pull( {{ vector }} ) > 3) == TRUE)   {
                              print("Vektor enhält Werte größer 3.")
                              
                    }
          
                    if(all( data |> pull( {{ vector }} ) <= 3) == TRUE)   {
                              print("Vektor enhält keine Werte größer 3.")
                              
                    }
          }
                    
          if(is.numeric(data |> 
                        pull({{ vector }})) == FALSE) {
                    print("Vektor ist nicht numerisch.")
          }
}

my_check_function(daten, y)


any(daten |> 
          pull(y) > 4) 

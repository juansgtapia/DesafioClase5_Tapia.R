getwd()

life_expectancy <- read.csv("life_expectancy.csv", sep =",", header = TRUE)

library(tidyverse)

#Me interesa saber la informacion para cada 5 años, para ver la evolucion de la expectivativa de vida
#Filtro para los años 2000, 2005, 2010 y 2015
life_expectancy_evolution <- life_expectancy %>%
  filter(Year %in% c(2000, 2005, 2010, 2015))

#Imprimo el dataset filtrado para ver que todo esta bien
#print(life_expectancy_evolution)


#Me interesa analizar los valores para el PBI Per Capita, pero hay observaciones que no tienen valores
#Introducire valores para la observacion United States, en las observaciones para los años 2000,2005,2010,2015

life_expectancy_evolution_2 <- life_expectancy_evolution %>%
  mutate(Population = ifelse(Country == "United States of America" & Year == 2000, 200282200, #Si esta condicion no se cumple
                      ifelse(Country == "United States of America" & Year == 2005, 295000000, #Se pasa a esta siguiente, y si es correcta, se ejecuta
                      ifelse (Country == "United States of America" & Year == 2010, 309300000, #Si no, se pasa a esta condicion
                      ifelse(Country == "United States of America" & Year == 2015, 320700000, Population))))) #Si no se cumple la condicion anterior, ni la presente, no se hacen cambios.

#Como he detectado que los datos para Estados Unidos no tiene datos para algunas variables, las agrego mediante la funcion mutate

life_expectancy_evolution_3 <- life_expectancy_evolution_2 %>%
  mutate(GDP = ifelse(Country == "United States of America" & Year == 2000, 36330,
                      ifelse(Country == "United States of America" & Year == 2005, 44123,
                      ifelse (Country == "United States of America" & Year == 2010, 48650,
                      ifelse(Country == "United States of America" & Year == 2015, 56763, GDP)))))


str(life_expectancy_evolution_3)

#Me interesa saber si el status de el pais tiene pocos valores unicos 

unique(life_expectancy_evolution_3$Status)

#Veo que hay dos tipos, developing y developed. Me interesa tomar esos datos como factores y jerarquizarlos

life_expectancy_evolution_3 <- life_expectancy_evolution_3 %>%
  mutate(Status=as.factor(Status)) #Convierto en Status

life_expectancy_evolution_3 <- life_expectancy_evolution_3 %>%
  mutate(Status = relevel(Status, ref ="Developed"))

str(life_expectancy_evolution_3) #Compruebo los cambios efectuados.


#Me interesa agrupar la informacion para los años entre 2000 y 2015 para ver como evoluciono la expectativa de vida y otras metricas durante esos 10 años

metrica_1 <- life_expectancy_evolution_3 %>%
  group_by(Year,Status) %>%
  summarise(
    Mean_life_expectancy = mean(Life.expectancy),
    Mean_Infantile_deaths = mean(infant.deaths),
    Mean_Expenditure = mean(Total.expenditure, na.rm = TRUE),
    Mean_BMI = mean(BMI, na.rm = TRUE)
    
  )

print("Este dataset muestra como evoluciono la expectativa de vida y sus parametros relacionados entre 2000 y 2015, cada 5 años, para los paises desarrollados y en desarrollo")

print(metrica_1)



#Tengo la hipotesis de que ciertos paises (que son no desarrollados en el dataframe) tienen mayor expectativa de vida que ciertos paises desarrollados siempre y cuando tengan mas de 13000 dolares de PBI Per Capita
#Los voy a llamar "Paises_clase_media"

paises_clase_media <- life_expectancy_evolution_3 %>%
  filter(Status == "Developing", GDP > 13000, Year == 2015 )

#Como obtengo muchos paises, quiero ver el caso de los 3 paises del sur de america y defino los paises desarrolados

cono_sur <- paises_clase_media %>%
  filter(Country == "Argentina" | Country == "Chile" | Country == "Uruguay")

paises_desarollados <- life_expectancy_evolution_3 %>%
  filter(Status == "Developed", Year == 2015)


#Me interesa contastar la informacion obtenida en el dataframe conosur 

promedio_life_expectancy_cono_sur <- mean(cono_sur$Life.expectancy, na.rm = TRUE)

#Creo el dataset para los paises con menor expectativa de vida que el promedio del cono sur y lo llamare metrica_3

metrica_2 <- paises_desarollados %>%
  filter(!is.na(Life.expectancy), Life.expectancy < promedio_life_expectancy_cono_sur)

print("Estos paises considerados desarrollados tienen una expectativa de vida menor a la expectativa de vida del Cono Sur")
print(metrica_2)



#Repaso de los datasets


#el dataset metrica_1 muestra la evolucion cada 5 años de los paises desarrollados y en desarrollo en todos los parametros del dataset original, haciendo foco en la expectativa de vida a traves de las funciones group by y summarise
#el dataset metrica_2 muestra los paises considerados desarrollados en el dataset original que tienen un promedio de expectativa de vida menor al promedio de expectativa de vida del conosur
#este analisis de la metrica_2 es extrapolable para los paises definidos como paises_clase_media, que eran aquellos considerados "Developing" en su varible status y con un valor numero mayor a 13000 en su GDP, que representa su PBI Per Capita



  
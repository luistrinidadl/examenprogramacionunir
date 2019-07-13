
#Estas operaciones se realizaron con la base de datos de las elecciones en la República Dominicana del 2016, a nivel presidencial.


#Se Cargaron las siguientes bibliotecas
library(ggplot2) 
library(tidyr)
library(tidyverse)
library(readr)
library(readxl)

#Profesor en última instancia convertí el archivo excel en csv e hice en el enlace con un git para vea que también lo pude hacer por esa vía pero el resto de la limpieza y análisis lo hice con el archivo excel.
filePath <- "https://raw.githubusercontent.com/luistrinidadl/luistrinidadl.github.io/master/Resultados%20Electorales%202016%20(Presidencial).csv"

csv <- read_csv(filePath) 

#Con read_excel hicimos lectura del documento xlsx Resultados Electorales 2016 (Presidencial)

Resultados_Electorales_2016_Presidencial_ <- read_excel("Desktop/Resultados Electorales 2016 (Presidencial).xlsx")
Votospresidenciales <- Resultados_Electorales_2016_Presidencial_ %>% 
  clean_names() %>% 
  remove_empty('cols') %>% 
  remove_empty('rows') %>% 
  #Algunas de limpiezas como de nombres, remover columnas filas y columnas facias. 
  mutate( 
    codigo_prov= formatC(cod_prov,width = 2, flag = '0'),
    codigo_muni= formatC(cod_mun,width = 2, flag = '0'),
  
cod = paste(codigo_prov,codigo_muni, sep = ''),

#Igual que España, el padrón electoral tiene códigos para las provincias, hasta dos cifras, y los munucipios, hasta tres cifras. Con este procedimiento lo unifique en una sola columna.

pld_votos = rowSums (select(.,contains('PLD') )),

#Suma todos los votos obtenidos por el PLD y Aliados.

prm_votos = rowSums (select(.,contains('PRM') )),

#Suma de todos los votos obtenidos por el PRM y Aliados

pld_pct = (pld_votos/validos) * 100,
prm_pct = (prm_votos / validos) * 100,

#Calculo de porcentajes de votos


#Colocar el código delante,
select('cod' , everything())


#Con select del dataset Votospresidenciales, creamos un dataset nuevo llamado resultados_filtro, con las siguientes variables:Total de inscriptos, municipios,porcentaje PLD y porcentaje PRM).

resultados_fltro <- select(Votospresidenciales, desc_mun,tot_inscritos,pld_pct,prm_pct) 

#Con la opción tidy agrupe los porcentajes y por partidos

resultados_tidy <- gather(resultados_filtro,partido,porcentaje_de_voto, pld_pct:prm_pct)

#Esta gráfica se realizó tomando como referencia los por porcentajes de votos de los partidos mayoritarios y el total de inscriptos por municipios. El resultado es que PLD tenía obtuvo mayor porcentaje de votos en zonas donde había mayor cantidad de personas hábiles para votar que el PRM. 

ggplot(resultados_tidy, aes(x= resultados_tidy$porcentaje_de_voto,resultados_tidy$tot_inscritos, color = partido)) + geom_point() + facet_wrap(~partido, nrow = 2) 


write_excel_csv(resultados_tidy,'Resultados elecciones 2') #Base de datos para hacer las gráficas
write_excel_csv(Votospresidenciales,'Resultados elecciones 1') #Base de datos con datos completos 

r
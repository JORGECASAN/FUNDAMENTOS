#PREGUNTA1

datos<- read.csv('C:/Users/jcasa/OneDrive/Cunef - MDS/datos.csv', sep=';')
datos

#Realizamos un análisis exploratorio de datos

dim(datos)
str(datos)
summary(datos)


#BONDAD DEL AJUSTE

chisq <- chisq.test(datos)
chisq

#Con nuestros datos observados
chisq$observed

#Con nuestros datos esperados
round(chisq$expected,2)

chisq$p.value

#Para conocer las celdas que más contribuyen al valor de la Chi-cuadrado.
#Devuelve los residuos de Pearson o residuos estandarizado

round(chisq$residuals, 3)

#El % cada celda en el valor de la chi-cuadrado es:
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

#Lo que vamos a realizar es un contraste de hipótesis de bondad del ajuste el cual mide la discrepancia entre una distribución tomada y 
#una teórica. Las hipótesis son las siguientes.
#H:0 <- igualdad de distribuciones
#H:1 <- No igualdad de distribuciones

#la prueba de equiporbabilidad, q
#ue como podemos ver su P-valor es inferior al 0.05 y por tanto existen diferencias significativas, no teniendo suficiente envidencia empírica para
#aceptar H0

#Equiprobabilidad
res1 <- chisq.test(datos, p = c(1/6, 1/6, 1/6,1/6,1/6,1/6))
res1

#Comparando con probabilidades tétricas dadas

res <- chisq.test(datos, p = c(1/3, 1/3, 1/6,0.08333335,0.08333335))

res$parameter


library(ggpubr)

#Contratación de hipótesis para evaluar la normalidad de la población

shapiro.test(datos$x)

datos<- data.frame(datos)

ggplot(datos, x="Datos",ylab = "Frecuencia", xlab = "Número de transacciones",  ggtheme = theme_minimal())
   

    
# Test

res <- t.test(datos$x, mu = 20)
res 

# Devuelve el p-valor
res$p.value

# Devuelve el valor medio
res$estimate

# Intervalo de confianza
res$conf.int

#  CONCLUSION:Las proporciones observadas no son significativamente diferentes 
# de las proporciones esperadas.




















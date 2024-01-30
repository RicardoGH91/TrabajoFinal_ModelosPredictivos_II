Trabajo final: Ciencia de Datos: Modelos Predictivos II
================

## INTRODUCCIóN

La importancia de contar con una cartera de clientes confiables
(clientes no morosos) en una institución de banca multiple es de suma
importancia, ya que esto permite tomar decisiones a la hora de
implementar promociones, tales como:

- Incremento de linea de crédito.
- Ofrecer tarjetas adicionales o de servicio

De igual forma para la captación de nuevos clientes. Es por esto que se
realizó un una investigación para determinar si un cliente es “mal
pagador o no”es mal pagador”. Se aplicararán modelos lineales
generalizados (GLM siglas en ingles) Logit y Probit, debido a que la
variable de interes es categorica dicotómica.

## Modelo de Regresion Logit y Probit para la clasificación de clientes en una institución de banca multiple

Se tomaron en cuenta las siguientes variables para dicho caso de
estudio:

- duration (plazo de la operación)  
- amount(monto de la operación)  
- installment(cuotas pagadas)  
- age(edad)  
- age2(edad al cuadrado)  
- cards(número de tarjetas de crédito)

Para el caso de nuestra variable de interes, “Default” definimos al
valor 1 como indicativo de “mal pagador” y 0 como indicativo de “buen
pagador”

Se cargan los datos y definimos los modelos de la siguiente manera:

``` r
ModLogit = glm(Default ~. ,
         family = binomial(logit),
         data = base2)

ModProbit = glm(Default ~. ,
               family = binomial(link = "probit"),
               data = base2)

Mat1<- mtable(ModLogit,ModProbit,digits = 6, sdigits = 3, summary.stats=FALSE)
Mat1
```

    ## 
    ## Calls:
    ## ModLogit: glm(formula = Default ~ ., family = binomial(logit), data = base2)
    ## ModProbit: glm(formula = Default ~ ., family = binomial(link = "probit"), 
    ##     data = base2)
    ## 
    ## ===========================================
    ##                  ModLogit     ModProbit    
    ## -------------------------------------------
    ##   (Intercept)   0.379896      0.194212     
    ##                (0.740033)    (0.444455)    
    ##   duration      0.026954***   0.016563***  
    ##                (0.007714)    (0.004688)    
    ##   amount        0.000073*     0.000044*    
    ##                (0.000034)    (0.000021)    
    ##   installment   0.216509**    0.127097**   
    ##                (0.072900)    (0.043100)    
    ##   age          -0.120182**   -0.070784**   
    ##                (0.037749)    (0.022523)    
    ##   cards        -0.122818     -0.073331     
    ##                (0.130105)    (0.076735)    
    ##   age2          0.001227**    0.000720**   
    ##                (0.000450)    (0.000268)    
    ## ===========================================
    ##   Significance: *** = p < 0.001;   
    ##                 ** = p < 0.01;   
    ##                 * = p < 0.05

Lo primero que podemos observar es que para todas las variables, con la
unica excepción de “cards”, el valor de probabilidad p \< 0.05, por lo
tanto rechazamos la H0 donde H0:bi = 0, por lo tanto, las variables
sirven para explicar si una persona es mal pagador en algun momento en
el tiempo.

Ademas las variables, duration, amount, installment y age2 son
positivas, por lo tanto, si estas variables se incrementan, la
probabilidad de que la persona sea catalogada como deudor o mal pagador
tambien tiende a incrementarse.

En caso contrario las variables age y cards son negativas, por lo tanto,
un aumento en las variables age y cards conlleva a una disminución en la
probabilidad de que la persona sea clasificada como deudor o mal
pagador.

Estas observaciones subrayan la importancia de considerar el impacto de
cada variable en la evaluación del riesgo crediticio, proporcionando
valiosa información para la toma de decisiones informada.

Calcularemos algunos estadísticos los cuales como tal, no son una prueba
de hipotesis, sin embargo, es un medio para la comparacion de los
modelos:

``` r
Estadist_Logit <- Mat1$ModLogit[["sumstat"]]
Estadist_Probit <- Mat1$ModProbit[["sumstat"]]

MatStats <- data.frame(Estadist_Logit, Estadist_Probit)

kable(MatStats)
```

|                | Estadist_Logit | Estadist_Probit |
|:---------------|---------------:|----------------:|
| phi            |      1.0000000 |       1.0000000 |
| LR             |     69.5944949 |      69.8349741 |
| df             |      6.0000000 |       6.0000000 |
| p              |      0.0000000 |       0.0000000 |
| logLik         |   -576.0670546 |    -575.9468150 |
| deviance       |   1152.1341092 |    1151.8936300 |
| Aldrich.Nelson |      0.0650662 |       0.0652764 |
| McFadden       |      0.0569640 |       0.0571608 |
| Cox.Snell      |      0.0672280 |       0.0674523 |
| Nagelkerke     |      0.0953211 |       0.0956391 |
| AIC            |   1166.1341092 |    1165.8936300 |
| BIC            |   1200.4883962 |    1200.2479169 |
| N              |   1000.0000000 |    1000.0000000 |

Para este caso nos centraremos en los criterios BIC y AIC, criterios de
informacion AKAIKE y SCHWARZ, el mejor modelo seria el que tiene menor
valor.  
El resultado sugiere que el mejor modelo es el modelo probit.

Finalmente calculamos el efecto marginal de los modelos

``` r
margins_Logit <- margins(ModLogit)
margins_probit <- margins(ModProbit)

Marginal_models <- left_join(summary(margins_Logit),summary(margins_probit),
                        by="factor")

names(Marginal_models)[2] <- "MarginLogit"
names(Marginal_models)[8] <- "MarginProbit"

Marginal_models <- dplyr::select(Marginal_models, factor, MarginLogit, MarginProbit)
kable(Marginal_models)
```

| factor      | MarginLogit | MarginProbit |
|:------------|------------:|-------------:|
| age         |  -0.0234650 |   -0.0231105 |
| age2        |   0.0002395 |    0.0002351 |
| amount      |   0.0000143 |    0.0000143 |
| cards       |  -0.0239797 |   -0.0239422 |
| duration    |   0.0052627 |    0.0054079 |
| installment |   0.0422725 |    0.0414967 |

Podemos observar que el efecto marginal de los coeficientes para cada
uno de los modelos es muy similar, para explicar la interpretación del
efecto marginal tomaremos como ejemplo los coeficientes de las variables
“plazo de la operación”(duration) y “Edad”(age):

- Un aumento en 1 unidad en el plazo de la operación se asocia a un
  aumento esperado de 0.005 mas probable que un individuo sea catalogado
  como deudor.
- Un aumento en 1 unidad en la Edad del individuo se asocia a una
  disminucion esperada de 0.0235 probable que un individuo sea
  catalogado como deudor.

## Bondad de ajuste.

Generalmente para estos modelos no se tiene criterios como la R^2 que se
utiliza en los MRLM, estos modelos lo que se busca es que se clasifique
correctamente. Para esto utilizaremos el Criterio de Hosmer-lemeshow, el
metodo funciona muy similar a una tabla de contingencia, la cual se
evalua a traves de un estadistico chi-cuadrada, evalua la prob. de
clasificacion de un grupo u otro, de tal manera que devuelva un valor p
utilizando dicho contraste. Las hipotesis a tomar en cuenta es la
siguiente:

H0: Bondad de ajuste  
H1: No se consigue bondad de ajuste

``` r
hl_logit <- hoslem.test(base2$Default,
                   fitted(ModLogit), g=10 )

hl_Probit <- hoslem.test(base2$Default,
                        fitted(ModProbit), g=10 )

Estdistico <- c(hl_logit$statistic, hl_Probit$statistic)
p_value <- c(hl_logit$p.value, hl_Probit$p.value)
Nombres <- c("logit","Probit")
estadisticos <- data.frame(row.names = Nombres, Estdistico, p_value)

kable(estadisticos)
```

|        | Estdistico |   p_value |
|:-------|-----------:|----------:|
| logit  |   7.656898 | 0.4676821 |
| Probit |   8.064415 | 0.4272033 |

Para ambos casos el valor p \> 0.05, no podemos rezachar la h0, existe
evidencia estadistica para decir que se tiene bondad de ajuste.

## Matriz de clasificación

Para continuar con el analisis, generaremos la matriz de clasificación,
la cual es una herramienta que se utiliza para evaluar el rendimiento de
un modelo de clasificación, su propósito principal es proporcionar una
visión general de cuantas predicciones hizo el modelo correcta e
incorrectamente en comparación con la verdad conocida y de acuerdo al
umbral de probabilidad que se haya definido, donde el umbral de
probabilidad es un punto de corte que permite decidir cuando clasificar
una observación como perteneciente a la clase positiva y cuando
clasificarla como perteneciente a la clase negativa.

Para este caso tomaremos como umbral el promedio de valores proyectados.

``` r
threshold_logit <- mean(fitted(ModLogit))

ClassLog(ModLogit, base2$Default, cut = threshold_logit)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 444 132
    ##   TRUE  256 168
    ## 
    ## $classtab
    ##        resp
    ##                 0         1
    ##   FALSE 0.6342857 0.4400000
    ##   TRUE  0.3657143 0.5600000
    ## 
    ## $overall
    ## [1] 0.612
    ## 
    ## $mcFadden
    ## [1] 0.05696396

``` r
threshold_Probit <- mean(fitted(ModProbit))

ClassLog(ModProbit, base2$Default, cut = threshold_Probit)
```

    ## $rawtab
    ##        resp
    ##           0   1
    ##   FALSE 439 130
    ##   TRUE  261 170
    ## 
    ## $classtab
    ##        resp
    ##                 0         1
    ##   FALSE 0.6271429 0.4333333
    ##   TRUE  0.3728571 0.5666667
    ## 
    ## $overall
    ## [1] 0.609
    ## 
    ## $mcFadden
    ## [1] 0.05716079

Antes de interpretar los resultados debemos definir los siguientes
conceptos de la matriz de clasificacion:

Verdaderos Positivos (TP): Representa la cantidad de casos en los que el
modelo predijo correctamente los valores uno. Verdaderos Negativos (TN):
Representa la cantidad de casos en los que el modelo predijo
correctamente los valores cero. Falsos Positivos (FP): Representa la
cantidad de casos en los que el modelo predijo incorrectamente los
valores uno. Falsos Negativos (FN): Representa la cantidad de casos en
los que el modelo predijo incorrectamente los valores cero.

Los resultados arrojados son los siguientes:

|                      |    CT_logit |   CT_Probit | Interpretacion                                                            |
|:---------------------|------------:|------------:|:--------------------------------------------------------------------------|
| TN                   | 444.0000000 | 439.0000000 | Cantidad de casos que el modelo predijo correctamente los valores uno.    |
| FN                   | 132.0000000 | 130.0000000 | Cantidad de casos que el modelo predijo incorrectamente los valores cero. |
| FP                   | 256.0000000 | 261.0000000 | Cantidad de casos que el modelo predijo incorrectamente los valores uno.  |
| TP                   | 168.0000000 | 170.0000000 | Cantidad de casos que el modelo predijo correctamente los valores uno.    |
| % TN                 |   0.6342857 |   0.6271429 | Porcentaje predichas por el modelo correctamente como cero.               |
| % FN                 |   0.4400000 |   0.4333333 | Porcentaje predichas por el modelo incorrectamente como cero.             |
| % FP                 |   0.3657143 |   0.3728571 | Porcentaje predichas por el modelo incorrectamente como uno.              |
| % TP                 |   0.5600000 |   0.5666667 | Porcentaje predichas por el modelo correctamente como uno.                |
| Clasificacion Global |   0.6120000 |   0.6090000 | Porcentaje de clasificacion correctamente para el modelo                  |

Como podemos ver ambos modelos clasifican de manera muy similar y el %
de clasificacion global = 61.2% VS 60.90%, se puede interpretar como el
61.2 % de los datos estan siendo clasificados correctamente para el
modelo Logit vs El 60.9 % de los datos estan siendo clasificados
correctamente para el modelo Probit.

A partir de estas clasificaciones, definimos las siguientes formulas:  
- Precision = tp/(tp + fp)  
- Recall = tp/(tp + fn) =\> tambien se le conoce como sensitividad
(recuperacion)  
- Especificidad = tn/(tn + fp)

Las 3 formulas definidas anteriormente esperamos que tiendan a 1, aqui
es cuando se genera la disyuntiva, donde a veces vamos a preferir
precision por encima de la sensitividad y la especificidad.

Recall hace referencia al % de clasificacion correcta de 1 cuando son
1.  
Especificidad hace referencia al % de clasificacion correcta de los
ceros cuando son ceros.  
Precision hace referencia al % de clasificacion 1 cuando son 1
considerando que se puede equivocar por los falsos positivos.

## Curva ROC

La curva denota que tan bien esta clasificandose el modelo (o bien
discriminado), muestra cómo cambia la tasa de verdaderos positivos
(sensibilidad) en función de la tasa de falsos positivos (1 -
especificidad) para diferentes valores de umbral de probabilidad.

- Si la curva esta mas pegado al eje, o esta lo mas alejado a la linea
  de 45°, se esta discriminando correctamente.  
- Si un modelo tiene una curva muy pegada a la tangente, el modelo no
  sirve.

El area bajo la curva esta comprendida entre 0.5 y 1, mientras mas
cercano esta a 1, mejor sera el modelo, hay una buena sensitividad y
buena especificidad.

``` r
predLogit <- ROCR::prediction(ModLogit$fitted.values,base2$Default) # valores predichos
perfLogit <- performance(predLogit,
                    measure = "tpr", 
                    x.measure = "fpr")

predProbit <- ROCR::prediction(ModProbit$fitted.values,base2$Default) # valores predichos
perfProbit <- performance(predProbit,
                         measure = "tpr", 
                         x.measure = "fpr")

par(mfrow=c(1,2))
plot(perfLogit,colorize = T, lty = 3,
     main = "Curva ROC Modelo Logit")
abline(0,1,col="black")

plot(perfProbit,colorize = T, lty = 3,
     main = "Curva ROC Modelo Probit")
abline(0,1,col="black")
```

![](TrabajoFinalModelosPredictivosII_files/figure-gfm/Curva_ROC-1.png)<!-- -->

``` r
# Area bajo la curva Logit
aucl <- performance(predLogit, measure = "auc")
aucl <- aucl@y.values[[1]]
aucl
```

    ## [1] 0.6558952

``` r
# Area bajo la curva Probit
aucp <- performance(predProbit, measure = "auc")
aucp <- aucp@y.values[[1]]
aucp
```

    ## [1] 0.6557667

Como se comentó lineas arriba se espera que el modelo tenga una curva
que este significativamente por encima de esta linea para considerarse
util.

Los valores del area bajo la curva es de:  
- 0.6558 para el modelo logit.  
- 0.6557 para el modelo probit.

Ambas areas son muy similares.

El objetivo final es maximizar la cantidad de casos en los que el modelo
predice correctamente los valores uno(TP) y la cantidad de casos en los
que el modelo predice correctamente los valores cero(TN), para esto se
debe encontrar el punto de corte optimo.

Para encontrar este valor calculamos:  
% de clasificacion correcta de 1 cuando son 1 -\> sensitividad  
% de clasificacion correcta de los ceros cuando son ceros. -\>
Especificidad

Graficamos dichos valores y el punto de corte optimo es donde la
sensitividad y la especificidad se intersectan.

![](TrabajoFinalModelosPredictivosII_files/figure-gfm/Corte_OptimoLog-1.png)<!-- -->

![](TrabajoFinalModelosPredictivosII_files/figure-gfm/Corte_OptimoProb-1.png)<!-- -->

Ahora que hemos encontrado el valor que maximiza la especificidad y
sensitividad calcularemos la matriz de clasificacion con este nuevo
umbral:

|                      |    CT_logit |   CT_Probit | CT_logit_Optimo | CT_Probit_optimo | Interpretacion                                                            |
|:---------------------|------------:|------------:|----------------:|-----------------:|:--------------------------------------------------------------------------|
| TN                   | 444.0000000 | 439.0000000 |     414.0000000 |      416.0000000 | Cantidad de casos que el modelo predijo correctamente los valores uno.    |
| FN                   | 132.0000000 | 130.0000000 |     123.0000000 |      123.0000000 | Cantidad de casos que el modelo predijo incorrectamente los valores cero. |
| FP                   | 256.0000000 | 261.0000000 |     286.0000000 |      284.0000000 | Cantidad de casos que el modelo predijo incorrectamente los valores uno.  |
| TP                   | 168.0000000 | 170.0000000 |     177.0000000 |      177.0000000 | Cantidad de casos que el modelo predijo correctamente los valores uno.    |
| % TN                 |   0.6342857 |   0.6271429 |       0.5914286 |        0.5942857 | Porcentaje predichas por el modelo correctamente como cero.               |
| % FN                 |   0.4400000 |   0.4333333 |       0.4100000 |        0.4100000 | Porcentaje predichas por el modelo incorrectamente como cero.             |
| % FP                 |   0.3657143 |   0.3728571 |       0.4085714 |        0.4057143 | Porcentaje predichas por el modelo incorrectamente como uno.              |
| % TP                 |   0.5600000 |   0.5666667 |       0.5900000 |        0.5900000 | Porcentaje predichas por el modelo correctamente como uno.                |
| Clasificacion Global |   0.6120000 |   0.6090000 |       0.5910000 |        0.5930000 | Porcentaje de clasificacion correctamente para el modelo                  |

Podemos ver que con este nuevo umbral el porcentaje de clasificación
para el modelo Logit disminuyo 4.28 % en la predicción correcta de los
valores cero pero incremento 3% en la predicción de valores uno, en
otras palabras, el modelo logit tiene un % de clasificación de 59% de
probabilidad de predecir a una persona deudora, y 59.14% de probabilidad
de predecir a una persona que no debe.

Para el modelo Probit, disminuyo 3.28% en la predicción correcta de los
valores cero pero incremento 2.33% en la predicción de valores uno, en
otras palabras, el modelo logit tiene un % de clasificación de 59% de
probabilidad de predecir a una persona deudora, y 59.42% de probabilidad
de predecir a una persona que no debe.

Para ambos modelos disminuyo el porcentaje de clasificación global a
59%.

Ahora bien con estos resultados aplicaremos los modelos a 3 clientes:

| Clientes  | duration | amount | installment | age | cards | age2 | Prob_Logit | Prob_Probit |
|:----------|---------:|-------:|------------:|----:|------:|-----:|-----------:|------------:|
| Cliente 1 |        4 |    250 |           1 |  19 |     1 |  361 |  0.2243007 |   0.2236966 |
| Cliente 2 |       18 |   3271 |           3 |  33 |     2 | 1089 |  0.2458863 |   0.2478331 |
| Cliente 3 |       72 |  18424 |           4 |  75 |     4 | 5625 |  0.8739511 |   0.8751676 |

Podemos observar que para los clientes con los valores presentados en la
tabla, se obtiene lo siguiente:

la probabilidad que el cliente 1 sea catalogada como “mal pagador” es de
0.22 con el modelo logit y Probit.  
la probabilidad que el cliente 2 sea catalogada como “mal pagador” es de
0.24 con el modelo logit y Probit.  
la probabilidad que el cliente 3 sea catalogada como “mal pagador” es de
0.87 con el modelo logit y Probit.

## Conclusiones

Revisando los modelos anteriores, podemos decir que: - Ambos modelos se
ajustan(Bondad de ajuste).  
- Ambos modelos clasifican de manera muy similar y el % de clasificacion
global = 59.1 % VS 59.3 %. - En las predicciones, ambos modelos arrojan
probabilidades similares.

Sin embargo, si nos enfocamos en los criterios BIC y AIC, criterios de
informacion AKAIKE y SCHWARZ, el mejor modelo seria el que tiene menor
valor.

Por lo anterior el resultado sugiere que el mejor modelo es el modelo
PROBIT:

|     | CT_logit | CT_Probit |
|:----|---------:|----------:|
| AIC | 1166.134 |  1165.894 |
| BIC | 1200.488 |  1200.248 |

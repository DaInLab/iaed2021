# Introdução à Machine Learning - Regressão

# Dados registrados do vendedor de sorvetes
temperatura = c(30, 25, 36, 18, 25, 29, 30, 33, 37, 31, 26, 37, 29, 26, 30, 31, 34, 38)
numero_sorvetes = c(20, 12, 50, 10, 18, 25, 26, 32, 48, 22, 16, 52, 24, 20, 28, 29, 35, 40)
# transformando as arrays em dataframe
df <- data.frame(temperatura, numero_sorvetes)
head(df)
#  temperatura numero_sorvetes
#1          30              20
#2          25              12
#3          36              50
#4          18              10
#5          25              18
#6          29              25

# Gerando gráfico
plot(df$temperatura, df$numero_sorvetes, 
     xlab = "Temperatura",
     ylab = "Sorvetes",
     frame.plot = T)

# Treinando o modelo
modelo = lm(numero_sorvetes ~ temperatura, data = df)

# Plotando o gráfico
plot(df, pch = 16, col = "blue") # Plotando os dados do dataframe
abline(modelo) # Adicionando no gráfico a linha da regressão do modelo 

# Resultados do modelo
summary(modelo)
#Call:
#lm(formula = numero_sorvetes ~ temperatura, data = df)
#
#Residuals:
#   Min     1Q Median     3Q    Max 
#-7.771 -2.552 -1.050  1.502  9.123 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -39.0896     7.7277  -5.058 0.000116 ***
#temperatura   2.2213     0.2518   8.822 1.53e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 5.351 on 16 degrees of freedom
#Multiple R-squared:  0.8295,	Adjusted R-squared:  0.8188 
#F-statistic: 77.82 on 1 and 16 DF,  p-value: 1.526e-07

# Ao analisar o resumo do modelo, observa-se que apesenta resultados muito bons
# (veja o R² (0.8295) e o R² ajustado ( 0.8188))

# Plotando os valores residuais
# Idealmente, quando você plota os resíduos, eles devem parecer aleatórios. 
# Caso contrário, significa que talvez haja um padrão oculto que o modelo linear não está considerando. 
# Para plotar os resíduos, use o comando plot(modelo$residuals) ou o comando plot(resid(modelo)).

plot(modelo$residuals, pch = 16, col = "red",
     ylab = "Residuais")
# ou
plot(resid(modelo), pch = 16, col = "red",
     ylab = "Residuais")

plot(fitted(modelo), pch = 16, col = "yellow",
     ylab = "Fitted")

# Como visto no gráfico se o dataframe tiver mais dados, este modelo linear simples poderá generalizar bem. 
# Na figura anterior, observe que não há efetivamente um padrão nos resíduos, o que é bom.

coef(modelo)
# Fórmula da regressâo: Yhat = -39.089556 + 2.221306x

x = (fitted(modelo)[[1]] + 39.089556) /  2.221306
x
#[1] 30.00001 graus de temperatura, venderá 27.54964 sorvetes


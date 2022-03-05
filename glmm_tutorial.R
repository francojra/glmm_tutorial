# GLMM no R: Modelo Linear Generalizado Misto ----------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 04/03/2022 -------------------------------------------------------------------------------------------------------------------------
# Fonte: Canal do YouTube Erre-Aprenda -----------------------------------------------------------------------------------------------------

# Conceito glmm ----------------------------------------------------------------------------------------------------------------------------

## Na análise glmm nós temos efeitos fixos e aleatórios. Efeito fixo se entende como as variáveis preditoras,
## e o efeito aleatório é a variável que agrupa nossos dados e seu efeito na variável resposta não interessa
## diretamente, mas temos que contabilizar.

## No exemplo temos a variável resposta riqueza de espécies herbáceas e precisamos avaliar o efeito
## de caracteres funcionais sobre a riqueza de herbáceas, juntamente com o efeito aleatório da parcela. 
## Portanto, vamos considerar localização como um efeito aleatório.

## Como riqueza é uma variável de contagem, vamos utilizar a família Poisson no modelo.

# Baixar pacotes ---------------------------------------------------------------------------------------------------------------------------

library(lme4)
library(lattice) # Pacote para funções auxiliares

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

dados <- read.table("http://labtrop.ib.usp.br/lib/exe/fetch.php?media=cursos:planeco:roteiro:praia.txt", h = T)
View(dados)

# Análise ----------------------------------------------------------------------------------------------------------------------------------

modelo <- glmer(Richness ~ NAP + (1|Beach), family = 'poisson', data = dados)
summary(modelo)

## Interpretação: em random effects podemos verificar os efeitos aleatórios ou o quanto da
## variabilidade da variável resposta é devido aos efeitos aleatórios.
## Abaixo encontramos os efeitos fixos do NAP com a estimativa, erro, valor z e valor de p.
## Encontramos que ocorre a reduação de 0.5 espécies com uma unidade a mais do NAP.

# Análises dos efeitos ---------------------------------------------------------------------------------------------------------------------

(fixo <- fixef(modelo))  # Efeitos fixos
(aleatorio = coef(modelo)$Beach)  # Efeitos aleatórios 

## Os coeficientes apresentam no primeiro caso o intercepto e a inclinação sem os efeitos
## aleatórios, e no segundo caso os interceptos e inclinações com os efeitos aleatórios em
## cada praia.

# Verificando amplitude dos valores de NAP -------------------------------------------------------------------------------------------------

range(dados$NAP)
seqNAP <- seq(-1.336, 2.255, len = 100) # Cria uma sequência de 100 valores de NAP dentro da amplitude.
seqNAP

# Calcular o modelo de cada uma das 9 praias -----------------------------------------------------------------------------------------------

# Estimativas para cada praia

praia01 <- aleatorio[1,1] + aleatorio[1,2] * seqNAP
praia02 <- aleatorio[2,1] + aleatorio[2,2] * seqNAP
praia03 <- aleatorio[3,1] + aleatorio[3,2] * seqNAP
praia04 <- aleatorio[4,1] + aleatorio[4,2] * seqNAP
praia05 <- aleatorio[5,1] + aleatorio[5,2] * seqNAP
praia06 <- aleatorio[6,1] + aleatorio[6,2] * seqNAP
praia07 <- aleatorio[7,1] + aleatorio[7,2] * seqNAP
praia08 <- aleatorio[8,1] + aleatorio[8,2] * seqNAP
praia09 <- aleatorio[9,1] + aleatorio[9,2] * seqNAP
praiaFixo <- fixo[1] + fixo[2] * seqNAP

# Estimativas por ponto - Devemos exponenciar, pois os valores antetiores estão em log

praia01a <- exp(praia01)
praia02a <- exp(praia02)
praia03a <- exp(praia03)
praia04a <- exp(praia04)
praia05a <- exp(praia05) 
praia06a <- exp(praia06)
praia07a <- exp(praia07) 
praia08a <- exp(praia08) 
praia09a <- exp(praia09)
praiaFixa <- exp(praiaFixo)

# Gráfico ----------------------------------------------------------------------------------------------------------------------------------

cores <- rainbow(9) # Selecionar 9 cores, uma para cada praia

par(mar = c(5,5,2,2))
plot(Richness ~ NAP, data = dados, pch = 19, 
     col = cores[as.factor(Beach)], las = 1, cex = 1.5, 
     cex.lab = 1.5, cex.axis = 1.5, xlim = c(-1.5, 2.5))

lines(praia01a ~ seqNAP, col = cores[1])
lines(praia02a ~ seqNAP, col = cores[2])
lines(praia03a ~ seqNAP, col = cores[3])
lines(praia04a ~ seqNAP, col = cores[4])
lines(praia05a ~ seqNAP, col = cores[5])
lines(praia06a ~ seqNAP, col = cores[6])
lines(praia07a ~ seqNAP, col = cores[7])
lines(praia08a ~ seqNAP, col = cores[8])
lines(praia09a ~ seqNAP, col = cores[9])
lines(praiaFixa ~ seqNAP, col = rgb(0,0,0,0.5), lwd = 5)

legend("topright", c("Praia média", paste("Praia", 1:9)), lty = 1, col = c(1,cores), 
       bty = "n", cex = 1.5)

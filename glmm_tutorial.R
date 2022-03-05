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


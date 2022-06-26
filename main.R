#A biblioteca utilizada para geração dos gráficos dplyr 

#importanto a base de dados em xls  
library(readxl)
INFLUD17 <- read_excel("INFLUD17.xlsx")


#soma de pessoas do sexo masculino: 15061
qnt_Sexo_M <- sum(INFLUD17$CS_SEXO == "M" )

qnt <- INFLUD17$CS_SEXO

View((qnt))

#soma de pessoas do sexo feminino: 14482
qnt_Sexo_F <- (INFLUD17$CS_SEXO == "F")
View(qnt_Sexo_F)

qnt_f <- INFLUD17$CS_SEXO == "F" 

data <- data.frame(qnt)
#Visualiza todos os dados inferidos consultados
View(data)
#Exibe Histograma da tabela
hist(data)


# Casos de Sindrome Respiratoria por Estado e Total
casosRegistrados <- sum(INFLUD17$SG_UF_NOT)
casosRegistradosEmSp <- sum(INFLUD17$SG_UF_NOT == 35)
casosEmTocatins <- sum(INFLUD17$SG_UF_NOT == 17)

# Histograma de todos os casos registrados
hist(casosRegistrados)

# Exibe todas os calculos de Minima, Media, mediana e máxima de todas as colunas
summary(INFLUD17)


# Calcular a Media de idades
mean(INFLUD17$NU_IDADE_N)


#Gráfico frequência por estado
#library(readxl)
#install.packages("tidyverse")
#install.packages("ggplot2")
#library(tidyverse)
# ggplot
#library(ggplot2)
#library(dplyr)

#exibe tabela com a distribuição de casos por estado
INFLUD17 %>% group_by(SG_UF_NOT_NEW) %>% summarise(Contagem = n()) %>%
  #exibe histograma baseado na tabela da distribuição de casos por estado
  ggplot(aes(x = SG_UF_NOT_NEW, y = Contagem), xlab("Estados")) + geom_bar(stat = "identity")







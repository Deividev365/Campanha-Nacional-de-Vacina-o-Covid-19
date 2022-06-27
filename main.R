#A biblioteca utilizada para geração dos gráficos dplyr 

#importanto a base de dados em xls  
library(readxl)
INFLUD17 <- read_excel("INFLUD17.xlsx")


#soma de pessoas do sexo masculino: 15061
qnt_Sexo_M <- sum(INFLUD17$CS_SEXO == "M" )

## Tabela de Frequência sexo masculino e feminino
tbDeFrequencia <- as.data.frame(table(INFLUD17$CS_SEXO))

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

## Visualização distribuição usando a função do ggplot dos estados obtidos na amostra
ggplot(INFLUD17, aes(SG_UF_NOT_NEW)) +
  geom_bar()


## Frequência Obtida na coluna de gestação
tbDeFrequenciaGestante <- as.data.frame(table(INFLUD17$CS_GESTANT))

proportions(table(INFLUD17$SEM_NOT))



## Teste de Hipótese
## Testar Hipótese de pessoas com idade entre 0 e 25 e 26 e 50 anos??
sindromeDown <- sum(INFLUD17$NU_IDADE_N)

## 0 a 25
## Pacote para Realizar a Filtragem de atributos da TabelaconverNumbersconverNumbers
require(tidyverse)
install.packages("tidyverse")

view(INFLUD17)

# Filtra idade dos pacientes, se o paciente tomou a vacina e tendência de sintomas Desconforto
# Respiratório
INFLUD17 %>%
    select(NU_IDADE_N, VACINA, DESC_RESP)

idadesPacientes <- INFLUD17$NU_IDADE_N

convertIdadePacientes <- round(idadesPacientes, -2)

view(idadesPacientes)

view(convertIdadePacientes)

## Media de Idade dos Pacientes com Casos ##
mediaIdadesPacientes <- mean(convertIdadePacientes)


## Escolaridade dos Pacientes ##

fundamental <- (INFLUD17$CS_ESCOL_N == 1)

if(fundamental == TRUE) {
  sum(fundamental)
}

view(fundamental)


## tomou vacina

zonaResid <- (INFLUD17$VACINA == 1)


view(converNumbers)














x<-                           #como lançar uma variável
rep(10,5)                      #Repete o 10 cinco vezes
rm(list = "x", "y")           #Remove variáveis
setwd("c:/econometria")        #Para setar a pasta de econometria como local para salver
getwd()                        #Para verificar qual pasta setamos
rm(list=ls())                # para limpar, depois dar enter e clicar em CTRL+L
install.packages("pwt8.0")     #Instalar o pacote PWT8.0
library(pwt8)                 #carrega o pacote
data("pwt8.0")                #carrega os dados elencados
View(pwt8.0)                  #visualiza os dados
                              # Quando quero digitar um comando que busque valor igual, precisa usar dois sinais de igual ==
subset( )                    #Extraindo dados da tabela. Estrutura: ( x , linhas, colunas  )
subset( x , linhas, colunas  ) #Linha acima. Subset significa subgrupo
br<- subset(pwt8.0, country== "Brazil", select = c(rgdpna,avh,xr))   #Planilha
View(br)                         # br foi a variável. Isso fará com que mostre apenas as colunas selecionadas no programa
colnames(br)<- c("PIB", "Trabalho", "Cambio")                    #Mudar o nome das colunas 
plot(Variavel$Variavel da variavel)          #Para criar gráfico, usar comando em questão. Exemplo plot(br$PIB) ele mostrará só o pib.
dados <- ts (br, start=1950, freq=1)   #mudar eixo x para datas usar comando: ts ( data set , data de inicio, periodicidade dos dados  ) e fazer o plot da nova variável criada
plot(dados, col= "blue", main= "Dados Brasileiros", xlab="Ano", plot.type = "single")    #Para mudar de cor e dar título: plot(dados, col="purple", main="Dados Brasileiros", xlab="Ano")
library(readr)
br <- read.csv("c:/Econometria B/br.csv")
View(br)                       #Três últimas linhas: setar item, definir variável e visualizar (br)
br <- br[,-1]                 #para apagar coluna colocar variavel <- variavel [, - numero da coluna]. Se eu quisesse apagar linha era só digitar algo antes da vírgula.
colnames(br)[3] <- "CAMBIO"    #comando para alterar nome
PIB <- ts(br$PIB, start = 1950, frequency = 1) / plot(PIB)      #setar variavel do grafico e plotar (plotar e fazer gráfico)



install.packages("urca")                   #pacote da aula 4
library("urca")  
library(readxl)


interdaay <- read_excel("c:/econometria B/interdaay.xls", col_types = c("date", "numeric", "numeric", "numeric"))              #Pegar o arquivo em excel



dados_diarios <- ts(interdaay, start=2017-01-10, frequency = 365)
plot(dados_diarios, col="purple", main="Dados do Indice Bovespa", xlab="Dias")     #criar planilha e plotar


Variação <- ts(interdaay$Variação, start=2017-01-10, frequency = 365)
Ibovespa <- ts(interdaay$Ibovespa, start=2017-01-10, frequency = 365)
Quantidade <- ts(interdaay$Quantidade, start=2017-01-10, frequency = 365)



plot(Variação, main="Percentual de Variação")                     #plotagem
plot(Ibovespa, main="Início do Dia", col="red")
plot(Quantidade, main="Índice do Dia", xlab="Dias", col="Blue")


TesteDF_Variação_none <- ur.df(Variação, "none", lags = 0)           #Teste Dick Fuller. Para descobrir se é estacionário ou não estacionário
summary(TesteDF_Variação_none)

Value of test-statistic is: -41.5838  #está na região de rejeição. Então é Estacionária


1pct  5pct 10pct      #Valores em 1% 5% e 10% . 
-2.58 -1.95 -1.62



TesteDF_Variação_drift <- ur.df(Variação, "drift", lags = 0)            #Teste de Drift
summary(TesteDF_Variação_drift)                                     #Deu valor positivo 864.1266 . Não tem drift, é estacionária


TesteDF_Variação_trend <- ur.df(Variação, "trend", lags = 0)                 #Teste de Trend
summary(TesteDF_Variação_trend)


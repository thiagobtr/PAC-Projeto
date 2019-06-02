install.packages(c('ggplot2','lubridate','dplyr'))
install.packages('data.table')

library('ggplot2')
library('dplyr')
library('lubridate')
library('stringr')
install.packages('lubridate')
install.packages('data.table')
install.packages('tidyr')
install.packages('readr')
install.packages('stringr')
print('teste')

setwd('/media/sf_FormacaoCientistaDeDados/Portfolio/AnaliseBrasil')

#########################Lendo arquivo

#Pac 
pac <- data.frame(read.csv('Data/Pac_2018_12.csv',header=TRUE,sep=',',fileEncoding='latin1'
                           , stringsAsFactors =FALSE))
#arquivo Pac 2
pac_2014 <- data.frame(read.csv('Data/PAC_2014_10.csv',header=TRUE,sep=';',fileEncoding='latin1'
                           , stringsAsFactors =FALSE))

#Siglas e estados
sigla_uf<- data.frame(read.csv('Data/Estados_Siglas.txt',header=TRUE,sep = '\t',
                               stringsAsFactors = FALSE))

#Dados de saneamento
saneam <- data.frame(read.csv('Data/IndicadoresSaneamentoGeral.csv',header=TRUE,sep=';'
                              ,fileEncoding = 'latin1',stringsAsFactors = FALSE))
?read.csv
View(pac)
View(saneam)

colnames(pac)
colnames(pac_2014)
#Variavel investimento_total recebendo valor mais recente 
pac_2014$investimento_total<-ifelse(is.na(pac_2014$investimento_total),
  ifelse(is.na(pac_2014$val_pos_2014),pac_2014$val_2011_2014,
                                    pac_2014$val_pos_2014),pac_2014$investimento_total)
  
#exclui variaveis
pac_2014$val_pos_2014=NULL
pac_2014$val_2011_2014=NULL

col_names<- colnames(pac)

names(pac_2014)<- col_names
#Uniao dos dataframes
pac_total<- rbind(pac,pac_2014)

#Exclui df pac_2014
remove(pac_2014)
##########
#Conversao formato data
pac_total$data_ciclo = as.Date(pac_total$dat_ciclo,"%d/%m/%Y") 
pac_total$data_conclusao = as.Date(pac_total$dat_conclusao_revisada,"%d/%m/%Y") 
pac_total$data_selecao = as.Date(pac_total$dat_selecao,"%d/%m/%Y") 

# Exclusao das variaveis originais
pac_total$dat_ciclo = NULL
pac_total$dat_conclusao_revisada = NULL 
pac_total$dat_selecao = NULL


#Visualizando as primeiras linhas do datase
glimpse(pac_total)
head(pac_total)
#extrai pontuação
# grep retorna numero da observacao do padrao encontrado
head(grep("[[:punct:]]",pac_total$obra_latitude))
?grep

#Usando stringr
#\\D = busca carac diferentes de numeros
head(str_extract_all(pac_total$obra_latitude,"\\D"))
# \\d somente numeros
head(str_extract_all(pac_total$obra_latitude,"\\d"))

# Replace nao digito por ""
pac_total$lat<-str_replace_all(pac_total$obra_latitude,"\\D","")
pac_total$lon<-str_replace_all(pac_total$obra_longitude,"\\D","")

# outra forma para remover colunas
pac_total<- select(pac_total,-c(obra_latitude,obra_longitude))

?str_extract

#Criaçao de Dataframe para o estagio da Obra
estagio_obras = tibble (
  codigo = c(0,3,5,10,40,41,42,70,71,90,91),
  descricao= c('Nao informado', 'A selecionar','Em contratação','Ação Preparatória',
               'Em licitação de obra','Em licitação de projeto','Em execução de projeto',
               'Em obras','Em execução','Concluído','Em operação'))

# Criacao DataFrame para Tipo e sueixo do empreendimento
tipos_empr = tibble(
  tipo <- c('Rodovias','Ferrovia','Porto', 'Hidrovia','Aeroporto'
            ,'Defesa','Comunicaçoes','Ciencia e Tecnologia'
            ,'Geração de Energia Elétrica','Transmissão de Energia Elétrica','Petróleo e Gás Natural'
            ,'Geologia e Mineração - Cprm','Marinha Mercante','Combustíveis Renováveis','Saneamento'
            ,'Água em áreas urbanas','Prevenção em áreas de risco','Pavimentação','Mobilidade Urbana'
            ,'Cidades Digitais','Cidades Históricas','Infraestrutura Turística','Olimpíadas'
            ,'Educação','Saúde','SUFRAMA','Casa da Mulher Brasileira','UBS','UPA','Creches e Pré Escolas'
            ,'Quadras Esportivas nas Escolas','Centro de Artes e Esportes Unificados'
            ,'Centro de Iniciação ao Esporte','MCMV','Urbanização de assentamentos precários'
            ,'Financiamento SBPE','Luz para Todos','Recursos Hídricos','Água em áreas urbanas','Estradas Vicinais'),
  subeixo <-c(rep('Infraestrutura Logística',8),rep('Infraestrutura Energética',6),
              rep('Infraestrutura Social e Urbana',24),'Infraestrutura Social e Urbana','Infraestrutura Logística'),
  idn_digs <- c(1000,1001,1002,1003,1004,1006,1007,1008,2000,2001,2002,2003,2004,2005,3000,3000,
                3001,3002,3003,3004,3005,3006,3007,3011,3012,3013,3009,4000,4001,4002,4003,4004,
                4005,5000,5001,5002,6000,6002,6001,1005),.name_repair= ~ c('tipo','subeixo','idn_digs')
)

tipos_empr
#https://dplyr.tidyverse.org/reference/join.html

#join 
# Criando novo dataframe com as informacaçoes do df pac_total e a variavel "descricao" 
#do df estagio_obras 
pac_total<-left_join(pac_total,estagio_obras,by = c("idn_estagio"= "codigo"))
# Criando novo dataframe com as informacaçoes do df pac_total2 e as variaveis "tipo_eixo" e "subeixo"
#do df tipos_empr
pac_total<- left_join(pac_total,tipos_empr,by = c("id_digs"="idn_digs"))

#Criacao de coluna para Qtd de UF
pac_total<- pac_total%>%
  mutate(Qtd_uf= str_count(sig_uf,"[A-Z]")/2)%>%
  select(idn_empreendimento:subeixo,Qtd_uf)

#filtra o df buscando Obras para somente 1 estado e em seguida faz um join 
#adicionando seu nome
pac_total <- pac_total%>%filter(Qtd_uf==1)%>%
  inner_join(sigla_uf,by=c("sig_uf"="SIGLA"))

#Cria variavel Regiao
pac_total<- pac_total%>%
  select(idn_empreendimento:ESTADOS)%>%
  mutate(
    Regiao=case_when(
      sig_uf %in% c('RS','PR','SC')~'Sul',
      sig_uf %in% c('SP','RJ','ES','MG')~'Sudeste',
      sig_uf %in% c('DF','MT','MS','GO')~'Centro-Oeste',
      sig_uf %in% c('BA','PE','PB','CE','MA','RN','SE','AL','PI')~'Nordeste',
      sig_uf %in% c('AM','TO','RR','AP','PA','AC','RO')~'Norte',
    )
  )
table(pac_total$subeixo)
table(pac_total$tipo)

#remove variaveis
pac_total$idn_empreendimento=NULL
pac_total$id_digs=NULL
pac_total$idn_estagio=NULL
pac_total$Qtd_uf=NULL

#Tipos de dados
str(pac_total)

# Qtde Valores NA
pac_total%>%filter(is.na(investimento_total))%>%
  group_by(data_ciclo)%>%
  count(investimento_total)

# Cria Dataframe sem valores nulos
pac_total_valor <- pac_total%>%filter(!is.na(investimento_total))
  
#Substitui 
pac_total$investimento_total<- replace(pac_total$investimento_total,NA,0)

pac_total$investimento_total<-ifelse(is.na(pac_total$investimento_total)
                  ,0,pac_total$investimento_total)

############### Analise das Obras Dataset pac_total ################################
## 1 -- str para visualizar os tipos dos dados
## 2 Analise Investimento total (amp e distribuicao)
## 2.1 Investimento total distribuicao em relacao as outras variaves

#media e mediana
class(pac_total)
sprintf('media %f ',mean(valor))

#Função para media mediana e desvio padrao
medidas<-function(valor){
  tibble('media'=mean(valor),
         'mediana'= median(valor),
         'sd'= sd(valor))
  }

#Funcao Moda
moda <- function(valor){
  calc<- table(valor)
  names(calc) [calc == max(calc)]
}
#executa função
medidas(pac_total_valor$investimento_total)
## O valor da média está muito distante da mediana, indicando que 
## este valor esta sendo influenciado por outliers
#moda
moda(pac_total_valor$investimento_total)

#Quantidade de valores 0
pac_total%>%
  filter(investimento_total==0)%>%
  tally()

sprintf('media %f',mean(pac_total2$investimento_total))

#Distribuição dos dados
#Quantile
format(quantile(pac_total_valor$investimento_total),scientific = F)
quantile(pac_total_valor$investimento_total,0.25)
## Há uma grande variabilidade nos dados

grupo<-tapply(pac_total_valor$investimento_total,pac_total_valor$tipo,scale )
View(grupo)


#calculando zscore por grupo tipo
pac_zscore <- pac_total_valor%>% group_by(tipo)%>%
  select(titulo:Regiao)%>%
  mutate(zscore = scale(investimento_total),qtd=n())%>%
  ungroup %>% 
  mutate(z_score_ungrouped = scale(investimento_total))

#total por tipo  
pac_total_valor%>% group_by(tipo)%>%
  tally()
  #moda verificar se ha 2 conjuntos de dados

#sd ajuda a verificar se os dados são mais parecidos

#cv ajuda a comparar a variabilidade entre datasets

#agrupando dateset por tipo de obra e calculando media, desvio, mediana e cv
pac_tipos<- pac_zscore%>%group_by(tipo)%>%
  select(titulo :z_score_ungrouped)%>%
  mutate(n=n(),
            media = mean(investimento_total),
            desvio = sd(investimento_total),
            mediana = median(investimento_total))%>%
            mutate(cv = (desvio / media) *100)%>%
  arrange(cv,desvio)

#Obeservando os valores, os tipos de obras com menor variabilidade são:
# 1-Centro de Iniciação ao Esporte
# 2-Educação
# 3-Centro de Artes e Esportes Unificados
# 4-Creches e Pré Escolas

# Adicionando UF

pac_tipos_uf<- pac_total_valor%>%group_by(tipo,sig_uf)%>%
  summarise(n=n(),
            media = mean(investimento_total),
            desvio = sd(investimento_total),
            mediana = median(investimento_total))%>%
  mutate(cv = (desvio / media) *100)%>%
  arrange(n)

#Salvando Dataframe
write.csv(pac_total,'Pac_total.csv')
write.csv(pac_total_valor,'Pac_total_valor.csv')

# Plot - Qtd por Tipo 
ggplot(data = pac_tipos_uf,aes(reorder(tipo,n),y=n))+
  geom_bar(stat='identity')+
  coord_flip()

count(investimento_total)%>%
  filter(investimento_total<=quantile(pac_total2$investimento_total,0.25))

hist(pac_total$investimento_total)
ggplot(data= pac_total,aes(investimento_total))+
  geom_histogram(bins = 30)+
  scale_x_continuous(labels = scales::comma)

ggplot(data= pac_total,aes(Regiao,investimento_total))+
  geom_boxplot()+
  ylim(100000000,900000000)+
  scale_y_continuous(labels = scales::comma)+
  ylim(100000000,900000000)



pac_total2<- pac_total%>%filter(!is.na(investimento_total))

ggplot(data= pac_total2,aes(Regiao,investimento_total))+
  geom_boxplot()+
  scale_y_continuous(labels = scales::comma)+
  ylim(100000000,900000000)

c(max(pac_total2$investimento_total),min(pac_total2$investimento_total))

max(pac_total2$investimento_total)
min(pac_total2$investimento_total)

View(pac_total2%>%filter(investimento_total %in% c(41764548000,37.62)))

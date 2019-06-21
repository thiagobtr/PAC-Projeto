library('ggplot2')
library('dplyr')
library('lubridate')
library('stringr')
library('readr')
library('plyr')

setwd('/media/sf_FormacaoCientistaDeDados/Portfolio/AnaliseBrasil')



######################### Importacao arquivo ######################################

### os arquivos foram agrupados em 2 tipos, pois a descrição dos campos foi atualizada
## a partir da versao de 2015  
## seleciona os arquivos


## Arquivos Pac a partir de 2015
arquivos_V2 <- list.files(path = 'Data/PAC/',pattern='*.csv',full.names = TRUE)

## Arquivos Pac até 2014
arquivos_V1 <- list.files(path = 'Data/PAC/PAC2/',pattern='*.csv',full.names = TRUE)


#Carregando os arquivos e gerando um dataframe utilizando ldply
arq_V1 <- ldply(arquivos_V1, read_csv2,col_names=TRUE ,locale=locale(encoding = 'Latin1'),
                col_types = cols(
                  val_2011_2014 = col_character(),
                  val_pos_2014 = col_character(),
                  investimento_total = col_character()
                )
)

arq_V2 <- ldply(arquivos_V2, read_csv,col_names=TRUE ,locale=locale(encoding = 'Latin1'),
                col_types = cols(
                  investimento_total = col_character()
                )
)

str(arq_V1)
str(arq_V2)

#### read.csv base package
# arq_V1 <- ldply(arquivos_V1, read.csv,header=TRUE,sep=';',fileEncoding='latin1'
#               , stringsAsFactors =FALSE)

# arq_V2 <- ldply(arquivos_V2, read.csv,header=TRUE,sep=',',fileEncoding='latin1'
#                 , stringsAsFactors =FALSE)


#Importacao arquivo de Siglas e estados
sigla_uf <- data.frame(read.csv('Data/Estados_Siglas.txt',header=TRUE,sep = '\t',
                                stringsAsFactors = FALSE))

#Dados de saneamento
saneam <- data.frame(read.csv('Data/IndicadoresSaneamentoGeral.csv',header=TRUE,sep=';'
                              ,fileEncoding = 'latin1',stringsAsFactors = FALSE))

#Variavel investimento_total recebendo valor mais recente 
arq_V1$investimento_total<-ifelse(is.na(arq_V1$investimento_total),
                                  ifelse(is.na(arq_V1$val_pos_2014),arq_V1$val_2011_2014,
                                         arq_V1$val_pos_2014),arq_V1$investimento_total)

#exclui variaveis
arq_V1$val_pos_2014=NULL
arq_V1$val_2011_2014=NULL

#Exclui variavel "observacao....."
arq_V2$observacao.....=NULL


#seleciona header
col_names <- colnames(arq_V2)
#atribui header ao arquivo arq_V1 
names(arq_V1) <- col_names

#Uniao dos dataframes
pac_total <- rbind(arq_V2,arq_V1)


#Exclui df arq_V1 e arq_V2 
remove(arq_V1)
remove(arq_V2)

##########
# Conversao para formato data com readr::parse_date 
## Criar loop para conversao de tipo
pac_total$data_ciclo = parse_date(pac_total$dat_ciclo,"%d/%m/%Y") 
pac_total$data_conclusao = parse_date(pac_total$dat_conclusao_revisada,"%d/%m/%Y") 
pac_total$data_selecao = parse_date(pac_total$dat_selecao,"%d/%m/%Y") 

# Exclusao das variaveis originais
pac_total$dat_ciclo = NULL
pac_total$dat_conclusao_revisada = NULL 
pac_total$dat_selecao = NULL


# Replace nao digito por ""
pac_total$lat <- str_replace_all(pac_total$obra_latitude,"\\D","")
pac_total$lon <- str_replace_all(pac_total$obra_longitude,"\\D","")

# Substitui os valores de "em revisao" por 0
pac_total$investimento_total <- str_replace_all(pac_total$investimento_total,"em revisão","0")


# outra forma para remover colunas
pac_total <- select(pac_total,-c(obra_latitude,obra_longitude))

#conversao para tibble
pac_total <- as_tibble(pac_total)

# Qtd de valores distintos
pac_total%>%
  summarise(n=n_distinct(idn_empreendimento))

# por conflito com algumas funçoes do dplyr vamos remover o plyr da sessao
detach(package:plyr)

### No arquivo temos 582987 registros dos quais 65312 são unicos.
pac_total_distinct <- pac_total%>%
  group_by(idn_empreendimento)%>%
  summarise(max_data =  max(data_ciclo,na.rm=TRUE),
            min_data =  min(data_ciclo,na.rm=TRUE))

# Seleciona os registros mais atualizados
pac_total_max_data <- pac_total%>%
  inner_join(pac_total_distinct,by = c('idn_empreendimento', 'data_ciclo' = 'max_data' ))

pac_total_min_data <- pac_total%>%
  inner_join(pac_total_distinct,by = c('idn_empreendimento', 'data_ciclo' = 'min_data' ))

pac_total_V3 <- pac_total_max_data %>%
  inner_join(select(pac_total_min_data,idn_empreendimento,investimento_total),
             by= c('idn_empreendimento')) %>%
  rename(investimento_total = investimento_total.x,
         investimento_total_min = investimento_total.y)



View(pac_total_V3 %>%
       filter (idn_empreendimento== 30358))

# Conversao de campos para tipo double
pac_total_V3$investimento_total <- parse_double(pac_total_V3$investimento_total,
                                                na =c("","NA"),
                                                locale = locale(decimal_mark = "."))

pac_total_V3$investimento_total_min <- parse_double(pac_total_V3$investimento_total_min,
                                                    na =c("","NA"),
                                                    locale = locale(decimal_mark = "."))

# Tipos de campos
str(pac_total_V3)

# Remove pac_distinct
#remove(pac_total_V3)  

# Criaçao de Dataframe para definicao do estagio da Obra
estagio_obras = tibble (
  codigo = c(0,3,5,10,40,41,42,70,71,90,91),
  descricao= c('Nao informado', 'A selecionar','Em contratação','Ação Preparatória',
               'Em licitação de obra','Em licitação de projeto','Em execução de projeto',
               'Em obras','Em execução','Concluído','Em operação'))

# Criacao DataFrame para Tipo e subeixo do empreendimento
tipos_empr = tibble(
  tipo <- c('Rodovias','Ferrovia','Porto', 'Hidrovia','Aeroporto'
            ,'Defesa','Comunicaçoes','Ciencia e Tecnologia'
            ,'Geração de Energia Elétrica','Transmissão de Energia Elétrica','Petróleo e Gás Natural'
            ,'Geologia e Mineração - Cprm','Marinha Mercante','Combustíveis Renováveis','Saneamento'
            ,'Prevenção em áreas de risco','Pavimentação','Mobilidade Urbana'
            ,'Cidades Digitais','Cidades Históricas','Infraestrutura Turística','Olimpíadas'
            ,'Educação','Saúde','SUFRAMA','Casa da Mulher Brasileira','UBS','UPA','Creches e Pré Escolas'
            ,'Quadras Esportivas nas Escolas','Centro de Artes e Esportes Unificados'
            ,'Centro de Iniciação ao Esporte','MCMV','Urbanização de assentamentos precários'
            ,'Financiamento SBPE','Luz para Todos','Recursos Hídricos','Água em áreas urbanas','Estradas Vicinais'),
  subeixo <-c(rep('Infraestrutura Logística',8),rep('Infraestrutura Energética',6),
              rep('Infraestrutura Social e Urbana',23),'Infraestrutura Social e Urbana','Infraestrutura Logística'),
  idn_digs <- c(1000,1001,1002,1003,1004,1006,1007,1008,2000,2001,2002,2003,2004,2005,3000,
                3001,3002,3003,3004,3005,3006,3007,3011,3012,3013,3009,4000,4001,4002,4003,4004,
                4005,5000,5001,5002,6000,6002,6001,1005),.name_repair= ~ c('tipo','subeixo','idn_digs')
)

#join 
# Criando novo dataframe adicionando variavel "estagio" variavel "descricao" 
# do df estagio_obras 
pac_total_V3 <-left_join(pac_total_V3,estagio_obras,by = c("idn_estagio"= "codigo"))

# Criando novo dataframe adicionando as informaçoes de  "tipo_eixo" e "subeixo"
pac_total_V3 <- left_join(pac_total_V3,tipos_empr,by = "idn_digs")

#Criacao de coluna para Qtd de UF
pac_total_V3 <- pac_total_V3%>%
  mutate(Qtd_uf = str_count(sig_uf,"[A-Z]")/2)

# filtra o df buscando Obras para somente 1 estado e em seguida faz um join 
# adicionando seu nome
pac_total_V3 <- pac_total_V3%>%filter(Qtd_uf==1)%>%
  inner_join(sigla_uf,by=c("sig_uf"="SIGLA"))

# Cria variavel Regiao
pac_total_V3 <- pac_total_V3%>%
  mutate(
    Regiao=case_when(
      sig_uf %in% c('RS','PR','SC')~'Sul',
      sig_uf %in% c('SP','RJ','ES','MG')~'Sudeste',
      sig_uf %in% c('DF','MT','MS','GO')~'Centro-Oeste',
      sig_uf %in% c('BA','PE','PB','CE','MA','RN','SE','AL','PI')~'Nordeste',
      sig_uf %in% c('AM','TO','RR','AP','PA','AC','RO')~'Norte',
    )
  )

# Verifica distirbuição dos dados
table(pac_total_V3$subeixo)
table(pac_total_V3$tipo)

# verifica qtd valores nulos
pac_total_V3%>% filter(is.na(investimento_total))%>%
  count(observacao)

# CArrega package para subsituir valores NA 
library('tidyr') #replace_na

# Atualiza valores nulos
pac_total_V3$investimento_total <- replace_na(pac_total_V3$investimento_total,0)
pac_total_V3$investimento_total_min <- replace_na(pac_total_V3$investimento_total_min,0)

# selecionado o valor medio por tipo 
pac_media <- 
  pac_total_V3%>%
  filter(investimento_total > 0)%>%
  group_by(tipo)%>%
  summarise(media = mean(investimento_total,na.rm = TRUE),qtd=n())%>%
  #mutate(media = replace_na (media,0))%>%
  select(tipo,media)


# retira package da memoria
detach(package:tidyr)


# Atribui valor da media 
pac_total_V3 <- pac_total_V3%>%
  inner_join(pac_media,by = "tipo")


#remove variaveis
pac_total_V3$idn_empreendimento=NULL
pac_total_V3$id_digs=NULL
pac_total_V3$idn_estagio=NULL
pac_total_V3$Qtd_uf=NULL


# Calculando zscore por grupo tipo
pac_zscore <- pac_total_V3 %>% 
  filter(investimento_total > 0)%>%
  group_by(tipo)%>%
  mutate(zscore_tipo = scale(investimento_total),qtd=n())%>%
  ungroup 

# Calcula zscore por Tipo e UF  
pac_zscore <- pac_zscore %>% group_by(sig_uf,tipo)%>%
  mutate(zscore_tipo_uf = scale(investimento_total),qtd=n())%>%
  ungroup

# Tipos de dados
str(pac_total)

# Qtde Valores NA
pac_zscore%>%filter(is.na(investimento_total))%>%
  group_by(data_ciclo)%>%
  count(investimento_total)


remove(pac_total_V3)

############### Analise das Obras Dataset pac_total ################################
## 1 -- str para visualizar os tipos dos dados
## 2 Analise Investimento total (amp e distribuicao)
## 2.1 Investimento total distribuicao em relacao as outras variaves


#Função para media mediana e desvio padrao
medidas <- function(valor, x){
  tibble('media'=mean(valor,na.rm = x),
         'mediana'= median(valor,na.rm = x),
         'sd'= sd(valor,na.rm = x),
         'IQR'=IQR(valor,na.rm = x))
}

#Funcao Moda
moda <- function(valor){
  calc <- table(valor)
  names(calc) [calc == max(calc)]
}
#executa função
medidas(pac_zscore$investimento_total,TRUE)
## O valor da média está muito distante da mediana, indicando que 
## este valor esta sendo influenciado por outliers.
## Neste dataset a amplitude dos valores é muito grande, o que acaba
## influenciando nessa diferença
#moda
moda(pac_zscore$investimento_total)



# Distribuição dos dados
# Quantile
format(quantile(pac_total_valor$investimento_total),scientific = F)
quantile(pac_total_valor$investimento_total,0.25)
## Há uma grande variabilidade nos dados

# sd ajuda a verificar se os dados são mais parecidos

# cv ajuda a comparar a variabilidade entre datasets

# agrupando dateset por tipo de obra e calculando media, desvio, mediana e cv
pac_tipos <- pac_zscore%>%
  filter(investimento_total > 0)%>%
  group_by(tipo)%>%
  select(tipo,investimento_total)%>%
  summarise(qtd = n(),
            media = mean(investimento_total),
            desvio = sd(investimento_total),
            mediana = median(investimento_total),
            valor_max = max(investimento_total),
            valor_min = min(investimento_total))%>%
  mutate(cv = (desvio / media) *100)%>%
  arrange(cv,desvio)

# Salvando arquivo
write_csv(pac_tipos,'Pac_tipos.csv')
write_csv(pac_zscore,'Pac_norm.csv')

# Atualizando variaveis sem valor de investimento com a media por tipo de obra


#Obeservando os valores, os tipos de obras com menor variabilidade são:
# 1-Estradas Vicinais
# 2-Centro de Iniciação ao Esporte
# 3-Educação
# 4-Centro de Artes e Esportes Unificados
# 5-Creches e Pré Escolas

# Adicionando UF

pac_tipos_uf <- pac_zscore%>%filter(investimento_total>0)%>%
  group_by(tipo,sig_uf)%>%
  summarise(n=n(),
            media = mean(investimento_total),
            desvio = sd(investimento_total),
            mediana = median(investimento_total))%>%
  mutate(cv = (desvio / media) *100)%>%
  arrange(n)

# Salvando Dataframe
write.csv(pac_total,'Pac_total.csv')
write.csv(pac_total_valor,'Pac_total_valor.csv')

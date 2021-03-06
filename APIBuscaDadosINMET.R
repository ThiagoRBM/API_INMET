## script simples para buscar de dados pela API do INMET, 
## com alguns gr�ficos de exemplo

library(readr)
library(httr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(pals)


## dados climaticos Brasil: 
## https://portal.inmet.gov.br/manual/manual-de-uso-da-api-esta%C3%A7%C3%B5es

estacoesTipos= c("https://apitempo.inmet.gov.br/estacoes/M",
                 "https://apitempo.inmet.gov.br/estacoes/T") ## estacoes manuais e automaticas



Estacoes= lapply(estacoesTipos, httr::GET)

DadosEstacoes= lapply(Estacoes, function(x) httr::content(x, encoding = "UTF-8",
                                                          type = 'application/json'))

## coluna DT_FIM_OPERACAO nao existe em todas as estacoes


ListaEsta= vector("list", length= length(unique(DadosEstacoes)))
names(ListaEsta)= c("Manual", "Automatica")

for (i in 1:length(DadosEstacoes)){ ## nesse loop, dados de cada esta��o, como tipo e localiza��o (coordenadas)
  ## sao obtidos. Cada esta��o tem um c�digo �nico, que � usado abaixo para conseguir os dados meteorol�gicos
  ## das esta��es
  
  Lista= vector("list", length= length(DadosEstacoes[[i]]))
  for(j in 1:length(DadosEstacoes[[i]])){
    
    dt= as.data.frame(t(do.call("rbind", DadosEstacoes[[i]][[j]])))
    
    Lista[[j]]= data.frame(CD_OSCAR= ifelse(is.null(dt$CD_OSCAR), NA, dt$CD_OSCAR),
                           DC_NOME= dt$DC_NOME,
                           TP_ESTACAO= dt$TP_ESTACAO,
                           CD_ESTACAO= dt$CD_ESTACAO,
                           VL_LATITUDE= dt$VL_LATITUDE,
                           VL_LONGITUDE= dt$VL_LONGITUDE,
                           SG_ESTADO= dt$SG_ESTADO,
                           DT_INICIO_OPERACAO= dt$DT_INICIO_OPERACAO)
    
    if(j == length(DadosEstacoes[[i]])){Lista= rbindlist(Lista, fill=TRUE, use.names=TRUE)}
    
  }
  
  ListaEsta[[i]] = Lista
  
  if(i == length(DadosEstacoes)){
    tabEsta= rbindlist(ListaEsta, fill=TRUE, use.names=TRUE)
  }
  
}


tabEsta$DT_INICIO_OPERACAO= as.Date(tabEsta$DT_INICIO_OPERACAO)
Estados= c("SP", "GO", "DF") ## criando um vetor para os Estados que quero
## buscar dados clim�ticos

inicioOperacao=tabEsta %>% 
  filter(SG_ESTADO %in% Estados) %>% 
  group_by(DT_INICIO_OPERACAO) %>% 
  mutate(DT_INICIO_OPERACAO= format(DT_INICIO_OPERACAO, format = "%Y")) %>% 
  dplyr::summarise(nData= n()) ## sumarizando os dados de ano de in�cio de opera��o de esta��es

listaPeriodos= vector("list")
for(i in 1:length(tabEsta$DT_INICIO_OPERACAO[tabEsta$SG_ESTADO %in% Estados])){
  estacao= tabEsta[tabEsta$SG_ESTADO %in% Estados][i]
  inicio= estacao$DT_INICIO_OPERACAO
  periodos= seq(inicio,to= Sys.Date(), by='6 month')
  estado= estacao$SG_ESTADO
  codigo= estacao$CD_ESTACAO
  vecPeriodos= vector("list", length= length(periodos))
  for(periodo in 1:length(periodos)){
    inicio= as.Date(periodos[periodo])
    fim= as.Date(periodos[periodo+1])
    vecPeriodos[[periodo]]= data.frame(estado,inicio,fim,codigo)
    if(periodo == length(periodos)){
      vecPeriodos= do.call("rbind", vecPeriodos)
    }
  }
  listaPeriodos[[i]]=vecPeriodos
  
  if(i == length(tabEsta$DT_INICIO_OPERACAO[tabEsta$SG_ESTADO %in% Estados])){
    listaPeriodos= do.call("rbind", listaPeriodos) %>% 
      data.frame() %>% 
      tidyr::drop_na(fim)
  }
}



url= paste0("https://apitempo.inmet.gov.br/estacao/diaria/", 
            listaPeriodos$inicio,"/",
            ## quixeramobim (CE, codigo 82586) � a esta��o com inicio de operacao mais antigo
            ## mas aparentemente so estao disponiveis os dados a partir de 1961
            listaPeriodos$fim,"/",
            listaPeriodos$codigo) ## montando as URL para
## as buscas dos dados meteorol�gicos dos estados do vetor, com base nos Estados de interesse
## e nas data de in�cio das opera��es

DadosClima= lapply(url, httr::GET)

Dados= lapply(DadosClima, function(x) httr::content(x, encoding = "UTF-8",
                                                    type = 'application/json'))

ListaClima= vector("list", length= length(unique(Dados)))

for (i in 1:length(Dados)){
  
  Lista= vector("list", length= length(Dados[[i]]))
  for(j in 1:length(Dados[[i]])){
    
    dt= as.data.frame(t(do.call("rbind", Dados[[i]][[j]])))
    
    Lista[[j]]= data.frame(TEMP_MAX= ifelse(is.null(dt$TEMP_MAX), NA, dt$TEMP_MAX),
                           UMID_MED= ifelse(is.null(dt$UMID_MED), NA, dt$UMID_MED),
                           UF= ifelse(is.null(dt$UF), NA, dt$UF),
                           DT_MEDICAO= ifelse(is.null(dt$DT_MEDICAO), NA, dt$DT_MEDICAO),
                           DC_NOME= ifelse(is.null(dt$DC_NOME), NA, dt$DC_NOME),
                           CHUVA= ifelse(is.null(dt$CHUVA), NA, dt$CHUVA),
                           CD_ESTACAO= ifelse(is.null(dt$CD_ESTACAO), NA, dt$CD_ESTACAO),
                           VL_LATITUDE= ifelse(is.null(dt$VL_LATITUDE), NA, dt$VL_LATITUDE),
                           VL_LONGITUDE= ifelse(is.null(dt$VL_LONGITUDE), NA, dt$VL_LONGITUDE),
                           TEMP_MIN= ifelse(is.null(dt$TEMP_MIN), NA, dt$TEMP_MIN),
                           TEMP_MED= ifelse(is.null(dt$TEMP_MED), NA, dt$TEMP_MED))
    
    if(j == length(Dados[[i]])){Lista= rbindlist(Lista, fill=TRUE, use.names=TRUE)}
    
  }
  
  ListaClima[[i]] = Lista
  
  if(i == length(Dados)){
    tabClima= rbindlist(ListaClima, fill=TRUE, use.names=TRUE)
  }
  
}

#write.table(tabClima,
#            file= paste0("C:/Users/HP/Desktop/", "Clima_DF_SP_GO_", format(Sys.time(), "%d%m%Y"), ".txt"),
#            sep= ";", dec= ".", quote= FALSE, row.names= FALSE, col.names= TRUE)

#### Tabelas para gr�ficos gerais, com m�dias anuais
#### 
#### 
#### 
#### 
#### 
#### 
#### 
#### 

tabClimaSumm= tabClima
tabClimaSumm$ano= format(as.Date(tabClimaSumm$DT_MEDICAO), format = "%Y")

tabClimaSumm2= tabClimaSumm %>%
  group_by(UF, ano) %>% 
  mutate(TEMP_MED= as.numeric(TEMP_MED),
         TEMP_MAX= as.numeric(TEMP_MAX),
         UMID_MED= as.numeric(UMID_MED)) %>% 
  dplyr::summarise(nMedicao= n(), 
                   medTmed= mean(TEMP_MED, na.rm=TRUE),
                   sdTmed= sd(TEMP_MED, na.rm=TRUE),
                   medTmax= mean(TEMP_MAX, na.rm=TRUE),
                   sdTmax= sd(TEMP_MAX, na.rm= TRUE),
                   medUmi= mean(UMID_MED, na.rm= TRUE),
                   sdUmi= sd(UMID_MED, na.rm= TRUE))

(as.numeric(max(tabClimaSumm2$ano)) - as.numeric(min(tabClimaSumm2$ano))) ## quantidade de ano para que 
## existem dados, independente de Estado


coresUF= setNames(glasbey()[unique(as.numeric(as.factor(tabClimaSumm2$UF)))], 
                  unique(tabClimaSumm2$UF))

spline_int= list()
for(i in 1:length(unique(tabClimaSumm2$UF))){
  
  UF= unique(tabClimaSumm2$UF)[i]
  
  df= data.frame(UF= tabClimaSumm2$UF[tabClimaSumm2$UF == UF],
                 ano= tabClimaSumm2$ano[tabClimaSumm2$UF == UF],
                 spline(interaction(tabClimaSumm2$UF[tabClimaSumm2$UF == UF], 
                                    tabClimaSumm2$ano[tabClimaSumm2$UF == UF]), 
                        tabClimaSumm2$medTmed[tabClimaSumm2$UF == UF]))
  
  spline_int[[i]]= df 
  
  if(i == length(unique(tabClimaSumm2$UF))){
    spline_int= do.call("rbind", spline_int)
  }
  
} ## gerando uma tabela com interpola��o "padr�o" das m�dias (de todas as esta��es) 
## de um ano para o seguinte para cada ano, para cada Estado

ggplot()+
  geom_point(data= tabClimaSumm2, aes(x=ano, y=medTmed, color= UF)) +
  geom_errorbar(data= tabClimaSumm2, aes(x=ano, ymin= medTmed-sdTmed, ymax=medTmed+sdTmed,
                                         color= UF)) +
  facet_wrap(~UF, ncol= 1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  geom_line(data = spline_int, aes(x = x, y = y))
## gr�fico da varia��o de temperatura para os 3 Estados do exemplo


#### Tabelas para gr�ficos gerais, com m�dias por estacao (outono, inverno, primavera, verao)
#### 
#### 
#### 
#### 
#### 
#### 
#### 
####  

tabClimaSummEstacao= tabClima
tabClimaSummEstacao$DT_MEDICAO= as.Date(tabClimaSummEstacao$DT_MEDICAO)
tabClimaSummEstacao$ano= format(as.Date(tabClimaSummEstacao$DT_MEDICAO), format = "%Y")

yday(grep("2010-03-20",tabClimaSummEstacao$DT_MEDICAO, value=TRUE)[1]) # come�o outono (dia 79)
yday(grep("2010-06-21",tabClimaSummEstacao$DT_MEDICAO, value=TRUE)[1]) # fim outono e come�o
# inverno (dia 172)

yday(grep("2010-09-22",tabClimaSummEstacao$DT_MEDICAO, value=TRUE)[1]) # fim do inverno e come�o
# da primavera (dia 265)
yday(grep("2010-12-21",tabClimaSummEstacao$DT_MEDICAO, value=TRUE)[1]) # fim da primavera e com�o do ver�o
# (dia 355) 



tabClimaSummEstacao2= tabClimaSummEstacao %>%
  mutate(DT_MEDICAO= as.POSIXct(DT_MEDICAO)) %>% ### peguei os dias do ano (1 a 365) a data
  ## para definir as esta��es, fica mais f�cil. Olhei na internet quais data de in�cio e fim
  ## de cada esta��o e peguei os dias do ano de cada uma
  ## me baseei nesse https://stackoverflow.com/questions/51243760/r-create-a-new-field-applying-condition-on-a-date-field
  mutate(estacao= case_when(yday(DT_MEDICAO) > 79 & yday(DT_MEDICAO) < 173 ~ "outono",
                            yday(DT_MEDICAO) > 172 & yday(DT_MEDICAO) < 266 ~ "inverno",
                            yday(DT_MEDICAO) > 265 & yday(DT_MEDICAO) < 356 ~ "primavera",
                            TRUE~ "verao")) %>% 
  mutate(TEMP_MED= as.numeric(TEMP_MED),
         TEMP_MAX= as.numeric(TEMP_MAX),
         UMID_MED= as.numeric(UMID_MED)) %>% 
  group_by(UF, ano, estacao) %>%
  dplyr::summarise(nMedicao= n(), 
                   medTmed= mean(TEMP_MED, na.rm=TRUE),
                   sdTmed= sd(TEMP_MED, na.rm=TRUE),
                   medTmax= mean(TEMP_MAX, na.rm=TRUE),
                   sdTmax= sd(TEMP_MAX, na.rm= TRUE),
                   medUmi= mean(UMID_MED, na.rm= TRUE),
                   sdUmi= sd(UMID_MED, na.rm= TRUE))

medEstacoes= tabClimaSummEstacao2 %>% 
  group_by(estacao) %>% 
  dplyr::summarise(medEstacao= mean(medTmed, na.rm=TRUE))

coresUF= setNames(glasbey()[unique(as.numeric(as.factor(tabClimaSummEstacao2$UF)))], 
                  unique(tabClimaSummEstacao2$UF))

coresEstacao= setNames(glasbey()[unique(as.numeric(as.factor(tabClimaSummEstacao2$estacao)))], 
                       unique(tabClimaSummEstacao2$estacao))


ggplot()+
  geom_point(data= tabClimaSummEstacao2, aes(x=ano, y=medTmed, color= UF, shape= estacao)) +
  #geom_errorbar(data= tabClimaSummEstacao2, aes(x=ano, ymin= medTmed-sdTmed, ymax=medTmed+sdTmed,
  #                                       color= estacao)) +
  geom_hline(data = medEstacoes, aes(yintercept = medEstacao), color= "darkred") +
  facet_wrap(~estacao, nrow= 1) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  scale_colour_manual(values= coresUF) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=5)) +
  NULL

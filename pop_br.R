########################
#POPULACAO - MUNICÍPIOS#
########################

##Code to generate a sequel of maps of Brazilian population by cities, from 1872 to 2016  

##Author: Gabriel Zanlorenssi

##Libraries
library(ggmap)
library(dplyr)
library(tm)
library(stringr)
library(stringi)
library(ggplot2)
library(readxl)
library(ggmap)
library(reshape2)

##1872 a 1970

url1<-"https://github.com/gabrielzanlorenssi/populacao_BR/raw/master/ipeadata%5B19-06-2017-01-43%5D.xls"
download.file(url1, "ipeadata[19-06-2017-01-43].xls", mode="wb")

pop_1872a70<-read_xls("ipeadata[19-06-2017-01-43].xls", sheet="Séries")

pop_1872long <- melt(pop_1872a70, id.vars = c("Sigla", "Codigo", "Município"))
pop_1872long$Codigo <- as.numeric(substr(pop_1872long$Codigo, 1,6))

pop_1872long <- pop_1872long %>%
  rename(uf=Sigla, municipio=Município, cod_ibge=Codigo, populacao=value, ano=variable)


##1980 a 2012

url2<-"https://raw.githubusercontent.com/gabrielzanlorenssi/populacao_BR/master/A132925189_79_76_80.csv"
download.file(url2, "A132925189_79_76_80.csv")

pop_80a12<-read.csv(file="A132925189_79_76_80.csv", header=TRUE, sep=";", skip=3)
pop_80a12<-pop_80a12[1:5618,]
pop_80a12$cod_ibge<-as.numeric(substr(pop_80a12$Município, 1,6))
pop_80a12$municipio<-(substr(pop_80a12$Município, 8,100))

pop_80long <- melt(pop_80a12, id.vars = c("Município", "municipio", "cod_ibge"))
pop_80long$ano<-as.numeric(str_replace_all(pop_80long$variable, "X", ""))
pop_80long$populacao<-as.numeric(str_replace_all(pop_80long$value, "-", ""))
pop_80long<-pop_80long[c(2,3,6,7)]


##2013 a 2016

url2013<-"ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2013/estimativa_2013_TCU_20170614.xls"
url2014<-"ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2014/estimativa_TCU_2014_20170614.xls"
url2015<-"ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2015/estimativa_TCU_2015_20170614.xls"
url2016<-"ftp://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2016/estimativa_TCU_2016_20170614.xls"

download.file(url2013, "estimativa_2013_TCU_20170614.xls", mode="wb")
download.file(url2014, "estimativa_TCU_2014_20170614.xls", mode="wb")
download.file(url2015, "estimativa_TCU_2015_20170614.xls", mode="wb")
download.file(url2016, "estimativa_TCU_2016_20170614.xls", mode="wb")

pop_13<-read_xls("~/estimativa_2013_TCU_20170614.xls", sheet="Municípios", skip=1)
pop_13<-pop_13[1:5570,]
pop_13$ano<-2013
pop_14<-read_xls("~/estimativa_TCU_2014_20170614.xls", sheet="Municípios", skip=1)
pop_14<-pop_14[1:5570,]
pop_14$ano<-2014
pop_15<-read_xls("~/estimativa_TCU_2015_20170614.xls", sheet="Municípios", skip=1)
pop_15<-pop_15[1:5570,]
pop_15$ano<-2015
pop_16<-read_xls("~/estimativa_TCU_2016_20170614.xls", sheet="Municípios", skip=1)
pop_16<-pop_16[1:5570,]
pop_16$ano<-2016

pop_13a16<-rbind(pop_13, pop_14, pop_15, pop_16)

pop_13a16$cod_ibge<-as.numeric(substr(
  paste(pop_13a16$`COD. UF`, pop_13a16$`COD. MUNIC`, sep=""),1,6))

pop_13a16<-pop_13a16[c(1,4,5,6,7)]

detach("package:reshape", unload=TRUE)
library(dplyr)

pop_13a16<-pop_13a16 %>%
  rename(populacao=`POPULAÇÃO ESTIMADA`, municipio=`NOME DO MUNICÍPIO`, uf=UF)


###########
#Populacao#
###########

pop_13a16$populacao<-stri_replace_all_fixed(pop_13a16$populacao, "(1)", "")
pop_13a16$populacao<-stri_replace_all_fixed(pop_13a16$populacao, "(2)", "")
pop_13a16$populacao<-stri_replace_all_fixed(pop_13a16$populacao, "(3)", "")
pop_13a16$populacao<-stri_replace_all_fixed(pop_13a16$populacao, ".", "")
pop_13a16$populacao<-as.numeric(pop_13a16$populacao)

pop_final<-full_join(pop_13a16, pop_80long, by=c("populacao", "municipio", "ano", "cod_ibge"))

pop_1872long$ano<-as.numeric(as.character(pop_1872long$ano))
pop_final<-full_join(pop_final, pop_1872long, by=c("populacao", "municipio", "ano", "cod_ibge"))

#corrigir municipio/uf
populacao<-pop_final[c(3,4,5)]

cod_merge<-pop_13a16[c(1,2,4,5)] %>%
  filter(ano==2016)
cod_merge<-cod_merge[c(1,2,4)]

populacao<-full_join(populacao, cod_merge, by="cod_ibge")

#teste
populacao22<-populacao %>%
  filter(municipio=="Foz do Iguaçu")

theme_set(theme_bw())
ggplot(data=populacao22, aes(x=ano, y=populacao)) + xlim(c(1940,2016)) + ylim(c(10000,350000)) +
  geom_line() + geom_point() + labs(x="Ano", y="População", title="Foz do Iguaçu - PR")
ggsave("rn.png")

###########
##Geocode##
###########

geocode<-pop_13a16 %>%
  filter(ano==2016)

geocode$id<-c(1:5570)

geocode$municipio2<-geocode$municipio
geocode$municipio2<-tolower(geocode$municipio2)
geocode$municipio2<-removePunctuation(geocode$municipio2)
geocode$municipio2<-str_replace_all(geocode$municipio2, "ú", "u")
geocode$municipio2<-str_replace_all(geocode$municipio2, "â", "a")
geocode$municipio2<-str_replace_all(geocode$municipio2, "á", "a")
geocode$municipio2<-str_replace_all(geocode$municipio2, "ã", "a")
geocode$municipio2<-str_replace_all(geocode$municipio2, "é", "e")
geocode$municipio2<-str_replace_all(geocode$municipio2, "ê", "e")
geocode$municipio2<-str_replace_all(geocode$municipio2, "ç", "c")
geocode$municipio2<-str_replace_all(geocode$municipio2, "í", "i")
geocode$municipio2<-str_replace_all(geocode$municipio2, "\n", "")
geocode$municipio2<-str_replace_all(geocode$municipio2, "û", "u")
geocode$municipio2<-str_replace_all(geocode$municipio2, "ú", "u")
geocode$municipio2<-str_replace_all(geocode$municipio2, "õ", "o")
geocode$municipio2<-str_replace_all(geocode$municipio2, "ó", "o")
geocode$municipio2<-str_replace_all(geocode$municipio2, "ô", "o")
geocode$municipio2<-str_replace_all(geocode$municipio2, "'", "")

geocode$uf2<-tolower(geocode$uf)

#endereco
geocode$endereco<-paste(geocode$municipio2, geocode$uf2, "brasil", sep=", ")

#dividir em três partes
#parte 1
geocode1<-geocode[1:2400,]
latlon1<-geocode(geocode1$endereco)
gc1<-cbind(geocode1, latlon1)

write.csv(gc1, file="gc1.csv", row.names=FALSE)
gc1 <- read.csv("~/gc1.csv")

gc1$populacao<-stri_replace_all_fixed(gc1$populacao, "(1)", "")
gc1$populacao<-stri_replace_all_fixed(gc1$populacao, "(2)", "")
gc1$populacao<-stri_replace_all_fixed(gc1$populacao, "(3)", "")
gc1$populacao<-stri_replace_all_fixed(gc1$populacao, ".", "")
gc1$populacao<-as.numeric(gc1$populacao)

##parte 2
geocode2<-geocode[2401:4800,]
latlon2<-geocode(geocode2$endereco)
gc2<-cbind(geocode2, latlon2)
write.csv(gc2, file="gc2.csv", row.names=FALSE)
gc2 <- read.csv("~/gc2.csv")

gc2$populacao<-stri_replace_all_fixed(gc2$populacao, "(1)", "")
gc2$populacao<-stri_replace_all_fixed(gc2$populacao, "(2)", "")
gc2$populacao<-stri_replace_all_fixed(gc2$populacao, "(3)", "")
gc2$populacao<-stri_replace_all_fixed(gc2$populacao, ".", "")
gc2$populacao<-as.numeric(gc2$populacao)


##parte 3
geocode3<-geocode[4801:5570,]
latlon3<-geocode(geocode3$endereco)
gc3<-cbind(geocode3, latlon3)
write.csv(gc3, file="gc3.csv", row.names=FALSE)
gc3 <- read.csv("~/gc3.csv")

gc3$populacao<-stri_replace_all_fixed(gc3$populacao, "(1)", "")
gc3$populacao<-stri_replace_all_fixed(gc3$populacao, "(2)", "")
gc3$populacao<-stri_replace_all_fixed(gc3$populacao, "(3)", "")
gc3$populacao<-stri_replace_all_fixed(gc3$populacao, ".", "")
gc3$populacao<-as.numeric(gc3$populacao)

##rbind
geocode_final<-full_join(gc1, gc2, by=c("cod_ibge","endereco","lat","lon","populacao","id","ano","uf","uf2","municipio", "municipio2"))
geocode_final<-full_join(geocode_final, gc3, by=c("cod_ibge","endereco","lat","lon","populacao","id","ano","uf","uf2","municipio", "municipio2"))

brazil<-c(-51.92528, -14.235)

br<-get_map(brazil, source="google", maptype="terrain", zoom=4)

geocode_final$latmed<-(as.numeric(geocode_final$lat)*as.numeric(geocode_final$populacao))/sum(as.numeric(geocode_final$populacao), na.rm=TRUE)
geocode_final$lonmed<-(as.numeric(geocode_final$lon)*as.numeric(geocode_final$populacao))/sum(as.numeric(geocode_final$populacao), na.rm=TRUE)
#Centro geográfico
px<-sum(geocode_final$lonmed, na.rm=TRUE)
py<-sum(geocode_final$latmed, na.rm=TRUE)
pxy<-data.frame(px, py)

br %>%
  ggmap() + geom_point(data=geocode_final, aes(lon, lat), 
                       size=as.numeric(geocode_final$populacao)/250000, alpha=0.3) +
  geom_point(data=pxy, aes(px, py), size=5, color="red", shape="+", stroke=7)


######

#POPULACAO FINAL#
geocode_final$lat[533]<-(-5.408827)
geocode_final$lon[533]<-(-44.328925)
geocode_final$lat[3975]<-(-25.571170)
geocode_final$lon[3975]<-(-52.045072)

geocode_final<-geocode_final[c(5,10,11)]

populacao_mapa<-full_join(populacao, geocode_final, by="cod_ibge")


##LOOP
anos<-c(1872,1890, 1910, 1920, 1940, 1950, 1960, 1970, 1980:2016)
populacao_mapa$populacao<-stri_replace_all_fixed(populacao_mapa$populacao, "(1)", "")
populacao_mapa$populacao<-stri_replace_all_fixed(populacao_mapa$populacao, "(2)", "")
populacao_mapa$populacao<-stri_replace_all_fixed(populacao_mapa$populacao, "(3)", "")
populacao_mapa$populacao<-stri_replace_all_fixed(populacao_mapa$populacao, ".", "")
populacao_mapa$populacao<-as.numeric(populacao_mapa$populacao)


for (i in 1:47){
dfmap<- populacao_mapa %>%
   filter(ano==anos[i])

dfmap$latmed<-(as.numeric(dfmap$lat)*as.numeric(dfmap$populacao))/sum(as.numeric(dfmap$populacao), na.rm=TRUE)
dfmap$lonmed<-(as.numeric(dfmap$lon)*as.numeric(dfmap$populacao))/sum(as.numeric(dfmap$populacao), na.rm=TRUE)
px<-sum(dfmap$lonmed, na.rm=TRUE)
py<-sum(dfmap$latmed, na.rm=TRUE)
pxy<-data.frame(px, py)

br %>%
  ggmap() + geom_point(data=dfmap, aes(lon, lat), size=(dfmap$populacao/250000), alpha=0.3) +
  geom_point(data=pxy, aes(px, py), size=5, color="red", shape="+", stroke=7) + 
  labs(x="", y="", title=anos[i], subtitle="População das cidades brasileiras") 
ggsave(plot=last_plot(), filename = paste("mapa", anos[i], ".png", sep=""), dpi=500)
}



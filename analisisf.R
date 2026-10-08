library(tidyverse)
library(stringi)
library(readxl)
library(readr)
library(tmaptools)
library(ggmap)
library(rworldmap)
library(sf)
library(rworldxtra)
library(tmap)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
columnerror<-function(file, encod, skip=3){
  tryCatch(
    expr = {
      read.csv(file, fileEncoding = encod, encoding = "UTF-8", skip=skip, na.strings = c("NA", "N/A", ""))
      return(TRUE) # If the file is read successfully, return TRUE
    },
    error = function(e) {
      message(paste("Error reading file:", file))
      return(FALSE)
    }
  )
}

#####0. Extra cleaning####
inter<-read_csv("../Rpnt/intersections2.csv")
posiciones<-read_csv("posiciones2.csv")
posiciones<-posiciones %>% 
  distinct()
inter<-inter %>% left_join(posiciones)

posics<- inter %>% subset(!is.na(posic2)|!is.na(status2)) %>%
  group_by(denominacion, sexo) %>% summarize(n=n()) %>%
  pivot_wider(names_from=sexo, values_from=n, values_fill = 0) %>%
  mutate(perfem=mujer/(mujer+hombre)) %>% subset(select=c(denominacion, perfem)) %>%
  distinct() 
posics2<-left_join(posiciones, posics)
posiciones2<-inter %>% subset(!is.na(posic2)|!is.na(status2)) %>%
  group_by(denominacion, status, posicion, status2, posic2) %>% summarize(n=n()) %>%
  arrange(desc(n))
write_csv(posiciones2, "posiciones2.csv")
write_csv(posics2, "temp.csv")
write_csv(inter, "intersections2.csv")

total<-read_csv("../Rpnt/totalfinal.csv")
total<-total %>% select(-c(posicion, status))
total<-total %>% left_join(posiciones)
academiques<-total %>% subset(!is.na(status2)|!is.na(posic2)|snii==1)
write_csv(academiques, "academiques.csv")
write_csv(total, "total2.csv")
####correcion genero####
total<-read_csv("total2.csv") %>% 
  mutate(sexo=ifelse(str_detect(sexo, "femenino"), "mujer", 
                     ifelse(str_detect(sexo, "masculino"), "hombre",sexo)))
write_csv(total, "total2.csv")
academiques<-read_csv("academiques.csv") %>% 
  mutate(sexo=ifelse(str_detect(sexo, "femenino"), "mujer", 
                     ifelse(str_detect(sexo, "masculino"), "hombre",sexo)))
write_csv(academiques, "academiques.csv")
inter<-read_csv("intersections2.csv") %>% 
  mutate(sexo=ifelse(str_detect(sexo, "femenino"), "mujer", 
                     ifelse(str_detect(sexo, "masculino"), "hombre",sexo)))
write_csv(inter, "intersections2.csv")

#####1.1 Tendencia secihti#####
inpc<-read_csv("../Rpnt/inpc.csv") %>% rename(ejercicio=year)
umas<-read_csv("../Rsecihti/umas.csv")
seci<-read_csv("../Rsecihti/secihti3.csv")
seci$nivel<-ifelse(str_detect(seci$nivel, "DOC$"), "DOCTORADO", 
                   ifelse(str_detect(seci$nivel, "MAESTRIA$|MAE$"), "MAESTRIA", 
                          ifelse(str_detect(seci$nivel, "ESPECIALI$|ESP$"), "ESPECIALIDAD", 
                                 ifelse(str_detect(seci$nivel, "\\sESTANCIA|EST$|EST\\sTEC"), "ESTANCIA TECNICA", 
                                        ifelse(str_detect(seci$convocatoria, "catedr"), "CATEDRA",
                                               ifelse(str_detect(seci$tipo, "repatria"), "REPATRIACION", 
                                                      seci$nivel)
                                        )
                                 )
                          )
                   )
)
sni<-read_csv("../Rsecihti/sni1.csv") 
sni<-left_join(sni, umas, by="year")
sni$estimulo<-ifelse(sni$nivel=="C", 3*sni$valor, 
                     ifelse(sni$nivel=="1", 6*sni$valor, 
                            ifelse(sni$nivel=="2", 8*sni$valor, 
                                   ifelse(str_detect(sni$nivel, "3|E"), 14*sni$valor, NA))))
####1.2 tendencia empleo####
total<-read_csv("total2.csv")
inter<-read_csv("intersections2.csv")
total$est_real[is.na(total$est_real)]<-0
total$sni_real[is.na(total$sni_real)]<-0
inter$est_real[is.na(inter$est_real)]<-0
inter$sni_real[is.na(inter$sni_real)]<-0
ordenst<-data.frame(
  status2=c("asistente", "hora/clase", "asociado/interino",  "medio tiempo", "carrera","titular"), 
  orden=c(1:6)
)
ordenps<-data.frame(
  posic2=c("administrativo/a", "tecnico/a", "profesor/a", "investigador/a", "funcionario/a"), 
  orden=c(1:5)
)
######1.2.1 sexo######
empleos<-inter %>% 
  subset(!is.na(status2)|!is.na(posic2), select=c(simp2, desam, status2, posic2, areaphd, sexo)) %>% 
  distinct()
tablastatus<- inter %>% subset(!is.na(status2)&!is.na(sexo)) %>%
  count(status2, sexo) %>% group_by(status2) %>%
  mutate(rel=n/sum(n))  %>%
  left_join(ordenst)
tablaposiciones<- inter %>% subset(!is.na(posic2)&!is.na(sexo)) %>%
  mutate(posic2=str_replace_all(posic2, "o$", "o/a")) %>%
  mutate(posic2=str_replace_all(posic2, "r$", "r/a")) %>%
  count(posic2, sexo) %>% group_by(posic2) %>%
  mutate(rel=n/sum(n))  %>%
  left_join(ordenps)
tablanombra<- empleos %>% subset(!is.na(sexo)) %>%
  mutate(posic2=str_replace_all(posic2, "o$", "o/a")) %>%
  mutate(posic2=str_replace_all(posic2, "r$", "r/a")) %>%
  mutate(nombra=paste(posic2, status2)) %>%
  count(nombra, sexo) %>% group_by(nombra) %>%
  mutate(rel=100*n/sum(n))%>% subset(n>50&sexo=="mujer", select=-sexo) %>%
  mutate(nombra=str_replace_all(nombra, "\\s*NA\\s*", "")) 
tablafem<-inter %>% subset(!is.na(sexo)&!is.na(areagrd), select=c(areagrd, sexo, simp2)) %>%
  distinct() %>%
  count(sexo, areagrd) %>% group_by(areagrd) %>%
  mutate(rel=n/sum(n))  %>%
  nest()
for(i in 1:nrow(tablafem)){
  tablafem$data[[i]]$orden<-tablafem$data[[i]]$rel[which(str_detect(tablafem$data[[i]]$sexo, "mujer"))]
}
tablafem<- tablafem %>% unnest(cols = data)

jpeg("../../posdocsf/statussexo.jpg")
tablastatus %>%
  ggplot(aes(x=fct_reorder(status2, orden), y=rel*100, fill=sexo, label=round(rel*100, 0))) + geom_col(position="stack")+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Estatus", y="Porcentaje", fill="Sexo", title="Distribución de sexo en estatus") +
  geom_text(position = position_stack(vjust = .5))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
jpeg("../../posdocsf/posicionsexo.jpg")
tablaposiciones %>%
  ggplot(aes(x=fct_reorder(posic2, orden), y=rel*100, fill=sexo, label=round(rel*100, 0))) + geom_col(position="stack")+
  scale_fill_brewer(palette="Dark2")+
  geom_text(position = position_stack(vjust = .5))+
  labs(x="Nombramiento", y="Porcentaje", fill="Sexo", title="Distribución de sexo en nombramientos")
dev.off()
jpeg("../../posdocsf/nombramientosexo.jpg")
tablanombra %>%
  ggplot(aes(x=fct_reorder(nombra, rel), y=rel, fill=rel)) + geom_col(position="stack")+
  coord_flip()+ scale_fill_viridis_c(option = "magma")+
  labs(x="nombramientos", y="Proporción", fill="Porcentaje mujer", title="Distribución de sexo en nombramientos")
dev.off()
jpeg("../../posdocsf/feminizacion.jpg")
tablafem %>% subset(n>5)%>%
  ggplot(aes(x=fct_reorder(areagrd, orden), y=100*rel, fill=sexo, label=round(rel*100, 0))) + geom_col(position="stack")+
  scale_fill_brewer(palette="Dark2")+
  geom_text(position = position_stack(vjust = .5))+
  labs(x="Status", y="Proporción", fill="Sexo", title="Distribución de sexo en areas de grado")
dev.off()

######1.2.4 cambios genero#####
cmba<- inter %>% subset(!is.na(sexo)) %>%
  mutate(cambio=ifelse(simp2==lead(simp2)&desam==lead(desam)&institucion!=lead(institucion), 1, 0)) %>%
  group_by(simp2, desam, sexo) %>%  summarize(cambio=sum(cambio, na.rm=T)) %>% 
  group_by(sexo) %>% summarize(cambio=mean(cambio, na.rm=T), tipo="institucion")
cmbb<- inter %>% subset(!is.na(sexo)) %>%
  mutate(cambio=ifelse(simp2==lead(simp2)&desam==lead(desam)&areasni!=lead(areasni), 1, 0)) %>%
  group_by(simp2, desam, sexo) %>%  summarize(cambio=sum(cambio, na.rm=T)) %>% 
  group_by(sexo) %>% summarize(cambio=mean(cambio, na.rm=T), tipo="areasni")
cmbc<- inter %>% 
  subset((!is.na(status2)|!is.na(posic2))&!is.na(sexo), select=c(simp2, desam, status2, posic2, areaphd, sexo)) %>% 
  mutate(nombra=paste(posic2, status2) %>% str_replace_all("\\s*NA\\s*", "")) %>%
  mutate(cambio=ifelse(simp2==lead(simp2)&desam==lead(desam)&nombra!=lead(nombra), 1, 0)) %>%
  group_by(simp2, desam, sexo) %>%  summarize(cambio=sum(cambio, na.rm=T)) %>% 
  group_by(sexo) %>% summarize(cambio=mean(cambio, na.rm=T), tipo="nombramiento")
cambios<-rbind(cmba, cmbb, cmbc) %>%
  pivot_wider(names_from=sexo, values_from=cambio) %>%
  mutate(brecha=100*(hombre-mujer)/mujer)
write_csv(cambios, "../../posdocsf/cambiosgenero.csv")

######1.2.6 cambios nivel sni######
sni<-read_csv("../Rsecihti/sni1.csv")
sni$simpname<-paste0(sni$apell_stand%>% trimws(which = "both"), 
                     ", ", sni$nom_stand%>% trimws(which = "both")) %>% tolower() %>%
  str_replace_all("[^[:alpha:],[']]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")
nombres1<-read_csv("intersections2.csv") %>% 
  subset(!is.na(sexo)&!is.na(cvu), select=c(cvu, sexo)) %>% distinct()
nombres2<-read_csv("total2.csv") %>% 
  subset(!is.na(sexo)&!is.na(cvu), select=c(cvu, sexo)) %>% distinct()
nombres<-rbind(nombres1, nombres2) %>% distinct()
sni<- sni %>% left_join(nombres, by="cvu", multiple="first") %>%
  arrange(nombre, year, cvu) %>%
  mutate(nivel=ifelse(str_detect(nivel, "Emerito"), "E", nivel))%>%
  group_by(nombre) %>% fill(cvu, .direction="downup")%>%
  arrange(nombre, year, cvu) %>%
  group_by(nombre) 

genero0<-read_csv("total2.csv") %>% subset(!is.na(sexo)&!is.na(nombre), select=c(apellido1, apellido2, nombre, sexo)) %>% distinct()
genero1<- genero0 %>% subset(select=c(nombre, sexo)) %>% count(nombre, sexo)
genero2<- genero1 %>% mutate(nombre=str_extract(nombre, "^\\w+")) %>% 
  subset(nchar(nombre)>2) %>% group_by(nombre, sexo)%>%summarize(n=sum(n), .groups="keep")
genero3<- genero1 %>% mutate(nombre=str_extract(nombre, "(?<=\\s)\\w+$")) %>% 
  subset(!is.na(nombre)&nchar(nombre)>2) %>% group_by(nombre, sexo)%>%summarize(n=sum(n), .groups="keep")
genero1<- genero1 %>%
  pivot_wider(names_from=sexo, values_from=n, values_fill=0) %>%
  mutate(femprob=mujer/(mujer+hombre), mascprob=hombre/(mujer+hombre)) %>%
  mutate(fem=ifelse(femprob>mascprob, "mujer", 
                    ifelse(femprob<mascprob, "hombre", NA))) %>%
  subset(femprob>0.7|mascprob>0.7, select=c(nombre, fem))
genero2<-genero2 %>% 
  pivot_wider(names_from=sexo, values_from=n, values_fill=0) %>%
  mutate(femprob=mujer/(mujer+hombre), mascprob=hombre/(mujer+hombre)) %>%
  mutate(fem=ifelse(femprob>mascprob, "mujer", 
                    ifelse(femprob<mascprob, "hombre", NA))) %>%
  subset(femprob>0.7|mascprob>0.7, select=c(nombre, fem))
genero3<-genero3 %>% 
  pivot_wider(names_from=sexo, values_from=n, values_fill=0) %>%
  mutate(femprob=mujer/(mujer+hombre), mascprob=hombre/(mujer+hombre)) %>%
  mutate(fem=ifelse(femprob>mascprob, "mujer", 
                    ifelse(femprob<mascprob, "hombre", NA))) %>%
  subset(femprob>0.85|mascprob>0.85, select=c(nombre, fem))
colnames(genero0)<-c("apellido1", "apellido2", "nombre", "fem")
colnames(genero1)<-c("nombre", "fem")
colnames(genero2)<-c("nombre1", "fem")
colnames(genero3)<-c("nombre2", "fem")
sni$simpname<-paste0(sni$apell_stand%>% trimws(which = "both"), 
                     ", ", sni$nom_stand%>% trimws(which = "both")) %>% tolower() %>%
  str_replace_all("[^[:alpha:],[']]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")
snina<-sni %>% subset(is.na(sexo)) %>%
  mutate(nombre=nom_stand%>% str_to_lower() %>%str_squish(), 
         apellido1=str_extract(apell_stand, "^\\w+") %>% str_to_lower() %>%str_squish(), 
         apellido2=str_extract(apell_stand, "(?<=\\s)\\w+$") %>% str_to_lower() %>%str_squish(),
         nombre1=str_extract(nom_stand, "(?<=\\s)\\w+") %>% str_to_lower() %>%str_squish(),
         nombre2=str_extract(nom_stand, "(?<=\\s)\\w+(?=\\s)") %>% str_to_lower() %>%str_squish()) %>%
  select(cvu, simpname, apellido1, apellido2, nombre, nombre1, nombre2, sexo) %>% distinct()
snina<-snina %>% 
  left_join(genero0) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-fem) %>%
  left_join(genero1) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-fem) %>%
  left_join(genero2) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-fem) %>%
  left_join(genero3) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-c(fem, nombre1, nombre2))
snina<-snina %>% subset(!is.na(sexo), select=c(cvu, simpname, sexo)) %>% rename(sexop=sexo) %>% distinct()
snina$nombre<-NULL
sniname<-snina %>% subset(is.na(cvu), select=c(simpname, sexop)) %>% distinct() %>% rename(sexop2=sexop)
snicvu<-snina %>% subset(!is.na(cvu), select=c(cvu, sexop)) %>% distinct()
sni<- sni %>% 
  left_join(snicvu, multiple="first") %>% mutate(sexo=ifelse(is.na(sexo), sexop, sexo)) %>% select(-sexop) %>%
  left_join(sniname, multiple="first") %>% mutate(sexo=ifelse(is.na(sexo), sexop2, sexo)) %>% select(-sexop2)

ordensni<-data.frame(
  nivel=c("C", "1", "2", "3", "E"), 
  orden=c(1:5)
) 
sni$year<-as.numeric(sni$year)
sni2<-sni %>% left_join(ordensni)%>%
  subset(!is.na(cvu)&!is.na(orden)) %>%
  left_join(ordensni) %>% arrange(cvu, year) %>%
  mutate(orin=ifelse(orden!=lag(orden)&cvu==lag(cvu), lag(nivel), NA), 
         orio=ifelse(orden!=lag(orden)&cvu==lag(cvu), lag(orden), NA))

cambiosn<-sni2 %>% subset(!is.na(orin)&!is.na(sexo)) %>%
  arrange(nombre, year, cvu) %>%
  group_by(nombre)%>% 
  mutate(orin2=ifelse(cvu==lag(cvu), lag(orin), NA), 
         yearc1=ifelse(cvu==lag(cvu), lag(year), NA)) %>%
  subset(!is.na(orin2), select=c(cvu, year, yearc1, nivel, orin, orin2, sexo)) %>%
  mutate(tiempoc=year-yearc1)
tablacambiosn<-cambiosn %>%
  group_by(orin, nivel, sexo) %>%
  summarize(n=n(), tiempoprom=mean(tiempoc, na.rm=T)) %>%
  arrange(orin, nivel) %>% subset(!is.na(sexo)) %>%
  left_join(ordensni, by=c("orin"="nivel")) %>% rename(orinorden=orden) %>%
  left_join(ordensni, by=c("nivel"="nivel")) %>% rename(nivelorden=orden) %>%
  group_by(orin, nivel) %>%
  mutate(prop=n/sum(n))
distribucion<-sni %>%
  subset(!is.na(sexo)&!is.na(nivel)&year>=2015) %>%
  group_by(simpname) %>% 
  subset(select=c(simpname, nivel, sexo)) %>% distinct() %>%
  group_by(nivel, sexo) %>%
  summarize(n=n()) %>%
  group_by(nivel) %>%
  mutate(rel=100*n/sum(n)) %>%
  left_join(ordensni)
interunique<-inter %>% select(simpname) %>% distinct() %>% mutate(nuevo=1)
distrinuevos<- sni %>% 
  left_join(interunique) %>% subset(!is.na(nuevo)) %>%
  subset(!is.na(sexo)&!is.na(nivel)&year>=2015) %>%
  group_by(simpname) %>% 
  subset(select=c(simpname, nivel, sexo)) %>% distinct() %>%
  group_by(nivel, sexo) %>%
  summarize(n=n()) %>%
  subset(n>5)%>%
  group_by(nivel) %>%
  mutate(rel=100*n/sum(n)) %>%
  left_join(ordensni)
library(ggpattern)
jpeg("../../posdocsf/cambiosni.jpg", width=800, height=600)
tablacambiosn %>% subset(orinorden<nivelorden&n>100) %>%
  ggplot(aes(x=factor(paste(orin, "->", nivel)), y=tiempoprom, pattern=sexo, fill = 100*prop)) +
  geom_col_pattern(position="dodge")+
  scale_fill_continuous( trans="reverse")+
  labs(x="Cambio SNI", y="Tiempo promedio (años)", pattern="Sexo", title="Cambios de nivel SNII", 
       fill="% de cambios", subtitle="Tiempo promedio y distribución procentual")
dev.off()
jpeg("../../posdocsf/distribucionsnii.jpg", width=800, height=600)
distribucion %>%
  ggplot(aes(x=fct_reorder(nivel, orden), y=rel, fill=sexo, label=round(rel, 0))) + 
  geom_col(position="stack")+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Nivel SNI", y="Porcentaje", fill="Sexo", title="Distribución de sexo en asignaciones SNII", 
       subtitle="2015-2024")+
  geom_text(position = position_stack(vjust = .5))
dev.off()
jpeg("../../posdocsf/distribucionsniin.jpg", width=800, height=600)
distrinuevos %>%
  ggplot(aes(x=fct_reorder(nivel, orden), y=rel, fill=sexo, label=round(rel, 0))) + 
  geom_col(position="stack")+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Nivel SNI", y="Porcentaje", fill="Sexo", title="Distribución de sexo en asignaciones SNII", 
       subtitle="2015-2024")+
  geom_text(position = position_stack(vjust = .5))
dev.off()
####1.3 plazas####
academiques<-read_csv("academiques.csv")
academiques<-academiques %>% subset((!is.na(posic2)|!is.na(status2))&!is.na(institucion)&!str_detect(posic2, "admin"))
plazas<-academiques %>% count(ejercicio) %>% mutate(tipo="plazas") 
plazas$indice<-100*plazas$n/plazas$n[which(plazas$ejercicio==2019)]
plot(plazas$ejercicio, plazas$n)
graduados<-read_csv("total2.csv") %>% 
  subset(phd==1, select=c(simp2, desam, ejercicio)) %>% 
  group_by(simp2, desam) %>%
  mutate(yphd=max(ejercicio)) %>% select(-ejercicio) %>%
  distinct()
grads<- graduados %>% group_by(yphd) %>% summarize(n=n()) %>% rename(ejercicio=yphd) %>%
  mutate(tipo="graduados")
grads$indice<-100*grads$n/grads$n[which(grads$ejercicio==2019)]
comparativo<-rbind(plazas, grads)
comparativo %>% subset(ejercicio>2017 & ejercicio<2023) %>%
  ggplot(aes(x=ejercicio, y=indice, color=tipo))+geom_point()+geom_line()+
  labs(y="Indice 2019=100", title="Tendencia entre graduados SECIHTI y ")

#####1.3 ingresos######
######1.3.1 general#######
total2<-total %>% group_by(simp2, desam, ejercicio) %>%
  mutate(espe=max(espe), mae=max(mae), phd=max(phd), pos=max(pos), rep=max(rep), cate=max(cate)) %>%
  mutate(nivel2=nivel, 
         grd=espe+mae+phd+pos+rep+cate) %>%
  group_by(simp2, desam, ejercicio) %>%
  fill(nivel2, .direction="downup")
ingresos1a<- total2 %>% subset(grd>0&trab==0) %>%
  group_by(nivel) %>% 
  summarize(beca=mean(mensual_real,na.rm=T), 
            sni=mean(sni_real, na.rm=T), n=n()) 
ingresos1b<-total2 %>% subset(grd>0&trab==1) %>%
  group_by(nivel2) %>% 
  summarize(salario=sum(mensual_real,na.rm=T), 
            estimulos=sum(est_real,na.rm=T), 
            n2=n()) %>%
  rename(nivel=nivel2)
ingresos1<-left_join(ingresos1a, ingresos1b) %>%
  mutate(salario=salario/n, estimulos=estimulos/n,
         sni=ifelse(str_detect(nivel, "CATE|POSDOCTORA|REPATRI"), sni, 0))
ingresos2<-inter %>% group_by(simp2, desam, ejercicio) %>%
  mutate(espe=max(espe), mae=max(mae), phd=max(phd), pos=max(pos), rep=max(rep), cate=max(cate), 
         nivel2=nivel, 
         grd=espe+mae+phd+pos+rep+cate) %>%
  subset(grd==0&trab==1) %>%
  mutate(nivel="EMPLEO")%>% group_by(nivel) %>%
  summarize(salario=mean(mensual_real, na.rm=T), 
            estimulos=mean(est_real,na.rm=T), 
            sni=mean(sni_real, na.rm=T), beca=NA, n=n(), n2=NA) 
ingresos<- rbind(ingresos1, ingresos2) %>%
  pivot_longer(cols=-c(nivel, n, n2)) %>%
  subset(!is.na(value)&value>0)
nombres<-data.frame(
  nivel=c("LICENCIATURA", "ESPECIALIDAD", "ESTANCIA TECNICA", "MAESTRIA", "DOCTORADO", "POSDOCTORADO", "REPATRIACION", "CATEDRA", "EMPLEO"), 
  orden=c(0:8)
)
ingresos<-ingresos %>% left_join(nombres)
jpeg("../../posdocsg/tendenciaing.jpg", height=600, width=800)
ingresos %>% subset(nivel!="CATEDRA") %>%
  mutate(nivel = fct_reorder(nivel, orden)) %>% 
  ggplot( aes(x=nivel, fill=name, y=value)) + geom_col(position="stack") + 
  labs(x="Nivel", y="Pesos reales(2018=100)", fill="Tipo ingreso")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()
######1.3.2 genero#######
genero1<- total %>% subset(trab==0) %>%
  count(nivel, sexo) 
genero2<-inter %>% subset(trab==1) %>%
  count(sexo) %>% mutate(nivel="EMPLEO")
genero<- rbind(genero1, genero2) %>% group_by(nivel) %>% subset(!is.na(sexo))%>%
  mutate(relativo=100*n/sum(n))


genero<-genero %>% left_join(nombres)
genero$nivel<-paste(genero$orden, genero$nivel)
jpeg("../../posdocsf/tendenciasex.jpg", height=600, width=800)
genero %>% 
  ggplot( aes(x=nivel, fill=sexo, y=relativo, label=round(relativo,0))) + geom_col(position="stack") + 
  labs(x="Nivel", y="Porcentaje", fill="Sexo", title="Distribución de sexo en trayectorias")+ 
  scale_fill_brewer(palette="Dark2")+
  geom_text(position = position_stack(vjust = .5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

######1.3.3 ingresos y genero#####
ingresex1<- total %>% subset(trab==0) %>%
  mutate(posic2=str_replace_all(posic2, "r$", "r/a")) %>%
  mutate(posic2=str_replace_all(posic2, "o$", "o/a")) %>%
  subset(!is.na(sexo)) %>%
  group_by(nivel, sexo) %>%  
  summarize(ingreso=mean(mensual_real,na.rm=T)+mean(est_real,na.rm=T)+mean(sni_real, na.rm=T)) 
ingresex2<-inter %>% subset(trab==1) %>%
  subset(!is.na(sexo)) %>%
  group_by(sexo) %>%  
  summarize(ingreso=mean(mensual_real, na.rm=T)+mean(est_real,na.rm=T)+mean(sni_real, na.rm=T)) %>%
  mutate(nivel="Media")
ingresex3<- inter %>% subset(trab==1) %>%
  subset(!is.na(sexo)) %>%
  mutate(posic2=posic2 %>% str_to_title() %>%
           str_replace_all("r$", "r/a") %>%
           str_replace_all("o$", "o/a")) %>%
  group_by(posic2, sexo) %>%  
  summarize(ingreso=mean(mensual_real,na.rm=T)+mean(est_real,na.rm=T)+mean(sni_real, na.rm=T)) %>%
  subset(!is.na(ingreso)&ingreso>0&!is.na(posic2)) %>%
  rename(nivel=posic2) 

ingresex<- rbind(ingresex2, ingresex3) %>%
  subset(!is.na(ingreso)&ingreso>0&nivel!="CATEDRA") %>%
  pivot_wider(names_from=sexo, values_from=ingreso)
nombres<-data.frame(
  nivel=c("ESPECIALIDAD", "MAESTRIA", "DOCTORADO", "POSDOCTORADO", "REPATRIACION", "CATEDRA", "MEDIA",
          "Administrativo/a", "Tecnico/a", "Profesor/a", "Investigador/a", "Funcionario/a"), 
  orden=c(1:12)
)
ingresex<-ingresex %>% left_join(nombres) %>% subset(!is.na(orden))
ingresex$brecha<-100*(ingresex$hombre-ingresex$mujer)/ingresex$mujer
jpeg("../../posdocsf/tendenciaingsex.jpg", height=400, width=600)
ingresex %>% 
  ggplot( aes(x=fct_reorder(nivel, brecha), y=brecha, fill=brecha)) + geom_col() +
  scale_fill_gradient(low="salmon", high="firebrick4")+
  labs(x="Nivel", y="Porcentaje", fill="Brecha", title="Brecha de ingresos entre hombres y mujeres")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

####1.4 ingresos totales####
inter<-read_csv("intersections2.csv") 
inter$n<-NULL
chamba<-inter %>% subset(ejercicio>=yphd)
ingresoscham1<- chamba %>% subset(!str_detect(posicion, "becar")) %>%
  group_by(simp2, desam, ejercicio, sexo, areagrd, posic2, status2, snii) %>%
  summarize(
    salario=sum(mensual_real, na.rm=T), estimulos=sum(est_real,na.rm=T),
    sni=mean(sni_real, na.rm=T)  
  )
ingresoscham2<- chamba %>% subset(str_detect(posicion, "becar")) %>%
  group_by(simp2, desam, ejercicio, sexo, areagrd, posic2, status2) %>%
  summarize(
    beca=mean(mensual_real, na.rm=T))
ingresoscham<-left_join(ingresoscham1, ingresoscham2) %>%
  mutate(beca=ifelse(is.na(beca), 0, beca),
         total=salario+estimulos+sni+beca) %>%
  subset(total>500) %>% arrange(total) 
ingresoscham$orden<-rownames(ingresoscham)  

jpeg("../../posdocsf/distribucioning.jpg")
ingresoscham %>% subset(!is.na(sexo)) %>%
  rownames_to_column(var="orden2") %>%
  ggplot(aes(x=as.numeric(orden2), y=total, fill=sexo)) + geom_col()+
  labs(title="Ingresos de mayor a menor por sexo", y="pesos de 2018", x="Orden")
dev.off()

library(tidyverse)
library(ggthemes)
library(ggthemr)
library(mboost)
library(ggcorrplot)
ggthemr('flat')
dati<-read.csv("datiReport.csv", header = TRUE, sep=",",stringsAsFactors=TRUE)
dati<-as_tibble(dati)
#----------------------------analisi struttura dati e controllo NA-----------------------------------------
sum(is.na(dati))
str(dati)
summary(dati)
#---------------------- matrice di correlazione------------------------------------------------------------
model.matrix(~0+., data=dati) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)+     theme(text=element_text(size=20),axis.text.x = element_text(angle=90, hjust=1)) 

#---------------------------- rimozione variabili-----------------------------------------------------------
drop <- c("X","NAnnullYear","NRimbYear","NArtVis","NAccMonth","NresiYear")
dati <- dati[,!(names(dati) %in% drop)]
#-----------------------------Utilizzo database provvisorio di appoggio per i grafici------------------------
dati_modificati<- dati %>% 
  mutate(Professione=case_when(Profess%in% c("Imprenditore") ~ "Imprenditori",
                               Profess%in% c("Lib.prof") ~ "Lib.prof",
                               Profess%in% c("Pensionato", "Studente") ~ "Inattivi",
                               Profess%in% c("Impiegato","Operaio") ~ "Dipendenti"),
         Area=case_when(AreaG %in% c("Centro", "Nord") ~ "Centro/Nord",
                        AreaG %in% c("Sud") ~ "Sud",
                        AreaG %in% c("Isole") ~ "Isole"))

#-------------------------------------------------------------------------------------------------------------
#funzione per calcolare moda 
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
#-------------------------------------------------------------------------------------------------------------
#grafico Percentuale acquisti in base alle professioni
dati_modificati %>%
  group_by(Professione) %>% 
  summarise(Percentuale=n(), .groups = 'drop') %>% 
  mutate(Percentuale=Percentuale/sum(Percentuale)*100) %>%
  transform(Professione=reorder(Professione,-Percentuale))%>%
  ggplot(aes(Professione,Percentuale,fill=Professione))+
  geom_bar(stat="identity")
#-------------------------------------------------------------------------------------------------------------
#grafico spesa media in base alle professioni
dati_modificati%>%group_by(Professione)%>% 
  summarise(SpesaMedia=mean(TotOrd),.groups="drop")%>%  
  transform(Professione=reorder(Professione,SpesaMedia))%>%
  ggplot(aes(Professione,SpesaMedia,fill=Professione))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(SpesaMedia,0)), vjust = -0.5)
#-------------------------------------------------------------------------------------------------------------
#BoxPlot di TotOrd
dati_modificati %>% ggplot(aes(x=Professione,y=TotOrd,fill=Professione)) + geom_boxplot() + theme(legend.position = "none")
#-------------------------------------------------------------------------------------------------------------
#crea un df di appoggio in cui prima raggruppo per professione poi tramite summarise calcolo media moda e mediana e riordino i dati tramite
#riorganizzo i dati tramite la funzione gather
df_stats <-dati_modificati %>% 
  group_by(Professione) %>% 
  summarize(mean = mean(TotOrd), 
            median = median(TotOrd),
            moda=Mode(TotOrd), .groups = 'drop') %>%
  gather(key = Linee, value = value, (mean:median:moda))

#grafici con sia istogrammi che density plot che richiamano valori di moda media e mediana dal df creato in precedenza
dati_modificati %>% ggplot(aes(TotOrd, ..density..,fill=Professione)) +
  geom_histogram(binwidth = 2, color="black") +
  geom_density(alpha = 0.7,bw=3,)+
  facet_grid(Professione~.)+
  geom_vline(data = df_stats, aes(xintercept = value, color = Linee),size=1,5)
#-------------------------------------------------------------------------------------------------------------
#density plot fidelty in base alle professioni
dati_modificati %>% ggplot(aes(fidelity, ..density..,fill=Professione)) +
  geom_histogram(binwidth = 1, color="black") +
  facet_grid(Professione~.) +
  geom_density(alpha = 0.5,bw=3,)
#-------------------------------------------------------------------------------------------------------------
#sconto istogramma in base alle professioni, facet grid serve a plottare in verticale più grafici
dati_modificati %>% ggplot(aes(Sconto, ..density..,fill=Professione)) +
  geom_histogram(binwidth =0.2, color="black") +
  facet_grid(Professione~.)
#-------------------------------------------------------------------------------------------------------------
#Acquisti per area geografica, diagrammi "riempiti" con le professioni 
dati_modificati %>% group_by(Area,Professione)%>%
  summarise(count=n(), .groups = 'drop')%>%
  ggplot(aes(x=Area,y=(count/sum(count))*100,fill=Professione))+
  geom_bar(stat="identity",width = 0.5)+
  labs(x = "Area Geografica", y = "percent")
#-------------------------------------------------------------------------------------------------------------
#accessi alla parte del sito relativa all'abbigliamento in base alla professione 
dati_modificati%>% ggplot(aes(Professione,fill=Abbigl))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)
#-------------------------------------------------------------------------------------------------------------
#accessi alla parte del sito relativa alle calzature in base alla professione
dati_modificati%>% ggplot(aes(Professione,fill=Calz))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)
#-------------------------------------------------------------------------------------------------------------
#accessi alla parte del sito relativa all'Abbigliamento Sportivo in base alla professione
dati_modificati%>% ggplot(aes(Professione,fill=AbbSport))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)
#-------------------------------------------------------------------------------------------------------------
#accessi alla parte del sito relativa ai Libri in base alla professione
dati_modificati%>% ggplot(aes(Professione,fill=Books))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent)
#-------------------------------------------------------------------------------------------------------------
#dataset di chi visualizza sezione 'Books' con Media TotOrd, Fidelity, Sconto
df_Books<-dati_modificati %>% 
  filter(Books=="Yes")%>%
  summarise(MediaTotOrd=mean(TotOrd),MediaSconto=mean(Sconto),MediaFidelity=mean(fidelity))
#dataset di chi visualizza sezione 'Abbigl' con Media TotOrd, Fidelity, Sconto
df_Abb<-dati_modificati %>% 
  filter(Abbigl=="Yes")%>%
  summarise(MediaTotOrd=mean(TotOrd),MediaSconto=mean(Sconto),MediaFidelity=mean(fidelity))

#dataset di chi visualizza sezione 'AbbSport' con Media TotOrd, Fidelity, Sconto
df_AbbSport<-dati_modificati %>% 
  filter(AbbSport=="Yes")%>%
  summarise(MediaTotOrd=mean(TotOrd),MediaSconto=mean(Sconto),MediaFidelity=mean(fidelity))%>%
  arrange(MediaTotOrd)

#dataset di chi visualizza sezione 'Calz' con Media TotOrd, Fidelity, Sconto
df_Calz<-dati_modificati %>%
  filter(Calz=="Yes")%>%
  summarise(MediaTotOrd=mean(TotOrd),MediaSconto=mean(Sconto),MediaFidelity=mean(fidelity))%>%
  arrange(MediaTotOrd)
#unisco i 4 mini dataframe creati tramite la funzione bind rows che unisce le in base alle righe
df_finale<-bind_rows(df_Books,df_Abb,df_AbbSport,df_Calz)

#aggiungo colonna che specifica a quale sessione fanno riferimento i dati
df_finale["Sezione_Visitata"] <- c("Books_Yes","Abbigl_Yes","AbbSport_Yes","Calz_Yes")

#riordino il dataframe mettendo in prima posizione la colonna denominata 'Sezione Visitata'
df_finale <- df_finale[c(4,1,2,3)]

library(reshape2)
#tramite la funzione melt della libreria 'reshape2' modifico la struttura del dataframe in modo
#da avere i valori tutti nella stessa colonna, cosi tramite il 'fill' di ggplot potrò creare un diagramma
#a barre affiancate
df2 <- melt(df_finale, id.vars='Sezione_Visitata')
#diagramma a barre con MediaTotOrd,MediaSconto,MediaFidelity di chi ha acquistato ed è entrato nelle varie sessioni del sito
#tabella
df_finale
#diagramma a barre affiancate
ggplot(df2, aes(x=Sezione_Visitata, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')+coord_flip()
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#--------------------------------- PARTE RELATIVA AI MODELLI -------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#importazione dataset
dati<-read.csv("datiReport.csv", header = TRUE, sep=",",stringsAsFactors=TRUE)
dati<-as_tibble(dati)#dati as tibble
drop <- c("NAnnullYear","NRimbYear","NArtVis","NAccMonth","X","NresiYear")#vettore con variabili da rimuovere
dati <- dati[,!(names(dati) %in% drop)]#rimozione variabili
#Divisione tra training set e validation set(test)
train = sample_n(dati, nrow(dati)*0.66)
test = setdiff(dati, train)
#-------------------------------------------------------------------------------------------------------------
#qq plot TotOrd
dati %>%                        
  ggplot(aes(sample = scale(TotOrd))) +                     
  geom_qq() +                                               
  geom_abline() 
#------------------------------------------MODELLO GLM BOOSTING-------------------------------------------------------------------
#Boosting glm  
set.seed(32)
modello_glm <- glmboost(TotOrd ~ .,
                        data = train,
                        control = boost_control(center = TRUE,#boost control per segnalare che i dati devono
                                                trace = TRUE))#essere centrati

#numero ottimale di iterazioni boosting attraversil'AIC
mstop(aic <- AIC(modello_glm))

#coefficienti del modello lineare con mstop pari a 100 
coef(modello_glm[mstop(aic)])

#faccio predizione e calcolo mse ###
pred<-predict(modello_glm[mstop(aic)],test)

#mse L2
mse_boost_L2<-apply((test$TotOrd-pred)^2,2,mean)
#RMSE
rmse_boost_L2<-sqrt(apply((test$TotOrd-pred)^2,2,mean))
#--------------------------------------------MODELLO GAM BOOSTING-----------------------------------------------------------------
# boosting gams ##
#assimo che tutte le variabili hanno un andamento non lineare e vengono utilizzate le spline come base learner "bbs()" l'unico limite è che si utilizza una bbs per tutti i predittori
set.seed(123)
modello_gam<- gamboost(TotOrd ~., baselearner = "bbs", 
                       data = train,
                       control = boost_control(trace = TRUE))

cvm <- cvrisk(modello_gam)#scelgo il numero di iterazioni boosting tramite CV, tramite indice
#di AIC non sono riuscito poichè sorgevano problemi di allocazione della memoria

# predizione e test boost con gams 
pred <- predict(modello_gam[mstop(cvm)],test)#predizione con modello che utilizza n=mstop scelto tramite cv
mse_gam<-#mean squared error
  rmse_gam<-sqrt(apply((test$TotOrd-pred)^2,2,mean))#root mean squared error

# predizione e test boost con gams #
pred <- predict(modello_gam[mstop(cvm)],test)#predizione
mse_gam<-apply((test$TotOrd-pred)^2,2,mean)#calcolo mse
rmse_gam<-sqrt(apply((test$TotOrd-pred)^2,2,mean))#calcolo rmse
#------------------------------------ MODELLO LINEARE-------------------------------------------------------------------------
#modello Lineare
modello_lm <- lm(TotOrd ~., data = train) #alleno modello

pred_lm <- predict(modello_lm, newdata=test)#faccio la predizione sul test set

#MSE del modello lineare
mse_lm<-apply((test$TotOrd-pred)^2,2,mean)
#rmse del modello lineare
rmse_lm<-sqrt(apply((test$TotOrd-pred)^2,2,mean))
#------------------------------------------------------------------------------------------------------------------------------
summary(modello_lm)
#------------------------------------------------------------------------------------------------------------------------------
summary(modello_glm)
#------------------------------------------------------------------------------------------------------------------------------
RMSE <- c(rmse_boost_L2, rmse_gam, rmse_lm)#creo vettore con tutti rmse
MSE <- c(mse_boost_L2, mse_gam, mse_lm)#creo vettore 
df <- data.frame(RMSE, MSE)#cambio formato in data frame
df["Modello"] <- c("glmL2","Boosting_gam","Lineare")
df_modelli<- df[c(3,1,2)]#riordino colonne del df
df

summary_modello_l<-summary(modello_lm)

summary_modello_l$r.squared
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------MODELLI CLASSIFICAZIONE-------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidyverse)
library(ggplot2)
library(mboost)
dati<-read.csv("datiReport.csv", header = TRUE, sep=",",stringsAsFactors=TRUE)
dati <-as_tibble(dati)
#--------------------------------------------------------------------------------------------------------------
drop <- c("X","NAnnullYear","NRimbYear","NresiYear","NAccMonth","NArtVis")
dati <- dati[,!(names(dati) %in% drop)]
#-----------------------------TRAINING E TEST SET--------------------------------------------------------------
train = sample_n(dati, nrow(dati)*0.66)
test = setdiff(dati, train)
#--------------------------------------------------------------------------------------------------------------

#--------------------------------- FUNZIONI  ------------------------------------------------------------------
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
# ABBIGLIAMENTO Regressione Logistica
Abbigl_logistic <- glm(Abbigl ~ ., data = train, 
                       family = binomial(), trace = 0)

#--------------------------------------------------------------------------------------------------------------
#accuracy train:
accuracy_Abbigl_train<-accuracy(table(train$Abbigl,round(predict.glm(Abbigl_logistic, type="response"),0))/nrow(train))
#accuracy abbigliamento test:
accuracy_Abbigliamento_test<-accuracy(table(test$Abbigl,round(predict(Abbigl_logistic,type="response",test),0))/nrow(test))
#--------------------------------------------------------------------------------------------------------------
#summary modello:
summary(Abbigl_logistic)
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# ABB SPORTIVO Regressione Logistica
set.seed(42)
AbbSport_logistic <- glm(AbbSport ~ ., data = train, 
                         family = binomial(), trace = 0)
#----------------------------------------------------------------------------------------------------------
#accuracy train:
accuracy_AbbSport_train<-accuracy(table(train$AbbSport,round(predict.glm(AbbSport_logistic, type="response"),0))/nrow(train))
#accuracy test:
accuracy_AbbSport_test<-accuracy(table(test$AbbSport,round(predict(AbbSport_logistic,type="response",test),0))/nrow(test))
#----------------------------------------------------------------------------------------------------------
#summary modello:
summary(AbbSport_logistic)
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#Books Regressione Logistica

Books_logistic <- glm(Books ~ ., data = train, 
                      family = binomial(), trace = 0)
#----------------------------------------------------------------------------------------------------------
#valori finali:
accuracy_Books_train<-accuracy(table(train$Books,round(predict.glm(Books_logistic, type="response"),0))/nrow(train))
#accuracy test
accuracy_Books_test<-accuracy(table(test$Books,round(predict(Books_logistic,type="response",test),0))/nrow(test))
#----------------------------------------------------------------------------------------------------------
#summary modello:
summary(Books_logistic)
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
# CALZ Regressione Logistica
set.seed(42)
#Fit del modello sui training data
Calz_logistic <- glm(Calz ~ ., data = train, 
                     family = binomial(), trace = 0)
#----------------------------------------------------------------------------------------------------------
#accuracy train
accuracy_Calz_train<-accuracy(table(train$Calz,round(predict.glm(Calz_logistic, type="response"),0))/nrow(train))

#accuracy test
accuracy_Calz_test<-accuracy(table(test$Calz,round(predict(Calz_logistic,type="response",test),0))/nrow(test))
#----------------------------------------------------------------------------------------------------------
#summary modello:
summary(Calz_logistic)
#----------------------------------------------------------------------------------------------------------
#---------------------------------PREDIZIONI CLASSIFICAZIONE-----------------------------------------------

#creo vettore per accuracy test 
AccuracyTest<- c(accuracy_Abbigliamento_test, accuracy_AbbSport_test, accuracy_Books_test,accuracy_Calz_test)

#creo vettore per accuracy train
AccuracyTrain<-c(accuracy_Abbigl_train,accuracy_AbbSport_train,accuracy_Books_train,accuracy_Calz_train)

#inserisco i due vettori in un nuovo dataframe
df_RisultatiClassificazione <- data.frame(AccuracyTest,AccuracyTrain)

#aggiungo colonna con caratteri che specificano l'area del sito a cui si fa riferimento
df_RisultatiClassificazione["Area_Sito"] <- c("Abbigliamento","Abbigliamento Sportivo","Books","Calzature")

#riordino colonne del dataset creato
df_RisultatiClassificazione<- df_RisultatiClassificazione[c(3,2,1)]

#trasformo in formato tibble
df_RisultatiClassificazione<-as_tibble(df_RisultatiClassificazione)

#output tabella
df_RisultatiClassificazione
#-----------------------------------------------------------------------------------------------------------------
summary(modello_lm)#summary per commenti finali
#-----------------------------------------------------------------------------------------------------------------
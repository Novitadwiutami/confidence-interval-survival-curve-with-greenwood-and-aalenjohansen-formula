library(readxl)
data=read_xlsx("E:/ITS/MATERI KULIAH/SEMESTER 8/RUN 1/data koding revisi.xlsx")
View(data)
library(survival)
library(survminer)
library(ggplot2)
library(KMsurv)
library(dplyr)
library(GGally)
library(ggfortify)
#Fit survival data using the Kaplan-Meier method 
surv_object <- Surv(time = data$time, event = data$Event)
summary(surv_object)
plot(surv_object)
autoplot(survfit(Surv(time, Event) ~ 1, data = data), surv.colour = 'blue', censor.colour = 'red',conf.int=FALSE)


fit.coxph <- coxph(Surv(time, Event)~Status_Pernikahan+Usia+Stadium+Surgery+Radiotherapy+Chemotherapy+Other_Treatment+Hormonal+Terapi_Hormonal+Riwayat_Kanker_Keluarga,data=data)
fit.coxph


#KM for variabel Status hubungan seksual
fit1 <- survfit(surv_object~status_hubungan_seksual, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
pl2 <- pl2 + conf.int=TRUE
survdiff(formula=surv_object~status_hubungan_seksual,data=data, rho=0)

ggsurvplot(fit1, data = data, pval = TRUE, conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Usia
fit1 <- survfit(surv_object~Usia, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
pl2 <- pl2 + conf.int=TRUE
survdiff(formula=surv_object~Usia,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE, conf.int = TRUE,ggtheme = theme_gray())


#KM for variabel Stadium
fit1 <- survfit(surv_object~Stadium, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Stadium,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Surgery
fit1 <- survfit(surv_object~Surgery, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Surgery,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Radiotherapy
fit1 <- survfit(surv_object~Radiotherapy, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Radiotherapy,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Chemotherapy
fit1 <- survfit(surv_object~Chemotherapy, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Chemotherapy,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Other Treatment
fit1 <- survfit(surv_object~Other_Treatment, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Other_Treatment,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Hormonal
fit1 <- survfit(surv_object~Hormonal, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Hormonal,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Terapi Hormonal
fit1 <- survfit(surv_object~Terapi_Hormonal, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Terapi_Hormonal,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Riwayat Kanker Keluarga
fit1 <- survfit(surv_object~Riwayat_Kanker_Keluarga, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
survdiff(formula=surv_object~Riwayat_Kanker_Keluarga,data=data, rho=0)
ggsurvplot(fit1, data = data, pval = TRUE,conf.int = TRUE,ggtheme = theme_gray())

#KM for variabel Karakteristik Sel
fit1 <- survfit(surv_object~Karakteristik_Sel, data = data,conf.type="plain") 
summary(fit1)
(pl2 <- ggsurv(fit1))
ggsurvplot(fit1, data = data, pval = TRUE)

#BARCHART
ggplot(data, aes(x = Stadium)) + 
  geom_bar(fill = "dodgerblue", color = "grey40", alpha = .5) 
  
ggplot(data, aes(x = Stadium)) +
  geom_bar(width = 0.5, fill = "dodgerblue") +
  labs(x = "Stadium", y = "Jumlah") +
  theme(plot.title = element_text(hjust = 0.5)) 

#USE THIS
df <- data.frame(Stadium=c("1","2","3","4"),
                 jumlah=c(21,41,38,17))
ggplot(data=df, aes(x=Stadium, y=jumlah)) +
  geom_bar(stat="identity", fill="dodgerblue",width = 0.7,color = "grey40", alpha = .7)+
  geom_text(aes(label=jumlah), vjust=-0.3, color="black", size=3.5)
#USE THIS
df <- data.frame(Status=c("Lajang","Menikah","Janda"),
                 jumlah=c(4,86,27))
ggplot(data=df, aes(x=Status, y=jumlah)) +
  geom_bar(stat="identity", fill="dodgerblue",width = 0.7,color = "grey40", alpha = .7)+
  geom_text(aes(label=jumlah), vjust=-0.3, color="black", size=3.5)


#PIECHART
x <-  c(55,45)
lbs <-  c("Pre Menopause","Post Menopause")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

x <-  c(53,47)
lbs <-  c("<50th",">50th")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

x <-  c(24,93)
lbs <-  c("Ya","Tidak")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

x <-  c(4,113)
lbs <-  c("Ya","Tidak")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

x <-  c(6,111)
lbs <-  c("Ya","Tidak")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

x <-  c(2,98)
lbs <-  c("Ya","Tidak")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

x <-  c(2,98)
lbs <-  c("Tidak","Pernah")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

statdes awal
x <-  c(17,83)
lbs <-  c("Metastasis","Tersensor")
piepercent<- round(x/sum(x)*100)
lbs<-paste(lbs,piepercent)
lbs<-paste(lbs,"%",sep = " ")
pie(x, labels = lbs, edges = 100, radius = 1,
    col = c("blue","violetred3"), border = NULL,
    lty = NULL, main = NULL, )

library("lavaan")
library("qgraph")
library("semPlot")
library("haven")
library("semTools") 
library(readxl)
Data_Trust <- read_excel("TSModeling_Dataset.xlsx")


#reverse columns

reverse_cols = c("ts1" ,"ts2" , "ts3" , "ts4" , "ts6" , "ts8" , "ts13"
                 , "ts17" , "ts18" , "ts19",  "ts20" , "ts21")
Data_Trust[ , reverse_cols] = 6 - Data_Trust[ , reverse_cols]



#CFA_Unimodal
Model_Uni<-'TrustinScienceScientist =~ ts1 + ts2 + ts3 + ts4 + ts5 + ts6 + ts7
                                     + ts8 + ts9 + ts10 + ts11 + ts12 + ts13
                                     +ts14 + ts15 + ts16 + ts17 + ts18 +ts19 + ts20 + ts21'
FitUni <- cfa(Model_Uni, data = Data_Trust, std.lv = TRUE)
summary(FitUni,fit.measures=TRUE,standardized=TRUE)
modindices(FitUni)


#CFA_SimpleStructure
Model_Simple<-'TrustinScience =~ ts3 + ts12 + ts14 + ts15 + ts21
            TrustinScientist =~ ts1 + ts2 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts13 + ts16 + ts17 + ts18 + ts19 + ts20 '
FitSimple <- cfa(Model_Simple, data = Data_Trust)
summary(FitSimple,fit.measures=TRUE,standardized=TRUE)
modindices(FitSimple)


#CFA_SecondOrder
Model_SecondOrder <- '
  TrustinScience =~ ts3 + ts12 + ts14 + ts15 + ts21 
  TrustinScientist =~ ts1 + ts2 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts13 + ts16 + ts17 + ts18 + ts19 + ts20 
  TrustGeneral =~ TrustinScience + TrustinScientist'
Fit_SecondOrder<- cfa(Model_SecondOrder, data = Data_Trust)
summary(Fit_SecondOrder,fit.measures=TRUE,standardized=TRUE)
modindices(Fit_SecondOrder)


#NOTE - second order model not identified and some lv variances negative -constrained model as noted below

Model_SecondOrder <- '
  TrustinScience =~ ts3 + ts12 + ts14 + ts15 + ts21 
  TrustinScientist =~ ts1 + ts2 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts13 + ts16 + ts17 + ts18 + ts19 + ts20 
  TrustGeneral =~ TrustinScience + TrustinScientist
  
  #Fixed variance of higher-order factor
  TrustGeneral~~1*TrustGeneral
  
  #Ensured positiive residual variances
  TrustinScience~~ 0.05*TrustinScience
  TrustinScientist~~ 0.05*TrustinScientist
'
Fit_SecondOrder<- cfa(Model_SecondOrder, data = Data_Trust)
summary(Fit_SecondOrder,fit.measures=TRUE,standardized=TRUE)
modindices(Fit_SecondOrder)



#CFA_BiFactor 
Model_Bi<-'TrustinScienceScientist =~ ts1 + ts2 + ts3 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts12 + ts13 + ts14 + ts15 + ts16 + ts17 + ts18 + ts19 + ts20 + ts21
           TrustinScience =~ ts3 + ts12 + ts14 + ts15 + ts21
           TrustinScientist =~ ts1 + ts2 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts13 + ts16 + ts17 + ts18 + ts19 + ts20
           TrustinScienceScientist ~~ 0*TrustinScience
           TrustinScienceScientist ~~ 0*TrustinScientist
           TrustinScience ~~ 0*TrustinScientist'
FitBi <- cfa(Model_Bi, data = Data_Trust)
summary(FitBi,fit.measures=TRUE,standardized=TRUE)
modindices(FitBi)





#SEM_BiFactor - Belief in Information
Model_Bi1<-'TrustinScienceScientist =~ ts1 + ts2 + ts3 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts12 + ts13 + ts14 + ts15 + ts16 + ts17 + ts18 + ts19 + ts20 + ts21
           TrustinScience =~ ts3 + ts12 + ts14 + ts15 + ts21
           TrustinScientist =~ ts1 + ts2 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts13 + ts16 + ts17 + ts18 + ts19 + ts20
            TrustinScienceScientist ~ 0*TrustinScience + 0*TrustinScientist
            Outcome_beliefinfo ~ TrustinScience + TrustinScientist + TrustinScienceScientist'
SEMBi1 <- sem(Model_Bi1, data = Data_Trust, std.lv = TRUE)
summary(SEMBi1,fit.measures=TRUE,standardized=TRUE)


#SEM_BiFactor - Trust in Individual Scientists
Model_Bi2<-'TrustinScienceScientist =~ ts1 + ts2 + ts3 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts12 + ts13 + ts14 + ts15 + ts16 + ts17 + ts18 + ts19 + ts20 + ts21
           TrustinScience =~ ts3 + ts12 + ts14 + ts15 + ts21
           TrustinScientist =~ ts1 + ts2 + ts4 + ts5 + ts6 + ts7 + ts8 + ts9 + ts10 + ts11 + ts13 + ts16 + ts17 + ts18 + ts19 + ts20
            TrustinScienceScientist ~ 0*TrustinScience + 0*TrustinScientist
           Outcome_highcred ~ TrustinScience + TrustinScientist + TrustinScienceScientist'
SEMBi2 <- sem(Model_Bi2, data = Data_Trust, std.lv = TRUE)
summary(SEMBi2,fit.measures=TRUE,standardized=TRUE)


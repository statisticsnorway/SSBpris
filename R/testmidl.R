install.packages("devtools")
library(devtools)
install_github("statisticsnorway/struktuR")
library(struktuR)
install_github("statisticsnorway/SSBpris")
library(SSBpris)
load(file="/ssb/bruker/ave/SSBpris/data/priceData.RData")

browseVignettes("struktuR")
browseVignettes("SSBpris")
getwd()

#Bruk av CalcInd uten consumVar gir alle elementærindeksene som output 
CalcInd(data=priceData, baseVar="b1", pVar="p1", type="jevons", groupVar="varenr", wVar = "weight")


#Bruk av CalcInd med consumVar gir indeksen for hver verdi av consumVar
CalcInd(data=priceData, baseVar="b1", pVar="p1", type="jevons", groupVar="varenr", consumVar = "coicop", wVar = "weight")

#Bruk av CalcInd med consumVar gir indeksen for hver verdi av consumVar
#For hovedindeks, lag en consumVar som er lik for alle
priceData$en <- 1 
CalcInd(data=priceData, baseVar="b1", pVar="p1", type="jevons", groupVar="varenr", consumVar = "en", wVar = "weight")




library(AllocSN)

data <- priceData
data <- testdata
baseVar <- "b1"
pVar <- "p1"
type <- "jevons"
groupVar <- "varenr_fac"
consumVar <- "coicop"
wVar = "weight"



testdata <- priceData

testdata$varenr_fac <- factor(testdata$varenr)
levels((testdata$varenr_fac))

testdata <- testdata[(testdata$varenr)<=100,]
levels((testdata$varenr_fac))

testdata <- testdata[order(testdata$dufNr),]

testdata$agg1 <- ifelse(testdata$coicop==1, "En",
                        ifelse(testdata$coicop==2, "To",
                               ifelse(testdata$coicop==3, "Tre",
                                      ifelse(testdata$coicop==4, "Fire",
                                             ifelse(testdata$coicop==5, "Fem",
                                                    ifelse(testdata$coicop==6, "Seks",
                                                           ifelse(testdata$coicop==7, "Sju",
                                                                  ifelse(testdata$coicop==8, "Åtte",
                                                                         ifelse(testdata$coicop==9, "Ni",
                                                                                ifelse(testdata$coicop==10, "Ti", 11))))))))))





testdata[2,"varenr_fac"] <- ""
testdata[1,"varenr"] <- NA
testdata[3,"b1"] <- NA
testdata[4,"p1"] <- NA
testdata["2","coicop"] <- 1
testdata[,"coicop"] <- NA
testdata[6,"weight"] <- NA
testdata[6,"weight"] <- Inf
testdata[7,"weight"] <- 0
testdata[testdata$coicop==1,"weight"] <- 0

y <- CalcInd(data=testdata, baseVar="b1", pVar="p1", type="jevons", groupVar="varenr", consumVar = "coicop", 
             wVar = "weight")

x <- CalcInd(data=testdata, baseVar="b1", pVar="p1", type="jevons", groupVar="varenr", consumVar = , 
             wVar = "weight")

z <- CalcInd(data=testdata, baseVar="b1", pVar="p1", type="jevons", groupVar="varenr", consumVar = "varenr" ,
             wVar = "weight")

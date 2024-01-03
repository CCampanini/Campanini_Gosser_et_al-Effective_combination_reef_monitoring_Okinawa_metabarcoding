#|||||||||||||||||||||||||||||||
#########################################################
#|||||||||||||||||||||||||||||||
#########################################################
#SUMMARY

#SECTION 1: PLIT data extraction to obtain the relative benthic coverage 
#marix at different sites
#     -SN = SUNABE
#     -AW = AWA
#     -ZA = ZATSUN
#     -HI = HIZUSHI 

#SECTION 2: nMDS

#N.B. search for section in the script to reach the top of its section
########################################################
#|||||||||||||||||||||||||||||||
#########################################################
#|||||||||||||||||||||||||||||||
#########################################################

#setwd("C:/Users/claud/Qsync/QSync Claudia/Thesis/Let_the_benthos_speak/PLIT_extraction")

library(readxl) #read xlsx files
library(tidyverse) #aggregate fc et al.
library(tidyverse)
library(dplyr)
library(MASS)


#########################################################
#########################################################
#SUNABE RELATIVE BENTHIC COVERAGE
########################################################
#########################################################

#SN_D1

SN_D1<- read_excel("PLIT_SN.xlsx", sheet = "D1")
class(SN_D1)
SN_D1<- SN_D1[-c(1:2),c(1:5)]
colnames(SN_D1) <-SN_D1[1,]
SN_D1<- SN_D1[-1,]
str(SN_D1)
SN_D1$Point <-as.numeric(SN_D1$Point)
SN_D1$Cat_tot <-as.numeric(SN_D1$Cat_tot)
SN_D1$Func_tot <-as.numeric(SN_D1$Func_tot)
SN_D1$Category<-as.factor(SN_D1$Category)
SN_D1$Functional_group<-as.factor(SN_D1$Functional_group)

str(SN_D1)

print(aggregate(Cat_tot ~ Category, SN_D1, sum))

SN_D1_cov <- data.frame(aggregate(Cat_tot ~ Category, SN_D1, sum))
names(SN_D1_cov)[2] <-"Abs_cov"
sum(SN_D1_cov$Abs_cov)
SN_D1_cov$Rel_cov <- as.numeric(round(SN_D1_cov$Abs_cov/sum(SN_D1_cov$Abs_cov),3))
str(SN_D1_cov)
sum(SN_D1_cov$Rel_cov)

categories <- c("algae","boulder","hard_coral","missing","other","rubble","sand",
                "shadow","soft_coral","sponge","unknown","zoanthids")
categories
count(SN_D1_cov[2]) #there should be 12 categories
list(SN_D1_cov[1]) #
SN_D1_cov[,1] <-as.character(SN_D1_cov[,1])
class(SN_D1_cov[,1])
SN_D1_cov <-rbind(SN_D1_cov,
                  c("other",0,0),
                  c("rubble",0,0),
                  c("sponge",0,0),
                  c("zoanthids",0,0))
class(SN_D1_cov[,1])

SN_D1_cov <- with(SN_D1_cov,  SN_D1_cov[order(SN_D1_cov$Category) , ])
SN_D1_cov

#########################################################
#########################################################
#SN_D2

SN_D2<- read_excel("PLIT_SN.xlsx", sheet = "D2")
class(SN_D2)
SN_D2<- SN_D2[-c(1:2),c(1:5)]
colnames(SN_D2) <-SN_D2[1,]
SN_D2<- SN_D2[-1,]
str(SN_D2)
SN_D2$Point <-as.numeric(SN_D2$Point)
SN_D2$Cat_tot <-as.numeric(SN_D2$Cat_tot)
SN_D2$Func_tot <-as.numeric(SN_D2$Func_tot)
SN_D2$Category<-as.factor(SN_D2$Category)
SN_D2$Functional_group<-as.factor(SN_D2$Functional_group)

str(SN_D2)

print(aggregate(Cat_tot ~ Category, SN_D2, sum))

SN_D2_cov <- data.frame(aggregate(Cat_tot ~ Category, SN_D2, sum))
names(SN_D2_cov)[2] <-"Abs_cov"
sum(SN_D2_cov$Abs_cov)
SN_D2_cov$Rel_cov <- as.numeric(round(SN_D2_cov$Abs_cov/sum(SN_D2_cov$Abs_cov),3))
str(SN_D2_cov)
sum(SN_D2_cov$Rel_cov)

categories
list(SN_D2_cov[1]) #there should be 12 categories
SN_D2_cov[,1] <-as.character(SN_D2_cov[,1])
class(SN_D2_cov[,1])
SN_D2_cov <-rbind(SN_D2_cov,
                  c("other",0,0),
                  c("rubble",0,0),
                  c("sponge",0,0))
class(SN_D2_cov[,1])

SN_D2_cov <- with(SN_D2_cov,  SN_D2_cov[order(SN_D2_cov$Category) , ])
SN_D2_cov


#########################################################
#########################################################
#SN_D3

SN_D3<- read_excel("PLIT_SN.xlsx", sheet = "D3")
class(SN_D3)
SN_D3<- SN_D3[-c(1:2),c(1:5)]
colnames(SN_D3) <-SN_D3[1,]
SN_D3<- SN_D3[-1,]
str(SN_D3)
SN_D3$Point <-as.numeric(SN_D3$Point)
SN_D3$Cat_tot <-as.numeric(SN_D3$Cat_tot)
SN_D3$Func_tot <-as.numeric(SN_D3$Func_tot)
SN_D3$Category<-as.factor(SN_D3$Category)
SN_D3$Functional_group<-as.factor(SN_D3$Functional_group)

str(SN_D3)

print(aggregate(Cat_tot ~ Category, SN_D3, sum))

SN_D3_cov <- data.frame(aggregate(Cat_tot ~ Category, SN_D3, sum))
names(SN_D3_cov)[2] <-"Abs_cov"
sum(SN_D3_cov$Abs_cov)
SN_D3_cov$Rel_cov <- as.numeric(round(SN_D3_cov$Abs_cov/sum(SN_D3_cov$Abs_cov),3))
str(SN_D3_cov)
sum(SN_D3_cov$Rel_cov)

categories
list(SN_D3_cov[1]) #there should be 12 categories
SN_D3_cov[,1] <-as.character(SN_D3_cov[,1])
class(SN_D3_cov[,1])
SN_D3_cov <-rbind(SN_D3_cov,
                  c("other",0,0),
                  c("sand",0,0),
                  c("sponge",0,0))
class(SN_D3_cov[,1])

SN_D3_cov <- with(SN_D3_cov,  SN_D3_cov[order(SN_D3_cov$Category) , ])
SN_D3_cov


#########################################################
#########################################################
#SN_S1

SN_S1<- read_excel("PLIT_SN.xlsx", sheet = "S1")
class(SN_S1)
SN_S1<- SN_S1[-c(1:2),c(1:5)]
colnames(SN_S1) <-SN_S1[1,]
SN_S1<- SN_S1[-1,]
str(SN_S1)
SN_S1$Point <-as.numeric(SN_S1$Point)
SN_S1$Cat_tot <-as.numeric(SN_S1$Cat_tot)
SN_S1$Func_tot <-as.numeric(SN_S1$Func_tot)
SN_S1$Category<-as.factor(SN_S1$Category)
SN_S1$Functional_group<-as.factor(SN_S1$Functional_group)

str(SN_S1)

print(aggregate(Cat_tot ~ Category, SN_S1, sum))

SN_S1_cov <- data.frame(aggregate(Cat_tot ~ Category, SN_S1, sum))
names(SN_S1_cov)[2] <-"Abs_cov"
sum(SN_S1_cov$Abs_cov)
SN_S1_cov$Rel_cov <- as.numeric(round(SN_S1_cov$Abs_cov/sum(SN_S1_cov$Abs_cov),3))
str(SN_S1_cov)
sum(SN_S1_cov$Rel_cov)

categories
list(SN_S1_cov[1]) #there should be 12 categories
SN_S1_cov[,1] <-as.character(SN_S1_cov[,1])
class(SN_S1_cov[,1])
SN_S1_cov <-rbind(SN_S1_cov,
                  c("other",0,0),
                  c("rubble",0,0),
                  c("sand",0,0),
                  c("sponge",0,0))
class(SN_S1_cov[,1])

SN_S1_cov <- with(SN_S1_cov,  SN_S1_cov[order(SN_S1_cov$Category) , ])
SN_S1_cov

#########################################################
#########################################################
#SN_S2

SN_S2<- read_excel("PLIT_SN.xlsx", sheet = "S2")
class(SN_S2)
SN_S2<- SN_S2[-c(1:2),c(1:5)]
colnames(SN_S2) <-SN_S2[1,]
SN_S2<- SN_S2[-1,]
str(SN_S2)
SN_S2$Point <-as.numeric(SN_S2$Point)
SN_S2$Cat_tot <-as.numeric(SN_S2$Cat_tot)
SN_S2$Func_tot <-as.numeric(SN_S2$Func_tot)
SN_S2$Category<-as.factor(SN_S2$Category)
SN_S2$Functional_group<-as.factor(SN_S2$Functional_group)

str(SN_S2)

print(aggregate(Cat_tot ~ Category, SN_S2, sum))

SN_S2_cov <- data.frame(aggregate(Cat_tot ~ Category, SN_S2, sum))
names(SN_S2_cov)[2] <-"Abs_cov"
sum(SN_S2_cov$Abs_cov)
SN_S2_cov$Rel_cov <- as.numeric(round(SN_S2_cov$Abs_cov/sum(SN_S2_cov$Abs_cov),3))
str(SN_S2_cov)
sum(SN_S2_cov$Rel_cov)

categories
list(SN_S2_cov[1]) #there should be 12 categories
SN_S2_cov[,1] <-as.character(SN_S2_cov[,1])
class(SN_S2_cov[,1])
SN_S2_cov <-rbind(SN_S2_cov,
                  c("other",0,0),
                  c("rubble",0,0),
                  c("sponge",0,0))
class(SN_S2_cov[,1])

SN_S2_cov <- with(SN_S2_cov,  SN_S2_cov[order(SN_S2_cov$Category) , ])
SN_S2_cov

#########################################################
#########################################################
#SN_S3

SN_S3<- read_excel("PLIT_SN.xlsx", sheet = "S3")
class(SN_S3)
SN_S3<- SN_S3[-c(1:2),c(1:5)]
colnames(SN_S3) <-SN_S3[1,]
SN_S3<- SN_S3[-1,]
str(SN_S3)
SN_S3$Point <-as.numeric(SN_S3$Point)
SN_S3$Cat_tot <-as.numeric(SN_S3$Cat_tot)
SN_S3$Func_tot <-as.numeric(SN_S3$Func_tot)
SN_S3$Category<-as.factor(SN_S3$Category)
SN_S3$Functional_group<-as.factor(SN_S3$Functional_group)

str(SN_S3)

print(aggregate(Cat_tot ~ Category, SN_S3, sum))

SN_S3_cov <- data.frame(aggregate(Cat_tot ~ Category, SN_S3, sum))
names(SN_S3_cov)[2] <-"Abs_cov"
sum(SN_S3_cov$Abs_cov)
SN_S3_cov$Rel_cov <- as.numeric(round(SN_S3_cov$Abs_cov/sum(SN_S3_cov$Abs_cov),3))
str(SN_S3_cov)
sum(SN_S3_cov$Rel_cov)

categories
list(SN_S3_cov[1]) #there should be 12 categories
SN_S3_cov[,1] <-as.character(SN_S3_cov[,1])
class(SN_S3_cov[,1])
SN_S3_cov <-rbind(SN_S3_cov,
                  c("other",0,0),
                  c("missing",0,0),
                  c("sponge",0,0))
class(SN_S3_cov[,1])

SN_S3_cov <- with(SN_S3_cov,  SN_S3_cov[order(SN_S3_cov$Category) , ])
SN_S3_cov

#########################################################
#########################################################
#AWA RELATIVE BENTHIC COVERAGE
########################################################
#########################################################

#AW_D1

AW_D1<- read_excel("PLIT_AWA.xlsx", sheet = "D1")
class(AW_D1)
AW_D1<- AW_D1[-c(1:2),c(1:5)]
colnames(AW_D1) <-AW_D1[1,]
AW_D1<- AW_D1[-1,]
str(AW_D1)
AW_D1$Point <-as.numeric(AW_D1$Point)
AW_D1$Cat_tot <-as.numeric(AW_D1$Cat_tot)
AW_D1$Func_tot <-as.numeric(AW_D1$Func_tot)
AW_D1$Category<-as.factor(AW_D1$Category)
AW_D1$Functional_group<-as.factor(AW_D1$Functional_group)

str(AW_D1)

print(aggregate(Cat_tot ~ Category, AW_D1, sum))

AW_D1_cov <- data.frame(aggregate(Cat_tot ~ Category, AW_D1, sum))
names(AW_D1_cov)[2] <-"Abs_cov"
sum(AW_D1_cov$Abs_cov)
AW_D1_cov$Rel_cov <- as.numeric(round(AW_D1_cov$Abs_cov/sum(AW_D1_cov$Abs_cov),3))
str(AW_D1_cov)
sum(AW_D1_cov$Rel_cov)

categories
list(AW_D1_cov[1]) #there should be 12 categories
AW_D1_cov[,1] <-as.character(AW_D1_cov[,1])
class(AW_D1_cov[,1])
AW_D1_cov <-rbind(AW_D1_cov,
                  c("other",0,0),
                  c("missing",0,0),
                  c("sponge",0,0), 
                  c("zoanthids",0,0),
                  c("hard_coral",0,0))
class(AW_D1_cov[,1])

AW_D1_cov <- with(AW_D1_cov,  AW_D1_cov[order(AW_D1_cov$Category) , ])
AW_D1_cov

#########################################################
#########################################################
#AW_D2

AW_D2<- read_excel("PLIT_AWA.xlsx", sheet = "D2")
class(AW_D2)
AW_D2<- AW_D2[-c(1:2),c(1:5)]
colnames(AW_D2) <-AW_D2[1,]
AW_D2<- AW_D2[-1,]
str(AW_D2)
AW_D2$Point <-as.numeric(AW_D2$Point)
AW_D2$Cat_tot <-as.numeric(AW_D2$Cat_tot)
AW_D2$Func_tot <-as.numeric(AW_D2$Func_tot)
AW_D2$Category<-as.factor(AW_D2$Category)
AW_D2$Functional_group<-as.factor(AW_D2$Functional_group)

str(AW_D2)

print(aggregate(Cat_tot ~ Category, AW_D2, sum))

AW_D2_cov <- data.frame(aggregate(Cat_tot ~ Category, AW_D2, sum))
names(AW_D2_cov)[2] <-"Abs_cov"
sum(AW_D2_cov$Abs_cov)
AW_D2_cov$Rel_cov <- as.numeric(round(AW_D2_cov$Abs_cov/sum(AW_D2_cov$Abs_cov),3))
str(AW_D2_cov)
sum(AW_D2_cov$Rel_cov)

categories
list(AW_D2_cov[1]) #there should be 12 categories
AW_D2_cov[,1] <-as.character(AW_D2_cov[,1])
class(AW_D2_cov[,1])
AW_D2_cov <-rbind(AW_D2_cov,
                  c("missing",0,0),
                  c("sponge",0,0), 
                  c("zoanthids",0,0),
                  c("boulder",0,0))
class(AW_D2_cov[,1])

AW_D2_cov <- with(AW_D2_cov,  AW_D2_cov[order(AW_D2_cov$Category) , ])
AW_D2_cov

#########################################################
#########################################################
#AW_D3

AW_D3<- read_excel("PLIT_AWA.xlsx", sheet = "D3")
class(AW_D3)
AW_D3<- AW_D3[-c(1:2),c(1:5)]
colnames(AW_D3) <-AW_D3[1,]
AW_D3<- AW_D3[-1,]
str(AW_D3)
AW_D3$Point <-as.numeric(AW_D3$Point)
AW_D3$Cat_tot <-as.numeric(AW_D3$Cat_tot)
AW_D3$Func_tot <-as.numeric(AW_D3$Func_tot)
AW_D3$Category<-as.factor(AW_D3$Category)
AW_D3$Functional_group<-as.factor(AW_D3$Functional_group)

str(AW_D3)

print(aggregate(Cat_tot ~ Category, AW_D3, sum))

AW_D3_cov <- data.frame(aggregate(Cat_tot ~ Category, AW_D3, sum))
names(AW_D3_cov)[2] <-"Abs_cov"
sum(AW_D3_cov$Abs_cov)
AW_D3_cov$Rel_cov <- as.numeric(round(AW_D3_cov$Abs_cov/sum(AW_D3_cov$Abs_cov),3))
str(AW_D3_cov)
sum(AW_D3_cov$Rel_cov)

categories
list(AW_D3_cov[1]) #there should be 12 categories
AW_D3_cov[,1] <-as.character(AW_D3_cov[,1])
class(AW_D3_cov[,1])
AW_D3_cov <-rbind(AW_D3_cov,
                  c("missing",0,0),
                  c("sponge",0,0), 
                  c("zoanthids",0,0),
                  c("soft_coral",0,0),
                  c("other",0,0) )
class(AW_D3_cov[,1])

AW_D3_cov <- with(AW_D3_cov,  AW_D3_cov[order(AW_D3_cov$Category) , ])
AW_D3_cov

#########################################################
#########################################################
#AW_S1

AW_S1<- read_excel("PLIT_AWA.xlsx", sheet = "S1")
class(AW_S1)
AW_S1<- AW_S1[-c(1:2),c(1:5)]
colnames(AW_S1) <-AW_S1[1,]
AW_S1<- AW_S1[-1,]
str(AW_S1)
AW_S1$Point <-as.numeric(AW_S1$Point)
AW_S1$Cat_tot <-as.numeric(AW_S1$Cat_tot)
AW_S1$Func_tot <-as.numeric(AW_S1$Func_tot)
AW_S1$Category<-as.factor(AW_S1$Category)
AW_S1$Functional_group<-as.factor(AW_S1$Functional_group)

str(AW_S1)

print(aggregate(Cat_tot ~ Category, AW_S1, sum))

AW_S1_cov <- data.frame(aggregate(Cat_tot ~ Category, AW_S1, sum))
names(AW_S1_cov)[2] <-"Abs_cov"
sum(AW_S1_cov$Abs_cov)
AW_S1_cov$Rel_cov <- as.numeric(round(AW_S1_cov$Abs_cov/sum(AW_S1_cov$Abs_cov),3))
str(AW_S1_cov)
sum(AW_S1_cov$Rel_cov)

categories
list(AW_S1_cov[1]) #there should be 12 categories
AW_S1_cov[,1] <-as.character(AW_S1_cov[,1])
class(AW_S1_cov[,1])
AW_S1_cov <-rbind(AW_S1_cov,
                  c("missing",0,0),
                  c("boulder",0,0),
                  c("sponge",0,0), 
                  c("zoanthids",0,0),
                  c("soft_coral",0,0),
                  c("other",0,0) )
class(AW_S1_cov[,1])

AW_S1_cov <- with(AW_S1_cov,  AW_S1_cov[order(AW_S1_cov$Category) , ])
AW_S1_cov

#########################################################
#########################################################
#AW_S2

AW_S2<- read_excel("PLIT_AWA.xlsx", sheet = "S2")
class(AW_S2)
AW_S2<- AW_S2[-c(1:2),c(1:5)]
colnames(AW_S2) <-AW_S2[1,]
AW_S2<- AW_S2[-1,]
str(AW_S2)
AW_S2$Point <-as.numeric(AW_S2$Point)
AW_S2$Cat_tot <-as.numeric(AW_S2$Cat_tot)
AW_S2$Func_tot <-as.numeric(AW_S2$Func_tot)
AW_S2$Category<-as.factor(AW_S2$Category)
AW_S2$Functional_group<-as.factor(AW_S2$Functional_group)

str(AW_S2)

print(aggregate(Cat_tot ~ Category, AW_S2, sum))

AW_S2_cov <- data.frame(aggregate(Cat_tot ~ Category, AW_S2, sum))
names(AW_S2_cov)[2] <-"Abs_cov"
sum(AW_S2_cov$Abs_cov)
AW_S2_cov$Rel_cov <- as.numeric(round(AW_S2_cov$Abs_cov/sum(AW_S2_cov$Abs_cov),3))
str(AW_S2_cov)
sum(AW_S2_cov$Rel_cov)

categories
list(AW_S2_cov[1]) #there should be 12 categories
AW_S2_cov[,1] <-as.character(AW_S2_cov[,1])
class(AW_S2_cov[,1])
AW_S2_cov <-rbind(AW_S2_cov,
                  c("sponge",0,0), 
                  c("zoanthids",0,0),
                  c("other",0,0) )
class(AW_S2_cov[,1])

AW_S2_cov <- with(AW_S2_cov,  AW_S2_cov[order(AW_S2_cov$Category) , ])
AW_S2_cov
class(AW_S2_cov$Category)


#########################################################
#########################################################
#AW_S3

AW_S3<- read_excel("PLIT_AWA.xlsx", sheet = "S3")
class(AW_S3)
AW_S3<- AW_S3[-c(1:2),c(1:5)]
colnames(AW_S3) <-AW_S3[1,]
AW_S3<- AW_S3[-1,]
str(AW_S3)
AW_S3$Point <-as.numeric(AW_S3$Point)
AW_S3$Cat_tot <-as.numeric(AW_S3$Cat_tot)
AW_S3$Func_tot <-as.numeric(AW_S3$Func_tot)
AW_S3$Category<-as.factor(AW_S3$Category)
AW_S3$Functional_group<-as.factor(AW_S3$Functional_group)

str(AW_S3)

print(aggregate(Cat_tot ~ Category, AW_S3, sum))

AW_S3_cov <- data.frame(aggregate(Cat_tot ~ Category, AW_S3, sum))
names(AW_S3_cov)[2] <-"Abs_cov"
sum(AW_S3_cov$Abs_cov)
AW_S3_cov$Rel_cov <- as.numeric(round(AW_S3_cov$Abs_cov/sum(AW_S3_cov$Abs_cov),3))
str(AW_S3_cov)
sum(AW_S3_cov$Rel_cov)

categories
list(AW_S3_cov[1]) #there should be 12 categories
AW_S3_cov[,1] <-as.character(AW_S3_cov[,1])
class(AW_S3_cov[,1])
AW_S3_cov <-rbind(AW_S3_cov,
                  c("boulder",0,0),
                  c("sponge",0,0),
                  c("other",0,0) )
class(AW_S3_cov[,1])

AW_S3_cov <- with(AW_S3_cov,  AW_S3_cov[order(AW_S3_cov$Category) , ])
AW_S3_cov

#########################################################
#########################################################
#ZATSUN RELATIVE BENTHIC COVERAGE
########################################################
#########################################################

#ZA_D1

ZA_D1<- read_excel("PLIT_ZA.xlsx", sheet = "D1")
class(ZA_D1)
ZA_D1<- ZA_D1[-c(1:2),c(1:5)]
colnames(ZA_D1) <-ZA_D1[1,]
ZA_D1<- ZA_D1[-1,]
str(ZA_D1)
ZA_D1$Point <-as.numeric(ZA_D1$Point)
ZA_D1$Cat_tot <-as.numeric(ZA_D1$Cat_tot)
ZA_D1$Func_tot <-as.numeric(ZA_D1$Func_tot)
ZA_D1$Category<-as.factor(ZA_D1$Category)
ZA_D1$Functional_group<-as.factor(ZA_D1$Functional_group)

str(ZA_D1)

print(aggregate(Cat_tot ~ Category, ZA_D1, sum))

ZA_D1_cov <- data.frame(aggregate(Cat_tot ~ Category, ZA_D1, sum))
names(ZA_D1_cov)[2] <-"Abs_cov"
sum(ZA_D1_cov$Abs_cov)
ZA_D1_cov$Rel_cov <- as.numeric(round(ZA_D1_cov$Abs_cov/sum(ZA_D1_cov$Abs_cov),3))
str(ZA_D1_cov)
sum(ZA_D1_cov$Rel_cov)

categories
list(ZA_D1_cov[1]) #there should be 12 categories
ZA_D1_cov[,1] <-as.character(ZA_D1_cov[,1])
class(ZA_D1_cov[,1])
ZA_D1_cov <-rbind(ZA_D1_cov,
                  c("missing",0,0),
                  c("rubble",0,0),
                  c("other",0,0) )
class(ZA_D1_cov[,1])

ZA_D1_cov <- with(ZA_D1_cov,  ZA_D1_cov[order(ZA_D1_cov$Category) , ])
ZA_D1_cov

#########################################################
#########################################################
#ZA_D2

ZA_D2<- read_excel("PLIT_ZA.xlsx", sheet = "D2")
class(ZA_D2)
ZA_D2<- ZA_D2[-c(1:2),c(1:5)]
colnames(ZA_D2) <-ZA_D2[1,]
ZA_D2<- ZA_D2[-1,]
str(ZA_D2)
ZA_D2$Point <-as.numeric(ZA_D2$Point)
ZA_D2$Cat_tot <-as.numeric(ZA_D2$Cat_tot)
ZA_D2$Func_tot <-as.numeric(ZA_D2$Func_tot)
ZA_D2$Category<-as.factor(ZA_D2$Category)
ZA_D2$Functional_group<-as.factor(ZA_D2$Functional_group)

str(ZA_D2)

print(aggregate(Cat_tot ~ Category, ZA_D2, sum))

ZA_D2_cov <- data.frame(aggregate(Cat_tot ~ Category, ZA_D2, sum))
names(ZA_D2_cov)[2] <-"Abs_cov"
sum(ZA_D2_cov$Abs_cov)
ZA_D2_cov$Rel_cov <- as.numeric(round(ZA_D2_cov$Abs_cov/sum(ZA_D2_cov$Abs_cov),3))
str(ZA_D2_cov)
sum(ZA_D2_cov$Rel_cov)

categories
list(ZA_D2_cov[1]) #there should be 12 categories
ZA_D2_cov[,1] <-as.character(ZA_D2_cov[,1])
class(ZA_D2_cov[,1])
ZA_D2_cov <-rbind(ZA_D2_cov,
                  c("missing",0,0),
                  c("rubble",0,0),
                  c("other",0,0),
                  c("sand",0,0))
class(ZA_D2_cov[,1])

ZA_D2_cov <- with(ZA_D2_cov,  ZA_D2_cov[order(ZA_D2_cov$Category) , ])
ZA_D2_cov

#########################################################
#########################################################
#ZA_D3

ZA_D3<- read_excel("PLIT_ZA.xlsx", sheet = "D3")
class(ZA_D3)
ZA_D3<- ZA_D3[-c(1:2),c(1:5)]
colnames(ZA_D3) <-ZA_D3[1,]
ZA_D3<- ZA_D3[-1,]
str(ZA_D3)
ZA_D3$Point <-as.numeric(ZA_D3$Point)
ZA_D3$Cat_tot <-as.numeric(ZA_D3$Cat_tot)
ZA_D3$Func_tot <-as.numeric(ZA_D3$Func_tot)
ZA_D3$Category<-as.factor(ZA_D3$Category)
ZA_D3$Functional_group<-as.factor(ZA_D3$Functional_group)

str(ZA_D3)

print(aggregate(Cat_tot ~ Category, ZA_D3, sum))

ZA_D3_cov <- data.frame(aggregate(Cat_tot ~ Category, ZA_D3, sum))
names(ZA_D3_cov)[2] <-"Abs_cov"
sum(ZA_D3_cov$Abs_cov)
ZA_D3_cov$Rel_cov <- as.numeric(round(ZA_D3_cov$Abs_cov/sum(ZA_D3_cov$Abs_cov),3))
str(ZA_D3_cov)
sum(ZA_D3_cov$Rel_cov)

categories
list(ZA_D3_cov[1]) #there should be 12 categories
ZA_D3_cov[,1] <-as.character(ZA_D3_cov[,1])
class(ZA_D3_cov[,1])
ZA_D3_cov <-rbind(ZA_D3_cov,
                  c("boulder",0,0),
                  c("missing",0,0),
                  c("rubble",0,0),
                  c("other",0,0),
                  c("sponge",0,0))
class(ZA_D3_cov[,1])

ZA_D3_cov <- with(ZA_D3_cov,  ZA_D3_cov[order(ZA_D3_cov$Category) , ])
ZA_D3_cov

#########################################################
#########################################################
#ZA_S1

ZA_S1<- read_excel("PLIT_ZA.xlsx", sheet = "S1")
class(ZA_S1)
ZA_S1<- ZA_S1[-c(1:2),c(1:5)]
colnames(ZA_S1) <-ZA_S1[1,]
ZA_S1<- ZA_S1[-1,]
str(ZA_S1)
ZA_S1$Point <-as.numeric(ZA_S1$Point)
ZA_S1$Cat_tot <-as.numeric(ZA_S1$Cat_tot)
ZA_S1$Func_tot <-as.numeric(ZA_S1$Func_tot)
ZA_S1$Category<-as.factor(ZA_S1$Category)
ZA_S1$Functional_group<-as.factor(ZA_S1$Functional_group)

str(ZA_S1)

print(aggregate(Cat_tot ~ Category, ZA_S1, sum))

ZA_S1_cov <- data.frame(aggregate(Cat_tot ~ Category, ZA_S1, sum))
names(ZA_S1_cov)[2] <-"Abs_cov"
sum(ZA_S1_cov$Abs_cov)
ZA_S1_cov$Rel_cov <- as.numeric(round(ZA_S1_cov$Abs_cov/sum(ZA_S1_cov$Abs_cov),3))
str(ZA_S1_cov)
sum(ZA_S1_cov$Rel_cov)

categories
list(ZA_S1_cov[1]) #there should be 12 categories
ZA_S1_cov[,1] <-as.character(ZA_S1_cov[,1])
class(ZA_S1_cov[,1])
ZA_S1_cov <-rbind(ZA_S1_cov,
                  c("sand",0,0),
                  c("rubble",0,0),
                  c("other",0,0),
                  c("sponge",0,0))
class(ZA_S1_cov[,1])

ZA_S1_cov <- with(ZA_S1_cov,  ZA_S1_cov[order(ZA_S1_cov$Category) , ])
ZA_S1_cov

#########################################################
#########################################################
#ZA_S2

ZA_S2<- read_excel("PLIT_ZA.xlsx", sheet = "S2")
class(ZA_S2)
ZA_S2<- ZA_S2[-c(1:2),c(1:5)]
colnames(ZA_S2) <-ZA_S2[1,]
ZA_S2<- ZA_S2[-1,]
str(ZA_S2)
ZA_S2$Point <-as.numeric(ZA_S2$Point)
ZA_S2$Cat_tot <-as.numeric(ZA_S2$Cat_tot)
ZA_S2$Func_tot <-as.numeric(ZA_S2$Func_tot)
ZA_S2$Category<-as.factor(ZA_S2$Category)
ZA_S2$Functional_group<-as.factor(ZA_S2$Functional_group)

str(ZA_S2)

print(aggregate(Cat_tot ~ Category, ZA_S2, sum))

ZA_S2_cov <- data.frame(aggregate(Cat_tot ~ Category, ZA_S2, sum))
names(ZA_S2_cov)[2] <-"Abs_cov"
sum(ZA_S2_cov$Abs_cov)
ZA_S2_cov$Rel_cov <- as.numeric(round(ZA_S2_cov$Abs_cov/sum(ZA_S2_cov$Abs_cov),3))
str(ZA_S2_cov)
sum(ZA_S2_cov$Rel_cov)

categories
list(ZA_S2_cov[1]) #there should be 12 categories
ZA_S2_cov[,1] <-as.character(ZA_S2_cov[,1])
class(ZA_S2_cov[,1])
ZA_S2_cov <-rbind(ZA_S2_cov,
                  c("sand",0,0),
                  c("rubble",0,0),
                  c("missing",0,0),
                  c("sponge",0,0))
class(ZA_S2_cov[,1])

ZA_S2_cov <- with(ZA_S2_cov,  ZA_S2_cov[order(ZA_S2_cov$Category) , ])
ZA_S2_cov

#########################################################
#########################################################
#ZA_S3

ZA_S3<- read_excel("PLIT_ZA.xlsx", sheet = "S3")
class(ZA_S3)
ZA_S3<- ZA_S3[-c(1:2),c(1:5)]
colnames(ZA_S3) <-ZA_S3[1,]
ZA_S3<- ZA_S3[-1,]
str(ZA_S3)
ZA_S3$Point <-as.numeric(ZA_S3$Point)
ZA_S3$Cat_tot <-as.numeric(ZA_S3$Cat_tot)
ZA_S3$Func_tot <-as.numeric(ZA_S3$Func_tot)
ZA_S3$Category<-as.factor(ZA_S3$Category)
ZA_S3$Functional_group<-as.factor(ZA_S3$Functional_group)

str(ZA_S3)

print(aggregate(Cat_tot ~ Category, ZA_S3, sum))

ZA_S3_cov <- data.frame(aggregate(Cat_tot ~ Category, ZA_S3, sum))
names(ZA_S3_cov)[2] <-"Abs_cov"
sum(ZA_S3_cov$Abs_cov)
ZA_S3_cov$Rel_cov <- as.numeric(round(ZA_S3_cov$Abs_cov/sum(ZA_S3_cov$Abs_cov),3))
str(ZA_S3_cov)
sum(ZA_S3_cov$Rel_cov)

categories
list(ZA_S3_cov[1]) #there should be 12 categories
ZA_S3_cov[,1] <-as.character(ZA_S3_cov[,1])
class(ZA_S3_cov[,1])
ZA_S3_cov <-rbind(ZA_S3_cov,
                  c("sand",0,0),
                  c("rubble",0,0),
                  c("other",0,0),
                  c("sponge",0,0),
                  c("soft_coral",0,0))
class(ZA_S3_cov[,1])

ZA_S3_cov <- with(ZA_S3_cov,  ZA_S3_cov[order(ZA_S3_cov$Category) , ])
ZA_S3_cov

#########################################################
#########################################################
#HITSUN RELATIVE BENTHIC COVERAGE
########################################################
#########################################################

#HI_D1

HI_D1<- read_excel("PLIT_HI.xlsx", sheet = "D1")
class(HI_D1)
HI_D1<- HI_D1[-c(1:2),c(1:5)]
colnames(HI_D1) <-HI_D1[1,]
HI_D1<- HI_D1[-1,]
str(HI_D1)
HI_D1$Point <-as.numeric(HI_D1$Point)
HI_D1$Cat_tot <-as.numeric(HI_D1$Cat_tot)
HI_D1$Func_tot <-as.numeric(HI_D1$Func_tot)
HI_D1$Category<-as.factor(HI_D1$Category)
HI_D1$Functional_group<-as.factor(HI_D1$Functional_group)

str(HI_D1)

print(aggregate(Cat_tot ~ Category, HI_D1, sum))

HI_D1_cov <- data.frame(aggregate(Cat_tot ~ Category, HI_D1, sum))
names(HI_D1_cov)[2] <-"Abs_cov"
sum(HI_D1_cov$Abs_cov)
HI_D1_cov$Rel_cov <- as.numeric(round(HI_D1_cov$Abs_cov/sum(HI_D1_cov$Abs_cov),3))
str(HI_D1_cov)
sum(HI_D1_cov$Rel_cov)

categories
list(HI_D1_cov[1]) #there should be 12 categories
HI_D1_cov[,1] <-as.character(HI_D1_cov[,1])
class(HI_D1_cov[,1])
HI_D1_cov <-rbind(HI_D1_cov,
                  c("missing",0,0),
                  c("other",0,0),
                  c("zoanthids",0,0))
class(HI_D1_cov[,1])

HI_D1_cov <- with(HI_D1_cov,  HI_D1_cov[order(HI_D1_cov$Category) , ])
HI_D1_cov

#########################################################
#########################################################
#HI_D2

HI_D2<- read_excel("PLIT_HI.xlsx", sheet = "D2")
class(HI_D2)
HI_D2<- HI_D2[-c(1:2),c(1:5)]
colnames(HI_D2) <-HI_D2[1,]
HI_D2<- HI_D2[-1,]
str(HI_D2)
HI_D2$Point <-as.numeric(HI_D2$Point)
HI_D2$Cat_tot <-as.numeric(HI_D2$Cat_tot)
HI_D2$Func_tot <-as.numeric(HI_D2$Func_tot)
HI_D2$Category<-as.factor(HI_D2$Category)
HI_D2$Functional_group<-as.factor(HI_D2$Functional_group)

str(HI_D2)

print(aggregate(Cat_tot ~ Category, HI_D2, sum))

HI_D2_cov <- data.frame(aggregate(Cat_tot ~ Category, HI_D2, sum))
names(HI_D2_cov)[2] <-"Abs_cov"
sum(HI_D2_cov$Abs_cov)
HI_D2_cov$Rel_cov <- as.numeric(round(HI_D2_cov$Abs_cov/sum(HI_D2_cov$Abs_cov),3))
str(HI_D2_cov)
sum(HI_D2_cov$Rel_cov)

categories
list(HI_D2_cov[1]) #there should be 12 categories
HI_D2_cov[,1] <-as.character(HI_D2_cov[,1])
class(HI_D2_cov[,1])
HI_D2_cov <-rbind(HI_D2_cov,
                  c("missing",0,0),
                  c("other",0,0),
                  c("rubble",0,0),
                  c("sand",0,0),
                  c("sponge",0,0))
class(HI_D2_cov[,1])

HI_D2_cov <- with(HI_D2_cov,  HI_D2_cov[order(HI_D2_cov$Category) , ])
HI_D2_cov


#########################################################
#########################################################
#HI_D3

HI_D3<- read_excel("PLIT_HI.xlsx", sheet = "D3")
class(HI_D3)
HI_D3<- HI_D3[-c(1:2),c(1:5)]
colnames(HI_D3) <-HI_D3[1,]
HI_D3<- HI_D3[-1,]
str(HI_D3)
HI_D3$Point <-as.numeric(HI_D3$Point)
HI_D3$Cat_tot <-as.numeric(HI_D3$Cat_tot)
HI_D3$Func_tot <-as.numeric(HI_D3$Func_tot)
HI_D3$Category<-as.factor(HI_D3$Category)
HI_D3$Functional_group<-as.factor(HI_D3$Functional_group)

str(HI_D3)

print(aggregate(Cat_tot ~ Category, HI_D3, sum))

HI_D3_cov <- data.frame(aggregate(Cat_tot ~ Category, HI_D3, sum))
names(HI_D3_cov)[2] <-"Abs_cov"
sum(HI_D3_cov$Abs_cov)
HI_D3_cov$Rel_cov <- as.numeric(round(HI_D3_cov$Abs_cov/sum(HI_D3_cov$Abs_cov),3))
str(HI_D3_cov)
sum(HI_D3_cov$Rel_cov)

categories
list(HI_D3_cov[1]) #there should be 12 categories
HI_D3_cov[,1] <-as.character(HI_D3_cov[,1])
class(HI_D2_cov[,1])
HI_D3_cov <-rbind(HI_D3_cov,
                  c("missing",0,0),
                  c("other",0,0),
                  c("sand",0,0),
                  c("sponge",0,0),
                  c("zoanthids",0,0))
class(HI_D3_cov[,1])

HI_D3_cov <- with(HI_D3_cov,  HI_D3_cov[order(HI_D3_cov$Category) , ])
HI_D3_cov

#########################################################
#########################################################
#HI_S1

HI_S1<- read_excel("PLIT_HI.xlsx", sheet = "S1")
class(HI_S1)
HI_S1<- HI_S1[-c(1:2),c(1:5)]
colnames(HI_S1) <-HI_S1[1,]
HI_S1<- HI_S1[-1,]
str(HI_S1)
HI_S1$Point <-as.numeric(HI_S1$Point)
HI_S1$Cat_tot <-as.numeric(HI_S1$Cat_tot)
HI_S1$Func_tot <-as.numeric(HI_S1$Func_tot)
HI_S1$Category<-as.factor(HI_S1$Category)
HI_S1$Functional_group<-as.factor(HI_S1$Functional_group)

str(HI_S1)

print(aggregate(Cat_tot ~ Category, HI_S1, sum))

HI_S1_cov <- data.frame(aggregate(Cat_tot ~ Category, HI_S1, sum))
names(HI_S1_cov)[2] <-"Abs_cov"
sum(HI_S1_cov$Abs_cov)
HI_S1_cov$Rel_cov <- as.numeric(round(HI_S1_cov$Abs_cov/sum(HI_S1_cov$Abs_cov),3))
str(HI_S1_cov)
sum(HI_S1_cov$Rel_cov)

categories
list(HI_S1_cov[1]) #there should be 12 categories
HI_S1_cov[,1] <-as.character(HI_S1_cov[,1])
class(HI_S1_cov[,1])
HI_S1_cov <-rbind(HI_S1_cov,
                  c("missing",0,0),
                  c("other",0,0),
                  c("sand",0,0),
                  c("sponge",0,0),
                  c("rubble",0,0))
class(HI_S1_cov[,1])

HI_S1_cov <- with(HI_S1_cov,  HI_S1_cov[order(HI_S1_cov$Category) , ])
HI_S1_cov

#########################################################
#########################################################
#HI_S2

HI_S2<- read_excel("PLIT_HI.xlsx", sheet = "S2")
class(HI_S2)
HI_S2<- HI_S2[-c(1:2),c(1:5)]
colnames(HI_S2) <-HI_S2[1,]
HI_S2<- HI_S2[-1,]
str(HI_S2)
HI_S2$Point <-as.numeric(HI_S2$Point)
HI_S2$Cat_tot <-as.numeric(HI_S2$Cat_tot)
HI_S2$Func_tot <-as.numeric(HI_S2$Func_tot)
HI_S2$Category<-as.factor(HI_S2$Category)
HI_S2$Functional_group<-as.factor(HI_S2$Functional_group)

str(HI_S2)

print(aggregate(Cat_tot ~ Category, HI_S2, sum))

HI_S2_cov <- data.frame(aggregate(Cat_tot ~ Category, HI_S2, sum))
names(HI_S2_cov)[2] <-"Abs_cov"
sum(HI_S2_cov$Abs_cov)
HI_S2_cov$Rel_cov <- as.numeric(round(HI_S2_cov$Abs_cov/sum(HI_S2_cov$Abs_cov),3))
str(HI_S2_cov)
sum(HI_S2_cov$Rel_cov)

categories
list(HI_S2_cov[1]) #there should be 12 categories
HI_S2_cov[,1] <-as.character(HI_S2_cov[,1])
class(HI_S1_cov[,1])
HI_S2_cov <-rbind(HI_S2_cov,
                  c("missing",0,0),
                  c("other",0,0),
                  c("sponge",0,0),
                  c("rubble",0,0))
class(HI_S2_cov[,1])

HI_S2_cov <- with(HI_S2_cov,  HI_S2_cov[order(HI_S2_cov$Category) , ])
HI_S2_cov


#########################################################
#########################################################
#HI_S3

HI_S3<- read_excel("PLIT_HI.xlsx", sheet = "S3")
class(HI_S3)
HI_S3<- HI_S3[-c(1:2),c(1:5)]
colnames(HI_S3) <-HI_S3[1,]
HI_S3<- HI_S3[-1,]
str(HI_S3)
HI_S3$Point <-as.numeric(HI_S3$Point)
HI_S3$Cat_tot <-as.numeric(HI_S3$Cat_tot)
HI_S3$Func_tot <-as.numeric(HI_S3$Func_tot)
HI_S3$Category<-as.factor(HI_S3$Category)
HI_S3$Functional_group<-as.factor(HI_S3$Functional_group)

str(HI_S3)

print(aggregate(Cat_tot ~ Category, HI_S3, sum))

HI_S3_cov <- data.frame(aggregate(Cat_tot ~ Category, HI_S3, sum))
names(HI_S3_cov)[2] <-"Abs_cov"
sum(HI_S3_cov$Abs_cov)
HI_S3_cov$Rel_cov <- as.numeric(round(HI_S3_cov$Abs_cov/sum(HI_S3_cov$Abs_cov),3))
str(HI_S3_cov)
sum(HI_S3_cov$Rel_cov)

categories
list(HI_S3_cov[1]) #there should be 12 categories
HI_S3_cov[,1] <-as.character(HI_S3_cov[,1])
class(HI_S3_cov[,1])
HI_S3_cov <-rbind(HI_S3_cov,
                  c("missing",0,0),
                  c("other",0,0),
                  c("sponge",0,0),
                  c("sand",0,0))
class(HI_S3_cov[,1])

HI_S3_cov <- with(HI_S3_cov,  HI_S3_cov[order(HI_S3_cov$Category) , ])
HI_S3_cov


#####################################################################


prel_abs_cov_df <- as.data.frame(cbind(categories,
                                       SN_D1_cov[2],
                                       SN_D2_cov[2],
                                       SN_D3_cov[2],
                                       SN_S1_cov[2],
                                       SN_S2_cov[2],
                                       SN_S3_cov[2],
                                       AW_D1_cov[2],
                                       AW_D2_cov[2],
                                       AW_D3_cov[2],
                                       AW_S1_cov[2],
                                       AW_S2_cov[2],
                                       AW_S3_cov[2],
                                       ZA_D1_cov[2],
                                       ZA_D2_cov[2],
                                       ZA_D3_cov[2],
                                       ZA_S1_cov[2],
                                       ZA_S2_cov[2],
                                       ZA_S3_cov[2],
                                       HI_D1_cov[2],
                                       HI_D2_cov[2],
                                       HI_D3_cov[2],
                                       HI_S1_cov[2],
                                       HI_S2_cov[2],
                                       HI_S3_cov[2]))

prel_trans <- c("SN_D1",
                "SN_D2",
                "SN_D3",
                "SN_S1",
                "SN_S2",
                "SN_S3",
                "AW_D1",
                "AW_D2",
                "AW_D3",
                "AW_S1",
                "AW_S2",
                "AW_S3",
                "ZA_D1",
                "ZA_D2",
                "ZA_D3",
                "ZA_S1",
                "ZA_S2",
                "ZA_S3",
                "HI_D1",
                "HI_D2",
                "HI_D3",
                "HI_S1",
                "HI_S2",
                "HI_S3")

colnames(prel_abs_cov_df) <- c("Categories",
                               "SN_D1",
                               "SN_D2",
                               "SN_D3",
                               "SN_S1",
                               "SN_S2",
                               "SN_S3",
                               "AW_D1",
                               "AW_D2",
                               "AW_D3",
                               "AW_S1",
                               "AW_S2",
                               "AW_S3",
                               "ZA_D1",
                               "ZA_D2",
                               "ZA_D3",
                               "ZA_S1",
                               "ZA_S2",
                               "ZA_S3",
                               "HI_D1",
                               "HI_D2",
                               "HI_D3",
                               "HI_S1",
                               "HI_S2",
                               "HI_S3")

rownames(prel_abs_cov_df) <-categories
prel_abs_cov_df<-prel_abs_cov_df[,-1]

prel_rel_cov_df <- as.data.frame(cbind(categories,
                                       SN_D1_cov[3],
                                       SN_D2_cov[3],
                                       SN_D3_cov[3],
                                       SN_S1_cov[3],
                                       SN_S2_cov[3],
                                       SN_S3_cov[3],
                                       AW_D1_cov[3],
                                       AW_D2_cov[3],
                                       AW_D3_cov[3],
                                       AW_S1_cov[3],
                                       AW_S2_cov[3],
                                       AW_S3_cov[3],
                                       ZA_D1_cov[3],
                                       ZA_D2_cov[3],
                                       ZA_D3_cov[3],
                                       ZA_S1_cov[3],
                                       ZA_S2_cov[3],
                                       ZA_S3_cov[3],
                                       HI_D1_cov[3],
                                       HI_D2_cov[3],
                                       HI_D3_cov[3],
                                       HI_S1_cov[3],
                                       HI_S2_cov[3],
                                       HI_S3_cov[3]))

colnames(prel_rel_cov_df) <- c("Categories",
                               "SN_D1",
                               "SN_D2",
                               "SN_D3",
                               "SN_S1",
                               "SN_S2",
                               "SN_S3",
                               "AW_D1",
                               "AW_D2",
                               "AW_D3",
                               "AW_S1",
                               "AW_S2",
                               "AW_S3",
                               "ZA_D1",
                               "ZA_D2",
                               "ZA_D3",
                               "ZA_S1",
                               "ZA_S2",
                               "ZA_S3",
                               "HI_D1",
                               "HI_D2",
                               "HI_D3",
                               "HI_S1",
                               "HI_S2",
                               "HI_S3")

rownames(prel_rel_cov_df) <-categories
prel_rel_cov_df<-prel_rel_cov_df[,-1]

################################################
#CLEAN RELATIVE COVERAGE DATA

prel_abs_cov_clean <- prel_abs_cov_df[-c(4,8,11),] #Eliminate "missing", "shadow", and "unknown"

str(prel_abs_cov_clean)

#Convert prel_abs_cov_clean from charachter to numeric
for(i in 1:ncol(prel_abs_cov_clean)) {       # for-loop over columns
  prel_abs_cov_clean[,i] <-as.numeric(prel_abs_cov_clean[,i])
}
str(prel_abs_cov_clean)

#Create a copy of prel_abs_cov_clean with relative coverage (expressed in %)
for(i in 1:ncol(prel_abs_cov_clean)) {       # for-loop over columns
  prel_abs_cov_clean[,i] <-as.numeric(prel_abs_cov_clean[,i])
  prel_rel_cov_clean <-prel_abs_cov_clean
  prel_rel_cov_clean[,i] <-as.numeric(prel_abs_cov_clean[,i])
}

str(prel_rel_cov_clean)

#Create a dataframe with relative coverage (expressed in %)
for(i in 1:ncol(prel_rel_cov_clean)) {       # for-loop over columns
  prel_rel_cov_clean[,i]<-round(prel_rel_cov_clean[,i]/
                                  sum(prel_rel_cov_clean[,i]),3)
}

str(prel_rel_cov_clean)

relcov<-t(prel_rel_cov_clean)

transect <-as.vector(rownames(relcov))
site <- c(rep("Sunabe",6),rep("Awa",6),rep("Zatsun",6),rep("Hizushi",6))
depth <-rep(c("Deep","Deep","Deep","Shallow","Shallow","Shallow"),4)
rep <- rep(c("1","2","3"),8)
site_depth <- c(rep("SN_D",3),
                rep("SN_S",3),
                rep("AW_D",3),
                rep("AW_S",3),
                rep("ZA_D",3),
                rep("ZA_S",3),
                rep("HI_D",3),
                rep("HI_S",3))

rel_cov_input <- cbind(transect,site,depth,rep,site_depth,relcov)
class(rel_cov_input)
rel_cov_input_df <-as.data.frame(rel_cov_input)
str(rel_cov_input_df)

rel_cov_input_df[,6:14] <- lapply(rel_cov_input_df[,6:14], as.numeric)

rel_cov_input_df$other <- rel_cov_input_df$other + rel_cov_input_df$sponge
rel_cov_input_df <- rel_cov_input_df[,-13]

for(i in 6:13) {  
  rel_cov_input_df <- rel_cov_input_df %>%
    group_by(site, depth)
    mutate(sd = round(sd(rel_cov_input_df[,i]),0))# for-loop over columns
}

class(rel_cov_input_df)

rel_cov_input_df <- as.data.frame(rel_cov_input_df)

class(rel_cov_input_df)
str(rel_cov_input_df)


rel_cov_input_df[,c(1:5)] <- lapply(rel_cov_input_df[,c(1:5)], as.factor)

str(rel_cov_input_df)

rel_cov_input_df <- rel_cov_input_df %>%
  group_by(site, depth) %>% 
  mutate(sd_alg = round(sd(algae),2),
         sd_boulder = round(sd(boulder),2),
         sd_hc = round(sd(hard_coral),2),
         sd_other = round(sd(other),2),
         sd_rubble = round(sd(rubble),2),
         sd_sand = round(sd(sand),2),
         sd_softc = round(sd(soft_coral),2),
         sd_zoanth = round(sd(zoanthids),2))


library("writexl")
write_xlsx(rel_cov_input_df,"C:/Users/claud/Documents/COMUNE/STUDIO.LAVORO/IMBRSea/Thesis/PLITs/rel_cov_input.xlsx")






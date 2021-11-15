
###Load data
library(psych)
library(tidyverse)
library(jtools)
library(huxtable)
library(haven)
library(car)
library(broom)
library(officer)
library(flextable)
library(interactions)
library(readxl)

## Stacy Code
Stacy_MTurk <- read_excel("~Data/Stacy.MTurk.xlsx")
View(Stacy_MTurk)

#Key list#
key.list <- list(Narc=c("NARQ1","NARQ2", "NARQ3", "NARQ4", "NARQ5", "NARQ6", "NARQ7", "NARQ8", "NARQ9", "NARQ10",  
                        "NARQ11", "NARQ12", "NARQ13", "NARQ14", "NARQ15", "NARQ16", "NARQ17", "NARQ18"),
                 Psycho = c("SRP_1", "SRP3", "SRP4", "SRP5", "SRP6", "SRP7", "SRP8", "SRP9", "SRP10",
                            "SRP11", "SRP12", "SRP13", "SRP14", "SRP15", "SRP16", "SRP17", "SRP18", "SRP19", 
                            "SRP20", "SRP21", "SRP22", "SRP23", "SRP24", "SRP25", "SRP26", "SRP27", "SRP28",
                            "SRP29", "SRP30", "SRP31", "SRP32", "SRP33", "SRP34", "SRP35", "SRP36", "SRP37",
                            "SRP38", "SRP39", "SRP40"),
                 Mach = c("MPS1", "MPS2", "MPS3", "MPS4", "MPS5", "MPS6", "MPS7", "MPS8",
                          "MPS9", "MPS10", "MPS11", "MPS12", "MPS13", "MPS14", "MPS15", "MPS16"),
                 
                 ##Relevance##
                 Adversity = c("SIT7", "SIT8", "SIT9"),
                 Deception = c("SIT19","SIT20", "SIT21"),
                 Sociability = c("SIT22", "SIT23",	"SIT24"),
                 
                 ##Restraint##
                 Duty = c("SIT1", "SIT2", "SIT3"),
                 Account = c("ACC1", "ACC2", "ACC3", "ACC4", "ACC5", "ACC6", "ACC7", "ACC8"),
                 
                 ##Regulation##
                 Burnout = c("WS1", "WS2", "WS3", "WS4", "WS5", "WS6", "WS7", "WS8", "WS9"),
                 PsycCon = c("PC1", "PC2", "PC3", "PC4", "PC5"),
                 OrgJust = c("OJ1", "OJ2", "OJ3", "OJ4", "OJ5", "OJ6", "OJ7", "OJ8",
                             "OJ9", "OJ10", "OJ11", "OJ12", "OJ13", "OJ14", "OJ15", "OJ16",
                             "OJ17", "OJ18", "OJ19", "OJ20"),
                 Negativity = c("SIT16", "SIT17", "SIT18"),
                 Positivity = c("SIT13", "SIT14", "SIT15"),
                 
                 ##Resourceful##
                 Power = c("POW1",	"-POW2",	"POW3",	"-POW4",	"POW5",	"-POW6",	"-POW7",	"POW8"),
                 
                 
                 CWB = c("CWB1", "CWB2", "CWB3", "CWB4", "CWB5", "CWB6", "CWB7",
                         "CWB8", "CWB9", "CWB10", "CWB11", "CWB12", "CWB13",
                         "CWB14", "CWB15", "CWB16", "CWB17", "CWB18", "CWB19",
                         "CWB20", "CWB21", "CWB22", "CWB23", "CWB24", "CWB25",
                         "CWB26", "CWB27", "CWB28", "CWB29", "CWB30"))

#Item analysis#

narcissism <- select(Stacy_MTurk, NARQ1:NARQ9, NARQ10:NARQ18)
psychopathy <- select(Stacy_MTurk, SRP_1:SRP40)
Machia <- select(Stacy_MTurk, MPS1:MPS16)
Duty <- select(Stacy_MTurk, SIT1:SIT3)
Adverse <- select(Stacy_MTurk, SIT7:SIT9)
Posit <- select(Stacy_MTurk, SIT13:SIT15)
Negati <- select(Stacy_MTurk, SIT16:SIT18)
Decep <- select(Stacy_MTurk, SIT19:SIT21)
Socia <- select(Stacy_MTurk, SIT22:SIT24)
Account <- select(Stacy_MTurk, ACC1:ACC8)
Power <- select(Stacy_MTurk, POW1:POW8)
Burnout <- select(Stacy_MTurk, WS1:WS9)
PsycCon <- select(Stacy_MTurk, PC1:PC5)
OrgJus <- select(Stacy_MTurk, OJ1:OJ20)
CWB <- select(Stacy_MTurk, CWB1, CWB2:CWB17, CWB18:CWB30)

psych::alpha(CWB)
psych::alpha(Power, keys = c(1, -1, 1, -1, 1, -1, -1, 1))

###Make scores###
keys <- make.keys(235,key.list,item.labels = colnames(Stacy_MTurk))
DarkTriad2 <- scoreItems(keys, Stacy_MTurk)
DarkTriad2 <- as.data.frame(DarkTriad2$scores)

###Descriptives and correlations###

Samp1 <- DarkTriad2 %>% select(Narc:Mach, Adversity, Deception, Duty, Account, Burnout,
                               OrgJust, Power, CWB)

library(apaTables)
apa.cor.table(na.omit(Samp1), filename = "correlations1.doc",
              table.number = 1, show.conf.interval = FALSE,
              landscape = TRUE)

##Interactions##

##Relevance##
n37 <- lm(CWB ~ Narc*Adversity, data = DarkTriad2)
n38 <- lm(CWB ~ Narc*Deception, data = DarkTriad2)

p37 <- lm(CWB ~ Psycho*Adversity, data = DarkTriad2)
p38 <- lm(CWB ~ Psycho*Deception, data = DarkTriad2)

m37 <- lm(CWB ~ Mach*Adversity, data = DarkTriad2)
m38 <- lm(CWB ~ Mach*Deception, data = DarkTriad2)

#Restraint##
n39 <- lm(CWB ~ Narc*Duty, data = DarkTriad2)
n40 <- lm(CWB ~ Narc*Account, data = DarkTriad2)

p39 <- lm(CWB ~ Psycho*Duty, data = DarkTriad2)
p40 <- lm(CWB ~ Psycho*Account, data = DarkTriad2)

m39 <- lm(CWB ~ Mach*Duty, data = DarkTriad2)
m40 <- lm(CWB ~ Mach*Account, data = DarkTriad2)


##Regulation##
n41 <- lm(CWB ~ Narc*Burnout, data = DarkTriad2)
n42 <- lm(CWB ~ Narc*PsycCon, data = DarkTriad2)
n43 <- lm(CWB ~ Narc*OrgJust, data = DarkTriad2)
n44 <- lm(CWB ~ Narc*Negativity, data = DarkTriad2)

p41 <- lm(CWB ~ Psycho*Burnout, data = DarkTriad2)
p42 <- lm(CWB ~ Psycho*PsycCon, data = DarkTriad2)
p43 <- lm(CWB ~ Psycho*OrgJust, data = DarkTriad2)
p44 <- lm(CWB ~ Psycho*Negativity, data = DarkTriad2)

m41 <- lm(CWB ~ Mach*Burnout, data = DarkTriad2)
m42 <- lm(CWB ~ Mach*PsycCon, data = DarkTriad2)
m43 <- lm(CWB ~ Mach*OrgJust, data = DarkTriad2)
m44 <- lm(CWB ~ Mach*Negativity, data = DarkTriad2)

##Resourceful##
n45 <- lm(CWB ~ Narc*Power, data = DarkTriad2)
p45 <- lm(CWB ~ Psycho*Power, data = DarkTriad2)
m45 <- lm(CWB ~ Mach*Power, data = DarkTriad2)

###Use broom and purr to summrize model output (might also use gather to run all models)

models2 <- (list(n37 = n37, n38 = n38, n39 = n39, n40 = n40, n41 = n41, n42 = n42, n43 = n43,
                 n44 = n44, p37 = p37, p38 = p38, p39 = p39, p40 = p40, p41 = p41, p42 = p42, 
                 p43 = p43, p44 = p44, p45 = p45, m37 = m37, m38 = m38, m39 = m39, m40 = m40, 
                 m41 = m41, m42 = m42, m43 = m43, m44 = m44, m45 = m45))

modsum2 <- purrr::map_df(models2, broom::tidy, .id = "model")  ##Tidy output from list above
modsum2 <- modsum2 %>% mutate_if(is.numeric, round, digits = 3) ###Round p-values to 3 digits 
modsum.int2 <- modsum2[seq(4, nrow(modsum2), 4),] ###Subset only interaction terms (every 4th row)
modsum.int2 <- modsum.int2 %>% mutate(p.value2 = p.value/2) ##one-tailed tests

p.adjust(modsum.int2$p.value, method = "bonferroni", n = length(modsum.int2$p.value))

###jtool version to create tables and summarize output###

summ(p38, scale = TRUE)

export_summs( scale = TRUE, to.file = "docx",
              file.name = "Table1DarkTriad.docx")

export_summs(n37, n38, n1, n2, p37, p38, p1, p2, m37, m38, m1, m2,  n3, n11, n111, p3, p11, p111, m3, m11, m111, scale = TRUE, to.file = "docx",
             file.name = "Table1DarkTriadS1.docx")

export_summs(n39, n40, n5, n6, p39, p40, p5, p6, m39, m40, m5, m6, n7, n8, n9, n10, p7, p8, p9, p10, m7, m8, m9, m10, scale = TRUE, to.file = "docx",
             file.name = "Table1DarkTriadS2.docx")

export_summs(n41, n44, n43, n29, n222, n21, p41, p44, p43, p29, p222, p21, m41, m44, m43, m29, m222, m21, n24, n25, p24, p25, m24, m25, scale = TRUE, to.file = "docx",
             file.name = "Table1DarkTriadS3.docx")

export_summs(n45, n333, p45, p333, m45, m333, n33, n34, p33, p34, m33, m34, scale = TRUE, to.file = "docx",
             file.name = "Table1DarkTriadS4.docx")
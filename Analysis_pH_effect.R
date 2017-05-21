
#Point out the working folder
setwd ("/Users/JiayiQin/Dropbox/ashback/bios/single-species test/pH")
#Import datafile
Test <- read.table( file = "pH effect test.csv", header=TRUE, sep =",", dec =".")
Test

summary (Test)

# It's important to tell R where does the vector come from(data$vector)


ASConc0 <- Test$ASConc
ASpH0<- Test$ASpH
ASAdult0 <- Test$ASAdult
ASConc<- ASConc0[c(-13:-24)]
ASpH<- ASpH0[c(-13:-24)]
ASAdult<- ASAdult0[c(-13:-24)]


MAConc <- Test$MAConc
MApH<- Test$MApH
MAAdult <- Test$MAAdult

CAConc <- Test$CAConc
CApH<- Test$CApH
CAAdult <- Test$CAAdult

ASConc 
ASpH
ASAdult 

MAConc 
MApH
MAAdult

CAConc 
CApH
CAAdult 


##############################binomial test for ash2 concentration/adult################################
Pro<-function(i){10-ASAdult}
respASConc<-cbind(ASAdult,Pro(ASAdult))
respASConc

binomial1 <- glm(respASConc~0+factor(ASConc),family= binomial(link="cloglog") )


# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(ASConc) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)


##############################binomial test for ash2 pH/adult################################
Pro<-function(i){10-ASAdult}
respASpH<-cbind(ASAdult,Pro(ASAdult))
respASpH

binomial1 <- glm(respASpH~0+factor(ASpH),family= binomial(link="cloglog") )


# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(ASpH) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)


##############################binomial test for Ca(OH)2 concentration/adult################################
Pro<-function(i){10-CAAdult}
respCAConc<-cbind(CAAdult,Pro(CAAdult))
respCAConc

binomial1 <- glm(respCAConc~0+factor(CAConc),family= binomial(link="cloglog") )


# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(CAConc) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)


##############################binomial test for Mother ash concentration/adult################################
Pro<-function(i){10-MAAdult}
respMAConc<-cbind(MAAdult,Pro(MAAdult))
respMAConc

binomial1 <- glm(respMAConc~0+factor(MAConc),family= binomial(link="cloglog") )


# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(MAConc) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)

##############################binomial test for Mother ash pH/adult################################
Pro<-function(i){10-MAAdult}
respMApH<-cbind(MAAdult,Pro(MAAdult))
respMApH

binomial1 <- glm(respMApH~0+factor(MApH),family= binomial(link="cloglog") )


# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(MApH) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)

###############################Test for significance ASConc/Adult#######################################
BINOMIAL <- function(i){
  
  
  binomial0 <- glm(i~ 0+factor(ASConc),family= binomial)
  a <- summary(binomial0)
  print(a)
  
  binomial1 <- glm (i ~ factor(ASConc), family = binomial)
  b <- summary(binomial1)
  print (b)
  
  binomial2 <- glm (i ~ ASConc, family = binomial)
  c <- summary(binomial2)
  print(c)
  
  l <- anova (binomial0, binomial1, test="Chisq")
  print(l)
  m <- anova (binomial1, binomial2, test="Chisq")
  print(m)
  x<-anova (binomial0, test="Chisq")
  print(x)
  y<-anova (binomial1, test="Chisq")
  print(y)
  z<-anova (binomial2, test="Chisq")
  print(z)
  
}

BINOMIAL(respASConc)
#################Turkey and Dunnett test ASConc/Adult######################
library(multcomp)

FASConc <- factor (AS$ASConc)
ftestASConc <-aov(AS$ASAdult ~ FASConc )
summary(ftestASConc)

dunnett.ASConc<-glht(ftestASConc,linfct=mcp(FASConc="Dunnett"),alternative = "less")
summary(dunnett.ASConc)

Tukey.ASConc<-glht(ftestASConc,linfct=mcp(FASConc="Tukey"),alternative = "less")
summary(Tukey.ASConc)

###############################Test for significance ASpH/Adult#######################################
respASpH
BINOMIAL <- function(i){
  
  
  binomial0 <- glm(i~ 0+factor(ASpH),family= binomial)
  a <- summary(binomial0)
  print(a)
  
  binomial1 <- glm (i ~ factor(ASpH), family = binomial)
  b <- summary(binomial1)
  print (b)
  
  binomial2 <- glm (i ~ ASpH, family = binomial)
  c <- summary(binomial2)
  print(c)
  
  l <- anova (binomial0, binomial1, test="Chisq")
  print(l)
  m <- anova (binomial1, binomial2, test="Chisq")
  print(m)
  x<-anova (binomial0, test="Chisq")
  print(x)
  y<-anova (binomial1, test="Chisq")
  print(y)
  z<-anova (binomial2, test="Chisq")
  print(z)
  
}

BINOMIAL(respASpH)
#################Turkey and Dunnett test ASpH/Adult######################
FASpH<- factor (ASpH)
ftestASpH<-aov(ASAdult ~ FASpH)
summary(ftestASpH)

dunnett.ASpH<-glht(ftestASpH,linfct=mcp(FASpH="Dunnett"),alternative = "less")
summary(dunnett.ASpH)

Tukey.ASpH<-glht(ftestASpH,linfct=mcp(FASpH="Tukey"),alternative = "less")
summary(Tukey.ASpH)

###############################Test for significance CAConc/Adult#######################################
BINOMIAL <- function(i){
  
  
  binomial0 <- glm(i~ 0+factor(CAConc),family= binomial)
  a <- summary(binomial0)
  print(a)
  
  binomial1 <- glm (i ~ factor(CAConc), family = binomial)
  b <- summary(binomial1)
  print (b)
  
  binomial2 <- glm (i ~ CAConc, family = binomial)
  c <- summary(binomial2)
  print(c)
  
  l <- anova (binomial0, binomial1, test="Chisq")
  print(l)
  m <- anova (binomial1, binomial2, test="Chisq")
  print(m)
  x<-anova (binomial0, test="Chisq")
  print(x)
  y<-anova (binomial1, test="Chisq")
  print(y)
  z<-anova (binomial2, test="Chisq")
  print(z)
  
}

BINOMIAL(respCAConc)
#################Turkey and Dunnett test CAConc/Adult######################
FCAConc<- factor (Test$CAConc)
ftestCAConc<-aov(CAAdult ~ FCAConc)
summary(ftestCAConc)

dunnett.CAConc<-glht(ftestCAConc,linfct=mcp(FCAConc="Dunnett"),alternative = "less")
summary(dunnett.CAConc)

Tukey.CAConc<-glht(ftestCAConc,linfct=mcp(FCAConc="Tukey"),alternative = "less")
summary(Tukey.CAConc)

###############################Test for significance CApH/Adult#######################################

BINOMIAL <- function(i){
  
  
  binomial0 <- glm(i~ 0+factor(CApH),family= binomial)
  a <- summary(binomial0)
  print(a)
  
  binomial1 <- glm (i ~ factor(CApH), family = binomial)
  b <- summary(binomial1)
  print (b)
  
  binomial2 <- glm (i ~ CApH, family = binomial)
  c <- summary(binomial2)
  print(c)
  
  l <- anova (binomial0, binomial1, test="Chisq")
  print(l)
  m <- anova (binomial1, binomial2, test="Chisq")
  print(m)
  x<-anova (binomial0, test="Chisq")
  print(x)
  y<-anova (binomial1, test="Chisq")
  print(y)
  z<-anova (binomial2, test="Chisq")
  print(z)
  
}

BINOMIAL(respCApH)

#################Turkey and Dunnett test CApH/Adult######################
FCApH<- factor (Test$CApH)
ftestCApH<-aov(CAAdult ~ FCApH)
summary(ftestCApH)

dunnett.CApH<-glht(ftestCApH,linfct=mcp(FCApH="Dunnett"),alternative = "less")
summary(dunnett.CApH)

Tukey.CApH<-glht(ftestCApH,linfct=mcp(FCApH="Tukey"),alternative = "less")
summary(Tukey.CApH)

###############################Test for significance MAConc/Adult#######################################
BINOMIAL <- function(i){
  
  
  binomial0 <- glm(i~ 0+factor(MAConc),family= binomial)
  a <- summary(binomial0)
  print(a)
  
  binomial1 <- glm (i ~ factor(MAConc), family = binomial)
  b <- summary(binomial1)
  print (b)
  
  binomial2 <- glm (i ~ MAConc, family = binomial)
  c <- summary(binomial2)
  print(c)
  
  l <- anova (binomial0, binomial1, test="Chisq")
  print(l)
  m <- anova (binomial1, binomial2, test="Chisq")
  print(m)
  x<-anova (binomial0, test="Chisq")
  print(x)
  y<-anova (binomial1, test="Chisq")
  print(y)
  z<-anova (binomial2, test="Chisq")
  print(z)
  
}

BINOMIAL(respMAConc)

#################Turkey and Dunnett test MAConc/Adult######################
FMAConc<- factor (Test$MAConc)
ftestMAConc<-aov(MAAdult ~ FMAConc)
summary(ftestMAConc)

dunnett.MAConc<-glht(ftestMAConc,linfct=mcp(FMAConc="Dunnett"),alternative = "less")
summary(dunnett.MAConc)

Tukey.MAConc<-glht(ftestMAConc,linfct=mcp(FMAConc="Tukey"),alternative = "less")
summary(Tukey.MAConc)


###############################Test for significance MApH/Adult#######################################

BINOMIAL <- function(i){
  
  
  binomial0 <- glm(i~ 0+factor(MApH),family= binomial)
  a <- summary(binomial0)
  print(a)
  
  binomial1 <- glm (i ~ factor(MApH), family = binomial)
  b <- summary(binomial1)
  print (b)
  
  binomial2 <- glm (i ~ MApH, family = binomial)
  c <- summary(binomial2)
  print(c)
  
  l <- anova (binomial0, binomial1, test="Chisq")
  print(l)
  m <- anova (binomial1, binomial2, test="Chisq")
  print(m)
  x<-anova (binomial0, test="Chisq")
  print(x)
  y<-anova (binomial1, test="Chisq")
  print(y)
  z<-anova (binomial2, test="Chisq")
  print(z)
  
}

BINOMIAL(respMApH)
#################Turkey and Dunnett test MApH/Adult######################
FMApH<- factor (Test$MApH)
ftestMApH<-aov(MAAdult ~ FMApH)
summary(ftestMApH)

dunnett.MApH<-glht(ftestMApH,linfct=mcp(FMApH="Dunnett"),alternative = "less")
summary(dunnett.MApH)

Tukey.MApH<-glht(ftestMApH,linfct=mcp(FMApH="Tukey"),alternative = "less")
summary(Tukey.MApH)


###########################################################################################################
###########################################################################################################
####################################Dose-response Curve   ASConc####################################
library (drc)

AS <-  subset(Test, ASConc >= 0, select=c(ASConc:ASAdult))
AS

UPPER<-max (ASAdult)
UPPER

p_ASAdult <- AS2$ASAdult / 10

fitASConc<- drm(p_ASAdult~AS2$ASConc,
           data=AS2,
           fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
           control=drmc(),
           lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
           upperl = c(Inf, Inf, 1, Inf),
           type="binomial"
)

summary(fitASConc)
ED(fitASConc,c(10,50,90),interval="delta")
####################################Draw plots####################################
Treatment

plot(fitASConc,#y~x, expression for x and y axis
     # broken=TRUE,
     log="",#log x or y axis. If none of them needed, put ""
     xlab = "Concentration of ash (g/kg)",#Lable for x axis
     ylab = "Number of adults",# Lable for y axis
     xlim = c(0, 75),# The range for x axis
     # ylim = c(0,1000),#The range for y axis
     xaxs = "i",# The number level of axis, i means according to setting, r means it goes with the range of custome data
     yaxs = "r",
     col = "black", #color for graph
     pch = 21,#Options for dots style
     type="all", # dot/line /all
     #cex=1, cex.axis=1.2, cex.lab=1.2, 
     lwd=2)

#####################################################################################################
####################################Dose-response Curve   MAConc####################################
library (drc)

UPPER<-max (MAAdult)
UPPER

#MA<- cbind (MAConc,MAAdult)
#MA
#MA <-rbind(MA,c("200","0"))
#MA <- as.data.frame.matrix(MA)
#MA
#MA$MAConc
#adult<-as.numeric(paste(MA$MAAdult))
#conc<-as.numeric(paste(MA$MAConc))
#adult
#conc

p_MAAdult <- Test$MAAdult*0.1

fitMAConc<- drm(p_MAAdult ~ MAConc,
                
                fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                control=drmc(),
                lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                upperl = c(Inf, Inf, 1, Inf),
                type="binomial"
)

summary(fitMAConc)
ED(fitMAConc,c(10,50,90),interval="delta")

####################################Draw plots####################################
MAConc

plot(fitMAConc,#y~x, expression for x and y axis
     # broken=TRUE,
     log="",#log x or y axis. If none of them needed, put "" on
     main="Survival of O.yodai",#main title of the graph
     xlab = "Concentration of ash( g/kg)",#Lable for x axis
     ylab = "Numbers of Adult",# Lable for y axis
     xlim = c(0, 75),# The range for x axis
     # ylim = c(0,1000),#The range for y axis
     
     xaxs = "i",# The number level of axis, i means according to setting, r means it goes with the range of custome data
     yaxs = "r",
     col = "black", #color for graph
     pch = 21,#Options for dots style
     type="all", # dot/line /all
     #cex=1, cex.axis=1.2, cex.lab=1.2, 
     lwd=2)

#####################################################################################################
####################################Dose-response Curve   CAConc####################################
library (drc)

UPPER<-max (CAAdult)
UPPER
p_CAAdult <- CAAdult/10

fitCAConc<- drm( p_CAAdult~CAConc,
                data=Test,
                fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                control=drmc(),
                lowerl = c(-Inf, -Inf, -Inf, -Inf), #Limitation for parameters
                upperl = c(Inf, Inf, 1, Inf),
                type="binomial"
)

summary(fitCAConc)
ED(fitCAConc,c(10,50,90),interval="delta")
####################################Draw plots####################################
CAConc
plot(fitCAConc,#y~x, expression for x and y axis
     # broken=TRUE,
     log="",#log x or y axis. If none of them needed, put "" on
     main="Survival of O.yodai",#main title of the graph
     xlab = "Concentration of Ca(OH)2( g/kg)",#Lable for x axis
     ylab = "Numbers of Adult",# Lable for y axis
     xlim = c(0, 15),# The range for x axis
     # ylim = c(0,1000),#The range for y axis
     
     xaxs = "i",# The number level of axis, i means according to setting, r means it goes with the range of custome data
     yaxs = "r",
     col = "black", #color for graph
     pch = 21,#Options for dots style
     type="all", # dot/line /all
     #cex=1, cex.axis=1.2, cex.lab=1.2, 
     lwd=2)

#############################################################################################################
####################################Dose-response Curve   ASpH####################################
library (drc)

AS <-  subset(Test, ASConc >= 0, select=c(ASConc:ASAdult))
AS2 <- rbind(AS,c(90,11,0))

AS2

UPPER<-max (ASAdult)
UPPER

ASAdult

fitASpH<- drm(p_ASAdult~AS2$ASpH,
                data=AS2,
                fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                control=drmc(),
                lowerl = c(-Inf, -Inf, -Inf, -Inf), #Limitation for parameters
                upperl = c(Inf, 0, 1, Inf),
                type="binomial"
)

summary(fitASpH)
ED(fitASpH,c(10,50,90),interval="delta")
####################################Draw plots####################################
ASpH

plot(fitASpH,#y~x, expression for x and y axis
     # broken=TRUE,
     log="",#log x or y axis. If none of them needed, put "" on
     # main="O. yodai",#main title of the graph
     xlab = "Soil pH",#Lable for x axis
     ylab = "Number of adults",# Lable for y axis
     xlim = c(6, 10),# The range for x axis
     # ylim = c(0,1000),#The range for y axis
     
     xaxs = "i",# The number level of axis, i means according to setting, r means it goes with the range of custome data
     yaxs = "r",
     col = "black", #color for graph
     pch = 21,#Options for dots style
     type="all", # dot/line /all
     #cex=1, cex.axis=1.2, cex.lab=1.2, 
     lwd=2)

#####################################################################################################
####################################Dose-response Curve   MApH####################################
library (drc)

UPPER<-max (MAAdult)
UPPER

MAAdult

fitMApH<- drm(p_MAAdult~MApH,
                data=Test,
                fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                control=drmc(),
                lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                upperl = c(Inf, Inf, 1, Inf),
                type="binomial"
)

summary(fitMApH)
ED(fitMApH,c(10,50,90),interval="delta")
####################################Draw plots####################################
MApH
par(new=TRUE)
plot(fitMApH,#y~x, expression for x and y axis
     # broken=TRUE,
     log="",#log x or y axis. If none of them needed, put "" on
     xlim = c(6, 10),# The range for x axis
     xaxs = "i",# The number level of axis, i means according to setting, r means it goes with the range of custome data
     yaxs = "r",
     col = "black", #color for graph
     pch = 21,#Options for dots style
     type="all", # dot/line /all
     #cex=1, cex.axis=1.2, cex.lab=1.2, 
     lwd=2)

#####################################################################################################
####################################Dose-response Curve   CApH####################################
library (drc)

UPPER<-max (CAAdult)
UPPER


fitCApH<- drm(p_CAAdult~CApH,
                data=Test,
                fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                control=drmc(),
                lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                upperl = c(Inf, Inf, 1, Inf),
              type="binomial"
)

summary(fitCApH)
ED(fitCApH,c(10,50,90),interval="delta")
####################################Draw plots####################################
CApH
plot(fitCApH,#y~x, expression for x and y axis
     # broken=TRUE,
     log="",#log x or y axis. If none of them needed, put "" on
     main="Survival of O.yodai",#main title of the graph
     xlab = "pH of soil caused by Ca(OH)2 (g/kg)",#Lable for x axis
     ylab = "Numbers of Adult",# Lable for y axis
     xlim = c(7, 11.5),# The range for x axis
     # ylim = c(0,1000),#The range for y axis
     
     xaxs = "i",# The number level of axis, i means according to setting, r means it goes with the range of custome data
     yaxs = "r",
     col = "black", #color for graph
     pch = 21,#Options for dots style
     type="all", # dot/line /all
     #cex=1, cex.axis=1.2, cex.lab=1.2, 
     lwd=2)

anova(fitCApH,fitMApH, test="F")
glm(fitCApH,fitMApH, family="binomal")

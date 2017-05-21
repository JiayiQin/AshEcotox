PersonResiduals<-residuals(LinerRegression,"pearson")
#Point out the working folder
setwd ("/Users/JiayiQin/Dropbox/ASHBACK/BIOS/single-species test")
#Import datafile
SUM <- read.table( file = "Briefly summary.csv", header=TRUE, sep =",", dec =".")
SUM
summary (SUM)

#############################Foulum-soil Definitive test Folsomia candida#########################
# create working dataset with subset()
FouDefFol <- subset(SUM, Soil.type == "Foulum soil" &
                      Test.type == "definitive" &
                      Species == "Folsomia candida", select=Species:Juvenile,drop=FALSE) 

FouDefFol

# It's important to tell R where does the vector come from(data$vector)
FouDefFol_Conc <- FouDefFol$Concentration

FouDefFol_Adult<- FouDefFol$Adult

FouDefFol_Juvenile <- FouDefFol$Juvenile

FouDefFol_pH<- FouDefFol$pH

NormalDistribution(FouDefFol_Juvenile)

##################### Foulum Folsomia Candida  pH      Turkey and Dunnett test######################
FouDefFol2 <- subset (FouDefFol, FouDefFol$pH>0,select=Species:Juvenile,drop=FALSE)
FouDefFol2 

FFouDefFol_pH<- factor (FouDefFol2$pH)
ftestFolJuv_pH<-aov(FouDefFol2$Juvenile ~ FFouDefFol_pH)
summary(ftestFolJuv_pH)

Dunnett.FolJuv_pH<-glht(ftestFolJuv_pH,linfct=mcp(FFouDefFol_pH="Dunnett"),alternative = "less")
summary(Dunnett.FolJuv_pH)

Tukey.FolJuv_pH<-glht(ftestFolJuv_pH,linfct=mcp(FFouDefFol_pH="Tukey"),alternative = "less")
summary(Tukey.FolJuv_pH)

#####################Foulum Folsomia Candida  pH      EC 50######################
library (drc)

FouDefFol2 

UPPER<-max (FouDefFol2$Juvenile )
UPPER



fitFolJuv_pH<- drm(FouDefFol2$Juvenile~FouDefFol2$pH,
                data=FouDefFol2,
                fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                control=drmc(),
                lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                upperl = c(Inf, Inf, UPPER, Inf)
)

summary(fitFolJuv_pH)
ED(fitFolJuv_pH,c(10,50,90),interval="delta")

##################### Foulum Folsomia Candida  Concentration      Turkey and Dunnett test######################


FFouDefFol_Conc<- factor (FouDefFol_Conc)
ftestFolJuv_Conc<-aov(FouDefFol_Juvenile ~ FFouDefFol_Conc)
summary(ftestFolJuv_Conc)

Dunnett.FolJuv_Conc<-glht(ftestFolJuv_Conc,linfct=mcp(FFouDefFol_Conc="Dunnett"),alternative = "less")
summary(Dunnett.FolJuv_Conc)

Tukey.FolJuv_Conc<-glht(ftestFolJuv_Conc,linfct=mcp(FFouDefFol_Conc="Tukey"),alternative = "less")
summary(Tukey.FolJuv_Conc)

#####################Foulum Folsomia Candida  Concentration      EC 50######################
library (drc)

UPPER<-max (FouDefFol_Juvenile )
UPPER

fitFolJuv_Conc<- drm(FouDefFol_Juvenile ~ FouDefFol_Conc,
                   data=FouDefFol,
                   fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                   control=drmc(),
                   lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                   upperl = c(Inf, Inf, UPPER, Inf)
)

summary(fitFolJuv_Conc)
ED(fitFolJuv_Conc,c(10,50,90),interval="delta")
###################################################################################################
#############################Foulum-soil Definitive test Onychiurus yodai#########################
# create working dataset with subset()
FouDefOny <- subset(SUM, Soil.type == "Foulum soil" &
                      Test.type == "definitive" &
                      Species == "Onychiurus yodai", select=Species:Juvenile,drop=FALSE) 

FouDefOny

# It's important to tell R where does the vector come from(data$vector)
FouDefOny_Conc <- FouDefOny$Concentration

FouDefOny_Adult<- FouDefOny$Adult

FouDefOny_Juvenile <- FouDefOny$Juvenile

FouDefOny_pH<- FouDefOny$pH
NormalDistribution(FouDefOny_Juvenile[0:16])

##################### Foulum Onychiurus yodai  pH      Turkey and Dunnett test######################
 

FFouDefOny_pH<- factor (FouDefOny_pH)
ftestOnyJuv_pH<-aov(FouDefOny_Juvenile[0:15] ~ FFouDefOny_pH[0:15])
summary(ftestOnyJuv_pH)

Dunnett.OnyJuv_pH<-glht(ftestOnyJuv_pH,linfct=mcp(FFouDefOny_pH="Dunnett"),alternative = "less")
summary(Dunnett.OnyJuv_pH)

Tukey.OnyJuv_pH<-glht(ftestOnyJuv_pH,linfct=mcp(FFouDefOny_pH="Tukey"),alternative = "less")
summary(Tukey.OnyJuv_pH)

FFouDefOny_pH<- factor (FouDefOny_pH)
ftestOnyAdult_pH<-aov(FouDefOny_Adult~ FFouDefOny_pH)
summary(ftestOnyJuv_pH)

Dunnett.OnyAdult_pH<-glht(ftestOnyAdult_pH,linfct=mcp(FFouDefOny_pH="Dunnett"),alternative = "less")
summary(Dunnett.OnyAdult_pH)

Tukey.OnyAdult_pH<-glht(ftestOnyAdult_pH,linfct=mcp(FFouDefOny_pH="Tukey"),alternative = "less")
summary(Tukey.OnyAdult_pH)

#####################Foulum Onychiurus  pH      EC 50######################
library (drc)

UPPER<-max (FouDefOny_Juvenile )
UPPER

fitOnyJuv_pH<- drm(FouDefOny_Juvenile ~ FouDefOny_pH,
                   data=FouDefOny,
                   fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                   control=drmc(),
                   lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                   upperl = c(Inf, Inf, UPPER, Inf)
)

summary(fitOnyJuv_pH)
ED(fitOnyJuv_pH,c(10,50,90),interval="delta")


UPPER<-max (FouDefOny_Adult )
UPPER

p_FouDefOny_Adult <- FouDefOny_Adult/15

fitOnyAdult_pH<- drm(p_FouDefOny_Adult ~ FouDefOny_pH,
                   data=FouDefOny,
                   fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                   control=drmc(),
                   lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                   upperl = c(Inf, Inf, 1, Inf),
                   type ="binomial"
)

summary(fitOnyAdult_pH)
ED(fitOnyAdult_pH,c(10,50,90),interval="delta")
##############################################################################################################
##################### Foulum Onychiurus yodai Concentration      Turkey and Dunnett test######################


FFouDefOny_Conc<- factor (FouDefOny_Conc)
ftestOnyJuv_Conc<-aov(FouDefOny_Juvenile[0:15] ~ FFouDefOny_Conc[0:15])
summary(ftestOnyJuv_Conc)

Dunnett.OnyJuv_Conc<-glht(ftestOnyJuv_Conc,linfct=mcp(FFouDefOny_Conc="Dunnett"),alternative = "less")
summary(Dunnett.OnyJuv_Conc)

Tukey.OnyJuv_Conc<-glht(ftestOnyJuv_Conc,linfct=mcp(FFouDefOny_Conc="Tukey"),alternative = "less")
summary(Tukey.OnyJuv_Conc)


ftestOnyAdult_Conc<-aov(FouDefOny_Adult ~ FFouDefOny_Conc)
summary(ftestOnyAdult_Conc)

Dunnett.OnyAdult_Conc<-glht(ftestOnyAdult_Conc,linfct=mcp(FFouDefOny_Conc="Dunnett"),alternative = "less")
summary(Dunnett.OnyAdult_Conc)

Tukey.OnyAdult_Conc<-glht(ftestOnyAdult_Conc,linfct=mcp(FFouDefOny_Conc="Tukey"),alternative = "less")
summary(Tukey.OnyAdult_Conc)



#####################Foulum Onychiurus yodai  Concentration      EC 50######################
library (drc)

UPPER<-max (FouDefOny_Juvenile )
UPPER

fitOnyJuv_Conc<- drm(FouDefOny_Juvenile ~ FouDefOny_Conc,
                     data=FouDefOny,
                     fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                     control=drmc(),
                     lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                     upperl = c(Inf, Inf, UPPER, Inf)
)

summary(fitOnyJuv_Conc)
ED(fitOnyJuv_Conc,c(10,50,90),interval="delta")




fitOnyAdult_Conc<- drm(p_FouDefOny_Adult~ FouDefOny_Conc,
                     data=FouDefOny,
                     fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")),
                     control=drmc(),
                     lowerl = c(-Inf, 0, -Inf, -Inf), #Limitation for parameters
                     upperl = c(Inf, Inf, 1, Inf),
                     type="binomial"
)

summary(fitOnyAdult_Conc)
ED(fitOnyAdult_Conc,c(10,50,90),interval="delta")
#############################################Gedhus soil_Folsomia candida##################################################################

GedDefFol <- subset(SUM, Soil.type == "Gedhus soil" &
                      Test.type == "definitive" &
                      Species == "Folsomia candida", select=Species:Juvenile,drop=FALSE) 



# It's important to tell R where does the vector come from(data$vector)
GedDefFol_Conc <- GedDefFol$Concentration

GedDefFol_Adult<- GedDefFol$Adult

GedDefFol_Juvenile <- GedDefFol$Juvenile

GedDefFol_pH<- GedDefFol$pH

fitGedDefFol_Adult <- lm (GedDefFol_Adult~GedDefFol_Conc)

fitGedDefFol_Juvenile <- lm (GedDefFol_Juvenile~GedDefFol_Conc)

#############################################Gedhus soil_Onychiurus yodai##################################################################

GedDefOny <- subset(SUM, Soil.type == "Gedhus soil" &
                      Test.type == "definitive" &
                      Species == "Onychiurus yodai", select=Species:Juvenile,drop=FALSE) 



# It's important to tell R where does the vector come from(data$vector)
GedDefOny_Conc <- GedDefOny$Concentration

GedDefOny_Adult<- GedDefOny$Adult

GedDefOny_Juvenile <- GedDefOny$Juvenile

GedDefOny_pH<- GedDefOny$pH

fitGedDefOny_Adult <- lm (GedDefOny_Adult~GedDefOny_Conc)

fitGedDefOny_Juvenile <- lm (GedDefOny_Juvenile~GedDefOny_Conc)

glmGedDefOny_Juvenile<- glm(GedDefOny_Juvenile~GedDefOny_Conc)
summary(glmGedDefOny_Juvenile)
anova(glmGedDefOny_Juvenile)

##### Non parametric test for survival
nonparaGedDefOny_Adult<-kruskal.test(GedDefOny_Adult~GedDefOny_Conc)
#http://www.baike.com/wiki/%E9%9D%9E%E5%8F%82%E6%95%B0%E6%A3%80%E9%AA%8C
nonparaFouDefOny_Adult<-kruskal.test(FouDefOny_Adult~FouDefOny_Conc)
nonparaFouDefOny_Adult
nonparaGedDefOny_Adult

nonparaGedDefFol_Adult<-kruskal.test(GedDefFol_Adult~GedDefFol_Conc)
#http://www.baike.com/wiki/%E9%9D%9E%E5%8F%82%E6%95%B0%E6%A3%80%E9%AA%8C
nonparaFouDefFol_Adult<-kruskal.test(FouDefFol_Adult~FouDefFol_Conc)
nonparaFouDefFol_Adult
nonparaGedDefFol_Adult

nonparaGedDefOny_Juvenile<-kruskal.test(GedDefOny_Juvenile~GedDefOny_Conc)
nonparaGedDefOny_Juvenile

###############################################################################################################
###########################################Graphic for pH_O.yodai/F.candida#########################

par()
par(mfrow=c(2,2))   #一个图版显示2行，3列。
plotfitOnyAdult_pH <- plot(fitOnyAdult_pH,#y~x, expression for x and y axis
                      # broken=TRUE,
                      log="",#log x or y axis. If none of them needed, put "" on
                      main="Survival",#main title of the graph
                      xlab = "Soil pH",#Lable for x axis
                      ylab = "Number of Adults",# Lable for y axis
                      xlim = c(5,10),# The range for x axis
                      ylim = c(0,16),#The range for y axis
                      
                      xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                      yaxs = "i",
                      col = "black", #color for graph
                      pch = 21,#Options for dots style
                      type="all", # dot/line /all
                      #cex=1, cex.axis=1.2, cex.lab=1.2, 
                      lwd=2)
Upper<- max(FouDefOny_Juvenile)
Upper
plotfitOnyJuv_pH <- plot(fitOnyJuv_pH,#y~x, expression for x and y axis
                           # broken=TRUE,
                           log="",#log x or y axis. If none of them needed, put "" on
                           main="Juvenile",#main title of the graph
                           xlab = "Soil pH",#Lable for x axis
                           ylab = "Number of Juveniles",# Lable for y axis
                           xlim = c(5,10),# The range for x axis
                           ylim = c(0,1000),#The range for y axis
                           
                           xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                           yaxs = "i",
                           col = "black", #color for graph
                           pch = 21,#Options for dots style
                           type="all", # dot/line /all
                           #cex=1, cex.axis=1.2, cex.lab=1.2, 
                           lwd=2)
par(new=TRUE)
Upper<- max(FouDefFol_Juvenile)
Upper
plotfitFolJuv_pH <- plot(fitFolJuv_pH,#y~x, expression for x and y axis
                      # broken=TRUE,
                      log="", #log x or y axis. If none of them needed, put "" on
                      #main="Juvenile of O.yodai",#main title of the graph
                      xlab = "",#Lable for x axis
                      ylab = "",# Lable for y axis
                      xlim = c(5,10),# The range for x axis
                      ylim = c(0,1000),#The range for y axis
                      
                      xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                      yaxs = "i",
                      col = "red", #color for graph
                      pch = 22,#Options for dots style
                      type="all", # dot/line /all
                      #cex=1, cex.axis=1.2, cex.lab=1.2, 
                      lwd=2)


###########################################Graphic for Concentration_O.yodai/F.candida#########################
par(new=FALSE)

plotfitOnyAdult_Conc <- plot(fitOnyAdult_Conc,#y~x, expression for x and y axis
                           # broken=TRUE,
                           log="",#log x or y axis. If none of them needed, put "" on
                           main="",#main title of the graph
                           xlab = "Concentration of Wood Ash (g/kg)",#Lable for x axis
                           ylab = "Number of Adults",# Lable for y axis
                           xlim = c(0,80),# The range for x axis
                           ylim = c(0,16),#The range for y axis
                           
                           xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                           yaxs = "i",
                           col = "black", #color for graph
                           pch = 21,#Options for dots style
                           type="all", # dot/line /all
                           #cex=1, cex.axis=1.2, cex.lab=1.2, 
                           lwd=2)

par(new=FALSE)
plotfitOnyJuv_Conc <- plot(fitOnyJuv_Conc,#y~x, expression for x and y axis
                         # broken=TRUE,
                         log="",#log x or y axis. If none of them needed, put "" on
                         main="",#main title of the graph
                         xlab = "Concentration of Wood Ash (g/kg)",#Lable for x axis
                         ylab = "Number of Juveniles",# Lable for y axis
                         xlim = c(0,80),# The range for x axis
                         ylim = c(0,1000),#The range for y axis
                         
                         xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                         yaxs = "i",
                         col = "black", #color for graph
                         pch = 21,#Options for dots style
                         type="all", # dot/line /all
                         #cex=1, cex.axis=1.2, cex.lab=1.2, 
                         lwd=2)
par(new=TRUE)
Upper<- max(FouDefFol_Juvenile)
Upper
plotfitFolJuv_Conc <- plot(fitFolJuv_Conc,#y~x, expression for x and y axis
                         # broken=TRUE,
                         log="", #log x or y axis. If none of them needed, put "" on
                         main="",#main title of the graph
                         xlab = "",#Lable for x axis
                         ylab = "",# Lable for y axis
                         xlim = c(0,80),# The range for x axis
                         ylim = c(0,1000),#The range for y axis
                         
                         xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                         yaxs = "i",
                         col = "red", #color for graph
                         pch = 22,#Options for dots style
                         type="all", # dot/line /all
                         #cex=1, cex.axis=1.2, cex.lab=1.2, 
                         lwd=2)




par(new=TRUE)


legend(-3,3, c("Folsomia candida", "Onychiurus yodai"), 
       col = c( "black","red"),
       text.col = "black", 
       #lty = c(2, -1, 1),
       pch = c(21,22),
       bty="n")



###### Two_way ANOVA F.candida ###############
# All the output dont fit normal distribution, so it is not proper to use two-way ANOVA
# Hopeless for transformation
# Alternative choice is needed.

DefFol <- subset(SUM, Test.type == "definitive" &
                      Species == "Folsomia candida", select=Soil.type&Species:Juvenile,drop=FALSE) 

NormalDistribution <- function(i){
  
  st<- shapiro.test(i)
  print (st)
  # Shapiro-Wilk normality test, p与查表中的W alpha 比较. p< W的可能性>0.05,即在0.05水平，p >0.05 就认为数据不符合正态分布 p值小于0.05，数据为正态分布
  
  kt<- ks.test(i, "pnorm", mean = mean(i), sd = sqrt(var(i))) 
  # This test is used for big data set>5000
  #Kolmogorov-Smirnov检验需要三个输入变量，及数据本身、均值及标准差
  # p与查表中的D alpha 比较. p > D 的可能性>0.05, 即，在0.05水平，p >0.05 就认为数据符合正态分布
  print(kt)
}

TWANOVA_candida_1<- aov(DefFol$Juvenile~DefFol$Soil.type*factor(DefFol$Concentration))
   summary(TWANOVA_candida_1)
   
   anova(TWANOVA_candida_1)
   
TWANOVA_candida_2<- aov(DefFol$Juvenile~DefFol$Soil.type+factor(DefFol$Concentration))
   summary(TWANOVA_candida_2)   
   anova(TWANOVA_candida_2)
anova(TWANOVA_candida_1,TWANOVA_candida_2)
# significant different, so TWANOVA_candida_1 is proper
anova(TWANOVA_candida_1)

## Test Binomial distribution of Adult Folsomia candida  ####
FouDefFol_Adult
Pro<-function(i){10-FouDefFol_Adult}
respFol<-cbind(FouDefFol_Adult,Pro(FouDefFol_Adult))
respFol

binomial1 <- glm(respFol~0+factor(FouDefFol$Concentration),family= binomial(link="logit") )
# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(FouDefFol$Concentration) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Test the binomial distribution, p>0.5 fit.
# 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)

GedDefFol_Adult
Pro<-function(i){10-GedDefFol_Adult}
respFol<-cbind(GedDefFol_Adult,Pro(GedDefFol_Adult))
respFol

binomial1 <- glm(respFol~0+factor(GedDefFol$Concentration),family= binomial(link="cloglog") )
# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)

dev<-deviance(binomial1)
n.observations <- length(GedDefFol$Concentration) 
n.observations

n.parameters.fit1 <- length(coef(binomial1))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)


###### Two_way ANOVA O.yodai_juvenile ###############
DefOny <- subset(SUM, Test.type == "definitive" &
                   Species == "Onychiurus yodai", select=Soil.type&Species:Juvenile,drop=FALSE) 

NormalDistribution(GedDefOny_Juvenile)
hist(asin(GedDefOny_Juvenile/163))
x<- log(sqrt(GedDefOny_Juvenile)+10)
NormalDistribution(x)
hist(x)
y<- log(sqrt(FouDefOny_Juvenile)+10)
NormalDistribution(y[1:12])
hist(y)

j<-log(sqrt(DefOny$Juvenile)+10)
NormalDistribution(j)
TWANOVA_yodai_1<- aov(j~DefOny$Soil.type*factor(DefOny$Concentration))
summary(TWANOVA_yodai_1)
anova(TWANOVA_yodai_1)
TWANOVA_yodai_2<- aov(j~DefOny$Soil.type+factor(DefOny$Concentration))
summary(TWANOVA_yodai_2)   

anova(TWANOVA_yodai_1,TWANOVA_yodai_2)
# significant different, so TWANOVA_yodai_1 is proper
anova(TWANOVA_yodai_1)

#### test binomial distribution of Adult yodai #####
FouDefOny_Adult
Pro<-function(i){15-FouDefOny_Adult}
respOny<-cbind(FouDefOny_Adult,Pro(FouDefOny_Adult))
respOny

binomial1 <- glm(respOny~0+factor(FouDefOny$Concentration),family= binomial(link="cloglog") )
# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)
binomial2 <- glm(respOny~factor(FouDefOny$Concentration),family= binomial(link="cloglog") )
anova(binomial1,binomial2, test="Chisq")
binomial3 <- glm(respOny~FouDefOny$Concentration,family= binomial(link="cloglog") )
anova(binomial2,binomial3, test="Chisq")

dev<-deviance(binomial2)
n.observations <- length(FouDefOny$Concentration) #length of dose
n.observations

n.parameters.fit1 <- length(coef(binomial2))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.05符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)

GedDefOny_Adult
Pro<-function(i){15-GedDefOny_Adult}
respOny<-cbind(GedDefOny_Adult,Pro(GedDefOny_Adult))
respOny

binomial1 <- glm(respOny~0+factor(GedDefOny$Concentration),family= binomial(link="cloglog") )
# For binmial distribution, link has option: logit, probit, log, cloglog
summary(binomial1)
binomial2 <- glm(respOny~factor(GedDefOny$Concentration),family= binomial(link="cloglog") )
anova(binomial1,binomial2, test="Chisq")
binomial3 <- glm(respOny~GedDefOny$Concentration,family= binomial(link="cloglog") )
anova(binomial2,binomial3, test="Chisq")

dev<-deviance(binomial2)
n.observations <- length(GedDefOny$Concentration) 
n.observations

n.parameters.fit1 <- length(coef(binomial2))
n.parameters.fit1

n.df.fit1 <- n.observations - n.parameters.fit1
n.df.fit1

# Performing the test 检验是否符合二项分布, p>0.5符合二项分布
pchisq(dev, df=n.df.fit1, lower.tail=F)

################# non parametric two way ANOVA test for FolAdult #################
#Create a variable containing the ranks each variable
rank_FolAdult<-rank(DefFol$Adult)
rank_Folsoil<-rank(DefFol$Soil.type)
rank_FolConc<-rank(DefFol$Concentration)

# Do the test
aov_rank_FolAdult<- aov(rank_FolAdult~rank_Folsoil*rank_FolConc)
summary(aov_rank_FolAdult)

# extract the sum of squares and degrees of freedom information and sum both
Df <- anova(aov_rank_FolAdult)[,"Df"]
Sum_Df<- sum(Df)
SS <- anova(aov_rank_FolAdult)[,"Sum Sq"]
Sum_SS<- sum(SS)

# Calculate the MS value
MS <- Sum_SS/Sum_Df

# Calculate the H value
SS[1]/MS
SS[2]/MS
SS[3]/MS

# Convert into probability
p_DefFol_Soil<-1-pchisq(SS[1]/MS,Df[1])
p_DefFol_Conc<-1-pchisq(SS[2]/MS,Df[2])
p_DefFol_Soil.Conc<-1-pchisq(SS[3]/MS,Df[3])
p_DefFol_Soil
p_DefFol_Conc
p_DefFol_Soil.Conc
################# non parametric two way ANOVA test forOnyAdult #################
#Create a variable containing the ranks each variable
rank_OnyAdult<-rank(DefOny$Adult)
rank_Onysoil<-rank(DefOny$Soil.type)
rank_OnypH<-rank(DefOny$pH)

# Do the test
aov_rank_OnyAdult<- aov(rank_OnyAdult~rank_Onysoil*rank_OnypH)
summary(aov_rank_OnyAdult)

# extract the sum of squares and degrees of freedom information and sum both
Df <- anova(aov_rank_OnyAdult)[,"Df"]
Sum_Df<- sum(Df)
SS <- anova(aov_rank_OnyAdult)[,"Sum Sq"]
Sum_SS<- sum(SS)

# Calculate the MS value
MS <- Sum_SS/Sum_Df

# Calculate the H value
SS[1]/MS
SS[2]/MS
SS[3]/MS

# Convert into probability
p_DefOny_Soil<-1-pchisq(SS[1]/MS,Df[1])
p_DefOny_Conc<-1-pchisq(SS[2]/MS,Df[2])
p_DefOny_Soil.Conc<- 1-pchisq(SS[3]/MS,Df[3])
p_DefOny_Soil
p_DefOny_Conc
p_DefOny_Soil.Conc

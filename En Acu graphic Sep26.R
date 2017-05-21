###############################################################################################
#                                                                                             #  
#                                                                                             #
#                                                                                             #
#                             Graphic for Enych and mite                                 #
#                                                                                             #
#                               Export in Feb 22, 2016  
#
#   eps command at the beginning of the script                                                                                       #
#   use dev.off() to close the edit and save image into an independent file
#   DPI are important, setting by tiff(res=)
###############################################################################################





setwd ("/Users/JiayiQin/Dropbox/ASHBACK/BIOS/single-species test")
#Import datafile
SUM <- read.table( file = "Briefly summary.csv", header=TRUE, sep =",", dec =".")
SUM
summary(SUM)
attach(SUM)
load("/Users/JiayiQin/Dropbox/ASHBACK/BIOS/single-species test/Statistic for definitive test/Graphic Enc Acu Sep 26.RData")



FouDefAcu <- subset(SUM, Soil.type == "Foulum soil" &
                      Test.type == "definitive" &
                      Species == "Hypoaspis aculeifer", select=Species:Juvenile,drop=FALSE) 

FouDefAcu

FouDefEnc<- subset(SUM, Soil.type == "Foulum soil" &
                      Test.type == "definitive" &
                      Species == "Enchytraeus crypticus", select=Species:Juvenile,drop=FALSE) 

FouDefEnc


FouGedAcu <- subset(SUM, Soil.type == "Gedhus soil" &
                      Test.type == "definitive" &
                      Species == "Hypoaspis aculeifer", select=Species:Juvenile,drop=FALSE) 

FouGedAcu

FouGedEnc<- subset(SUM, Soil.type == "Gedhus soil" &
                     Test.type == "definitive" &
                     Species == "Enchytraeus crypticus", select=Species:Juvenile,drop=FALSE) 

FouGedEnc

se <- function(i){sd(i)/sqrt(length(i))}

FouDefAcu_J <- FouDefAcu $ Juvenile
FouDefAcu_A <- FouDefAcu $ Adult
FouDefAcu_C <- FouDefAcu $ Concentration

MFouDefAcu_J<-tapply (FouDefAcu_J,FouDefAcu_C,mean)
SFouDefAcu_J<-tapply (FouDefAcu_J,FouDefAcu_C,se)

MFouDefAcu_A<-tapply (FouDefAcu_A,FouDefAcu_C,mean)
SFouDefAcu_A<-tapply (FouDefAcu_A,FouDefAcu_C,se)

FouDefEnc_J <- FouDefEnc $ Juvenile
FouDefEnc_A <- FouDefEnc $ Adult
FouDefEnc_C <- FouDefEnc $ Concentration

MFouDefEnc_J<-tapply (FouDefEnc_J,FouDefEnc_C,mean)
SFouDefEnc_J<-tapply (FouDefEnc_J,FouDefEnc_C,se)

MFouDefEnc_A<-tapply (FouDefEnc_A,FouDefEnc_C,mean)
SFouDefEnc_A<-tapply (FouDefEnc_A,FouDefEnc_C,se)


FouGedAcu_J <- FouGedAcu $ Juvenile
FouGedAcu_A <- FouGedAcu $ Adult
FouGedAcu_C <- FouGedAcu $ Concentration

MFouGedAcu_J<-tapply (FouGedAcu_J,FouGedAcu_C,mean)
SFouGedAcu_J<-tapply (FouGedAcu_J,FouGedAcu_C,se)


MFouGedAcu_A<-tapply (FouGedAcu_A,FouGedAcu_C,mean)
SFouGedAcu_A<-tapply (FouGedAcu_A,FouGedAcu_C,se)


FouGedEnc_J <- FouGedEnc $ Juvenile
FouGedEnc_A <- FouGedEnc $ Adult
FouGedEnc_C <- FouGedEnc $ Concentration

MFouGedEnc_J<-tapply (FouGedEnc_J,FouGedEnc_C,mean)
SFouGedEnc_J<-tapply (FouGedEnc_J,FouGedEnc_C,se)

MFouGedEnc_A<-tapply (FouGedEnc_A,FouGedEnc_C,mean)
SFouGedEnc_A<-tapply (FouGedEnc_A,FouGedEnc_C,se)
library("gplots")
postscript(file = "/Users/JiayiQin/Desktop/Fig2_En&Mite.eps",
           width = 5.51181, height = 4.72441,pointsize = 7,
           bg = "white")
# the default unit is inch

par(mfrow = c(2,2),family="Helvetica")
par(oma = c(2,2,2,2))
########################################Hypoaspis aculeifer--Foulum Soil#############################################
par(new=FALSE)
par (mar = c(2,4,4,2))

plotCI(x = MFouDefAcu_J, 
       uiw = SFouDefAcu_J, 
       xaxt ="n", 
       las = 1,
       xlim = c(0.5,2.5), 
       ylim = c(0,400), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 21,
       lwd = 1,cex=1,cex.axis=1,
       col = "blue")
mtext("Number of Juveniles", font=1,side=2,line=2.5,cex=1)



par(new=TRUE)
plotCI(x = MFouDefAcu_A, 
       uiw = SFouDefAcu_A, 
       xaxt ="n", 
       yaxt ="n", 
       xlim = c(0.5,2.5), 
       ylim = c(0,10.5), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 18,
       lwd = 1,cex=1,cex.axis=1,
       col = "red")

axis(4,las=1)#控制坐标轴刻度数字标记方向的整数(0: 平行于轴,1: 横排,2: 垂直于轴,3: 竖排) 
mtext("H. aculeifer", font=3,side=3,line=1,cex =1)
mtext("Foulum soil", font=2,side=2,line=4, cex =1)


############################################Graphic E. crypticus--Foulum Soil####################################

par(new = FALSE)
par (mar = c(2,2.5,4,4))
plotCI(x = MFouDefEnc_J, 
       uiw = SFouDefEnc_J, 
       xaxt ="n", 
       las = 1,
       xlim = c(0.5,2.5), 
       ylim = c(0,400), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 21,
       lwd = 1,cex=1,cex.axis=1,
       col = "blue")
mtext("E. crypticus", font=3,side=3,line=1,cex=1)


par(new=TRUE)
plotCI(x = MFouDefEnc_A, 
       uiw = SFouDefEnc_A, 
       xaxt ="n", 
       yaxt ="n", 
       xlim = c(0.5,2.5), 
       ylim = c(0,10.5), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 18,
       lwd = 1,cex=1,cex.axis=1,
       col = "red")

axis(4,las=1)
mtext("Number of Adults", font=1,side=4,line=2,cex=1)

####################################Graphic Gedhus soil H. aculeifer#################################


par(new = FALSE)
par (mar = c(5,4,1,2))

plotCI(x = MFouGedAcu_J, 
       uiw = SFouGedAcu_J, 
       xaxt ="n", 
       las = 1,
       xlim = c(0.5,2.5), 
       ylim = c(0,400), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 21,
       lwd = 1,cex=1,cex.axis=1,
       col = "blue")

mtext("Number of Juveniles", font=1,side=2,line=2.5,cex=1)
mtext("Gedhus soil", font=2,side=2,line=4,cex=1)



par(new =TRUE)
plotCI(x = MFouGedAcu_A, 
       uiw = SFouGedAcu_A, 
       xaxt ="n", 
       yaxt ="n", 
       xlim = c(0.5,2.5), 
       ylim = c(0,10.5), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 18,
       lwd = 1,cex=1,cex.axis=1,
       col = "red")

mtext("Concentration of Galten ash (g/kg)", font=1,side=1,line=3,cex=1)
axis(1, at = c(1, 2), label = c(0,50),cex=1)
axis(4,las=1,cex=1)

####################################Graphic Gedhus soil E. crpticus#################################


par(new = FALSE)
par (mar = c(5,2.5,1,4))
plotCI(x = MFouGedEnc_J, 
       uiw = SFouGedEnc_J, 
       xaxt ="n", 
       las = 1,
       xlim = c(0.5,2.5), 
       ylim = c(0,400), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 21,
       lwd = 1,cex=1,cex.axis=1,
       col = "blue")

par(new =TRUE)
plotCI(x = MFouGedEnc_A, 
       uiw = SFouGedEnc_A, 
       xaxt ="n", 
       yaxt = "n",
       xlim = c(0.5,2.5), 
       ylim = c(0,10.5), 
       gap = 0, 
       ylab="", 
       xlab="",
       main = "",
       lty = 1,
       pch = 18,
       lwd = 1,cex=1,cex.axis=1,
       col = "red")



mtext("Concentration of Galten ash (g/kg)", font=1,side=1,line=3,cex=1)

axis(4,las=1,cex=1)
axis(1, at = c(1, 2), label = c(0,50),cex=1)
mtext("Number of Adults", font=1,side=4,line=2,cex=1)
dev.off()


###############################################################################################
#                                                                                             #  
#                                                                                             #
#                                                                                             #
#                             Graphic for Definitive test                                     #
#                                                                                             #
#                               Export in Feb 22, 2016  
#
#   tiff command at the beginning of the script                                                                                       #
#   use dev.off() to close the edit and save image into an independent file
#   DPI are important, setting by tiff(res=)
###############################################################################################
library(drc)
load("/Users/JiayiQin/Dropbox/ASHBACK/BIOS/single-species test/Statistic for definitive test/July 16.RData")

postscript(file = "/Users/JiayiQin/Desktop/Fig1_collembolans.eps",
           width = 5.51181, height = 4.72441,pointsize = 7,
           bg = "white")
# the default unit is inch

par(mfrow = c(2,2),family="Helvetica")
par(oma = c(2,2,2,2))
########################################Graphic Folsomia candida--Foulum Soil#############################################
par()
par (mar = c(3.1,4.1,2.1,2.1))
plotfitFolJuv_Conc <- plot(fitFolJuv_Conc,#y~x, expression for x and y axis
                           # broken=TRUE,
                           axes=T,
                           log="", #log x or y axis. If none of them needed, put "" on
                           main="",#main title of the graph
                           xlab = "",#Lable for x axis
                           ylab = "",# Lable for y axis
                           xlim = c(0,20),# The range for x axis
                           ylim = c(0,1000),#The range for y axis
                           xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                           yaxs = "i",
                           col = "blue", #color for graph
                           pch = 21,#Options for dots style
                           type="all", # dot/line /all
                           cex=1, cex.axis=1, 
                           lwd=0.8)
mtext("Number of Juveniles", font=1,side=2,line=2.5,cex=1)

fitFouDefFol_Adult<-lm(FouDefFol_Adult~FouDefFol_Conc)
par(new=TRUE)
plot(FouDefFol_Conc,FouDefFol_Adult,
     axes=F,#export axis T/F
     xlim=c(0,20),
     ylim=c(0,11),
     xlab="",
     ylab="",
     xaxs = "i",
     yaxs = "i",
     col="red",
     type="p",
     cex=1,
     pch=18) 

axis(4,las=1,cex = 1)#控制坐标轴刻度数字标记方向的整数(0: 平行于轴,1: 横排,2: 垂直于轴,3: 竖排)
abline(fitFouDefFol_Adult, xlim=c(0,20),lty=1,col="red",lwd=0.8) 
mtext("F. candida", font=3,side=3,line=1,cex=1)
mtext("Foulum soil", font=2,side=2,line=4,cex =1)
axis(1, col = "transparent", line = 1,at = c(0, 5, 10, 15, 20), labels = c("(6.40", "7.40", "7.82", "8.32", "8.68)"),cex=1)

############################################Graphic Onychiurus yodai--Foulum Soil####################################
par(new = FALSE)
par (mar = c(3.1,2.1,2.1,4.1))
plotfitOnyJuv_Conc <- plot(fitOnyJuv_Conc,#y~x, expression for x and y axis
                           # broken=TRUE,
                           axes=T,
                           log="",#log x or y axis. If none of them needed, put "" on
                           main="",#main title of the graph
                           xlab = "",#Lable for x axis
                           ylab = "",# Lable for y axis
                           xlim = c(0,80),# The range for x axis
                           ylim = c(0,350),#The range for y axis
                           
                           xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                           yaxs = "i",
                           col = "blue", #color for graph
                           pch = 21,#Options for dots style
                           type="all", # dot/line /all
                           cex=1, cex.axis=1,
                           lwd=0.8)

par(new=TRUE)

plotfitOnyAdult_Conc <- plot(fitOnyAdult_Conc,#y~x, expression for x and y axis
                             # broken=TRUE,
                             axes=F,
                             log="",#log x or y axis. If none of them needed, put "" on
                             main="",#main title of the graph
                             xlab = "",#Lable for x axis
                             ylab = "",# Lable for y axis
                             xlim = c(0,80),# The range for x axis
                             ylim = c(0,16),#The range for y axis
                             
                             xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                             yaxs = "i",
                             col = "red", #color for graph
                             pch = 18,#Options for dots style
                             type="all", # dot/line /all
                             cex=1, 
                             lwd=0.8)
mtext("O. yodai", font=3,side=3,line=1,cex=1)



axis(1, col = "transparent", line = 1,at = c(0, 20, 40, 60, 75), cex=1,labels = c("(6.4", "8.68", "9.01", "9.17", "9.25)"))



axis(4,las=1,cex=1)
mtext("Number of Adults", font=1,side=4,line=2,cex=1)

####################################Graphic Gedhus soil Folsomia candida#################################
par(new = FALSE)
par (mar = c(5.1,4.1,1.1,2.1))
plotfitGedDefFol_Juvenile<- plot(GedDefFol_Conc,GedDefFol_Juvenile,
     axes= T,#export axis T/F
     las = 1,
     log="",#log x or y axis. If none of them needed, put "" on
     main="",
     xlim=c(0,20),
     ylim=c(0,420),
     xlab="",
     ylab="",
     xaxs = "i",
     yaxs = "i",
     col="blue",
     type="p",
     pch=21,
     cex=1, cex.axis=1,
     lwd=0.8) 
abline(fitGedDefFol_Juvenile, xlim=c(0,20),lty=1,col="blue",lwd=0.8) 


mtext("Number of Juveniles", font=1,side=2,line=2.5,cex=1)
mtext("Gedhus soil", font=2,side=2,line=4,cex=1)

par(new =TRUE)
plot(GedDefFol_Conc,GedDefFol_Adult,
     axes=F,#export axis T/F
     xlim=c(0,20),
     ylim=c(0,11),
     xlab="",
     ylab="",
     xaxs = "i",
     yaxs = "i",
     col="red",
     type="p",
     pch=18,
     lwd=0.8,
     cex=1) 


abline(fitGedDefFol_Adult, xlim=c(0,20),lty=1,col="red",lwd=0.8,cex=1) 


axis(1, at = c(0, 5, 10, 15, 20), cex=1,labels = c("(4.02", "4.16", "4.18", "4.22", "4.35)"), line = 1, col = "transparent")
mtext("Concentration of Galten ash (g/kg)", font=1,side=1,line=3,cex=1)
mtext("(Soil pH after adding Galten ash)", font=1,side=1,line=4,cex=1)

axis(4,las=1,cex=1)


####################################Graphic Gedhus soil Onychiurus yodai#################################
par(new = FALSE)
par (mar = c(5.1,2.1,1.1,4.1))
plot(GedDefOny_Conc,GedDefOny_Juvenile,
     axes=T,#export axis T/F
     las =1,
     xlim=c(0,80),
     ylim=c(0,200),
     xlab="",
     ylab="",
     xaxs = "i",
     yaxs = "i",
     col="blue",
     type="p",
     pch=21,
     cex=1,cex.axis=1,
     lwd=0.8) 
abline(fitGedDefOny_Juvenile, xlim=c(0,20),lty=1,col="blue",lwd=0.8) 



par(new =TRUE)
plot(GedDefOny_Conc,GedDefOny_Adult,
     axes=F,#export axis T/F
     xlim=c(0,80),
     ylim=c(0,15),
     xlab="",
     ylab="",
     xaxs = "i",
     yaxs = "i",
     col="red",
     type="p",
     pch=18,
     lwd=0.8,cex=1) 


abline(fitGedDefOny_Adult, xlim=c(0,20),lty=1,col="red",lwd=0.8) 


axis(1, cex=1,at = c(0, 20, 40, 60, 75), labels = c("(4.02", "4.35", "4.75", "4.95", "5.10)"), line = 1, col = "transparent")
mtext("Concentration of Galten ash (g/kg)", font=1,side=1,line=3,cex=1)
mtext("(Soil pH after adding Galten ash)", font=1,side=1,line=4,cex=1)

axis(4,las=1,cex=1)
mtext("Number of Adults", font=1,side=4,line=2,cex=1)

dev.off()


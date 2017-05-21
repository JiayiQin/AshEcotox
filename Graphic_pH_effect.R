###############################################################################################
#                                                                                             #  
#                                                                                             #
#                                                                                             #
#                             Graphic for pH-liming-mortality                            #
#                                                                                             #
#                               Export in Feb 23, 2016  
#
#   postscript command at the beginning of the script to export eps file                                                                                      #
#   use dev.off() to close the edit and save image into an independent file
#   The model have changed since the calculation was reproduced according to binomial distribution
###############################################################################################
load("/Users/JiayiQin/Dropbox/ASHBACK/BIOS/single-species test/pH/pH effect.RData")

postscript(file = "/Users/JiayiQin/Desktop/Fig3_soil_pH.eps",
           width = 3.54331, height = 2.3622,pointsize = 7,
           bg = "white")
par(mfrow = c(1,1))
par(mgp=c(3, 0.7, 0), plt=c(0.2, 1, 0.2, 1), omd=c(0,0.9,0,0.9), family="Helvetica")
par(xaxt="s",yaxt="s")
plotfitCApH <- plot(fitCApH,#y~x, expression for x and y axis
                  # broken=TRUE,
                 log="",#log x or y axis. If none of them needed, put "" on
                 # main="Survival of O.yodai",#main title of the graph
                 xlab = "",#Lable for x axis
                 ylab = "",# Lable for y axis
                 xlim = c(6.8, 10),# The range for x axis
                 ylim = c(0,11),#The range for y axis
                 xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                 yaxs = "i",
                 col = "black", #color for graph
                 pch = 21,#Options for dots style
                 type="all", # dot/line /all
                 
                 cex=1, cex.axis= 1, cex.lab=1, 
                 lwd=0.8)
par(new=TRUE)
par(xaxt="n",yaxt="n")
plotfitMApH <- plot(fitMApH,#y~x, expression for x and y axis
                  # broken=TRUE,
                  log="",#log x or y axis. If none of them needed, put "" on
                  main="",#main title of the graph
                  xlab = "",#Lable for x axis
                  ylab = "",# Lable for y axis
                  xlim = c(6.8, 10),# The range for x axis
                  ylim = c(0,11),#The range for y axis
                  xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                  yaxs = "i",
                  col = "blue", #color for graph
                  pch = 17,#Options for dots style
                  type="all", # dot/line /all
                  bty="n",
                  cex=1, cex.axis=1, #cex.lab=1.2,
                  lwd=0.8)
par(new=TRUE)
par(xaxt="n",yaxt="n")
plotfitASpH <- plot(fitASpH,#y~x, expression for x and y axis
                  # broken=TRUE,
                  log="", #log x or y axis. If none of them needed, put "" on
                  main="",#main title of the graph
                  xlab = "",#Lable for x axis
                  ylab = "",# Lable for y axise
                  xlim = c(6.8, 10),# The range for x axis
                  ylim = c(0,11),#The range for y axis
                
                  xaxs = "i",# The number level of axis, i means accordino setting, r means it goes with the range of custome data
                  yaxs = "i",
                  cex=1, cex.axis=1, #cex.lab=1.2,
                  col = "red", #color for graph
                  pch = 22,#Options for dots style
                  type="all", # dot/line /all
                  bty="n",
                  lwd=0.8)
par(new=TRUE)
# par(mfrow=c(1,1))   一个图版显示2行，3列。
legend("bottomleft", c("Calcium hydroxide", "Galten ash", "Brande ash"), 
       col = c( "black","red","blue" ),
       text.col = "black", 
       #lty = c(2, -1, 1),
       pch = c(21,22,17),
       bty="n",
       cex= 1 )
mtext(side=3, "O. yodai", font=3,line=0.5, cex=1)
mtext(side=2, "Number of Adults", font=1,line=1.5, cex=1)
mtext(side=1, "Soil pH", font=1,line=1.7, cex=1)
par(xaxt="s",yaxt="s")
axis(1,at=c(10),labels=c("10"),cex=1,line=0)

dev.off()


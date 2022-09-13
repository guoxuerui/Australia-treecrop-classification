options(scipen = 999)

rm(list = ls())
library(dplyr)
library(shape)
# the sample dataset --------------------------------------------------------------
in_dir <- "C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/new_index_new/ndvi/Apples"
excel_files <- list.files(path=in_dir,pattern=".csv",full.names=T)

# the functions ----------------------------------------------------------------
na_inpol            <- function(x) {
  x <- zoo::na.approx(x,rule=2)
  return(x)
}
sm_fouri            <- function(data_in,harmonics=4){
  
  N           <- length(data_in)
  A           <- matrix(0,nrow=N,ncol=1,byrow=T)
  B           <- matrix(0,nrow=N,ncol=1,byrow=T)
  smooth_data <- c()
  
  for(i in 1:((N/2)+1)){
    A[i,1]   <- 0
    B[i,1]   <- 0
    
    for(j in 1:N){
      A[i,1] <- A[i,1]+(2/N)*data_in[j]*cos(2*pi*(i-1)*j/N)
      B[i,1] <- B[i,1]+(2/N)*data_in[j]*sin(2*pi*(i-1)*j/N)
    }}
  A[N/2+1]   <- A[N/2+1]/2
  
  for(i in 1:N){
    smooth_data[i]   <- A[1]/2
    
    for(j in 2:harmonics){
      smooth_data[i] <- smooth_data[i]+A[j,1]*cos(2*pi*(j-1)*i/N)+B[j,1]*sin(2*pi*(j-1)*i/N);
    }}
  return(smooth_data)
}


#plot raw data and the one after DFT smoothing ----------------------------------------------------------------
# pick one of the sample to visulize
i =112
png(file="C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/visual/Figures/smoothing.png",
    width=1100, height=900)

time_series <- read.csv(excel_files[i], na.strings = c("","NA"))
  ts_raw    <- c(time_series[[3]])
 
  time_series$date <- as.Date(time_series$Date, "%d/%m/%Y")
  date<-time_series$date
  plot(axes = FALSE,date,ts_raw,col="red",lwd=2,xaxt = "n", ylim=c(0,1),type = "o",ylab = "",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  par(new=TRUE)
  axis(2,cex.axis = 1.4)
  mtext("NDVI", side = 2, cex.axis = 8, cex=2,line=2.5,at=0.5)
  axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 1.4)
  ts_int    <- na_inpol  (ts_raw)
  ts_dft    <- sm_fouri  (ts_int)
  df1 <- data.frame(index=1:length(ts_raw),
                    diff=round((ts_raw-ts_dft),6))
  df1 <- dplyr::mutate(df1,diff_square=round(sqrt((ts_raw-ts_dft)^2),3))
  df1<- mutate(df1,pr = percent_rank(df1$diff_square)) %>%dplyr::filter(pr>= 0.95, diff < 0)
  ts_raw[df1$index] <- NA
  
  df1 <- data.frame(index=1:length(ts_raw),
                    diff=round((ts_raw-ts_dft),6))
  df1 <- mutate(df1,diff_square=round(sqrt((ts_raw-ts_dft)^2),3))
  
  df1<- mutate(df1,pr = percent_rank(df1$diff_square)) %>%dplyr::filter(pr>= 0.95, diff < 0)
  ts_raw[df1$index] <- NA
  par(new=TRUE)
  plot(axes = FALSE,date,ts_raw,col="green",lwd=2,xaxt = "n", ylim=c(0,1),type = "o",ylab = "",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  par(new=TRUE)
  ts_int    <- na_inpol  (ts_raw)
  ts_dft    <- sm_fouri  (ts_int)
 par(new=TRUE)
  plot(date, ts_dft ,col="black",lwd=2,xaxt = "n", yaxt = "n",ylim=c(0,1),type = "o",ylab = "",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  dev.off()

# plot the visualization of phenological metrics retrieval----------------------------------------------------------------
  png(file="C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/visual/Figures/phenology retrieval.png",
      width=1100, height=900)
  
  par(mar = c(4, 4,4, 4))    
  par(mfrow = c(1, 1))
  plot(axes = FALSE,date,ts_raw,col="red",lwd=2,xaxt = "n",yaxt = "n", ylim=c(0,1),type = "o",ylab = "",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 1.4)
  par(new=TRUE)
  plot(axes = FALSE,ts_dft ~ date, merge, xaxt = "n",yaxt = "n", col="black",lwd=2, ylim=c(0,1),type = "o",ylab = "",xlab = "",xlim=c(date[1],date[37]),cex.lab = 1.4)
  axis(side = 2, cex.axis = 1.5)
  par(new=TRUE)
  plot(ts_dft[26]~date[26], merge,xaxt = "n",yaxt = "n",pch=13,xlim=c(date[1],date[37]), lwd=5,ylim=c(0,1),col="green",ylab = "",xlab = "Date",cex.lab = 1.4)
  par(new=TRUE)
  plot(ts_dft[7]~date[7], merge,xaxt = "n",yaxt = "n",pch=13,xlim=c(date[1],date[37]), lwd=5,ylim=c(0,1),col="orange",ylab = "",xlab = "Date",cex.lab = 1.4)
  mtext("NDVI", side = 2, cex.axis = 4, cex=1.5,line=2.5,at=0.5)
  
  mtext("Adjacent difference", side = 4, cex.axis = 4, cex=1.5,line=2.5,at=0.5)
  l <- min(which(merge$date >= merge$date[12]))
  h <- max(which(merge$date <= merge$date[32]))
  polygon(c(merge$date[c(l, l:h, h)]),
          c(0,merge$ts_dft[12:32],0),density = 7, angle = 45,col="blueviolet", ylim=c(0,1),xlim=c(date[12:32]),axes = FALSE,ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]))
  abline(h =merge$ts_dft[7], col = "orange", lwd = 2, lty = 2)
  text(as.numeric(merge$date[26]-5),as.numeric(merge$ts_dft[10]),'Amplitude',lty = 2,cex = 1.5,pos = 2, srt = 270,col='green')
  text(as.numeric(date[23]),as.numeric(ts_dft[7]-0.023),'LOS',cex =1.5)
  segments(merge$date[32],merge$ts_dft[6]-1,merge$date[32],merge$ts_dft[32]+1,lty = 2)
  segments(merge$date[12],merge$ts_dft[12]-1,merge$date[12],merge$ts_dft[12]+1,lty = 2)
  # segments(0,ts_dft[12],date[12],ts_dft[12]+0.007,lty = 2)
  # segments(date[32],ts_dft[6]-1,date[32],ts_dft[32]+1,lty = 2)
  Arrows(as.numeric(merge$date[12]),merge$ts_dft[7],as.numeric(merge$date[12]),as.numeric(merge$ts_dft[12]),lwd=2, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2,col="green")
  segments(merge$date[7],merge$ts_dft[7],merge$date[12],merge$ts_dft[7],lty = 2,col="orange",cex.lab = 1.4)
  segments(merge$date[26],merge$ts_dft[7],merge$date[26],merge$ts_dft[26],lty = 2,col="green",cex.lab = 1.4)
  par(new=TRUE)
  Arrows(as.numeric(date[12]),as.numeric(ts_dft[7]-0.05),as.numeric(date[32]-2),as.numeric(ts_dft[7]-0.05),lwd=1, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2)
  Arrows(as.numeric(date[32]-2),as.numeric(ts_dft[7]-0.05),as.numeric(date[12]+2),as.numeric(ts_dft[7]-0.05),lwd=1, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2)
  
  text(as.numeric(merge$date[20]),as.numeric(merge$ts_dft[21]-0.1),'Accumulation',cex =1.5,lty = 2,col="blueviolet")
  text(as.numeric(merge$date[14]),as.numeric(merge$ts_dft[10]-0.06),'30% amplitude',lty = 8,cex = 1.4,pos = 2, srt = 270)
  text(as.numeric(merge$date[12]-3),as.numeric(merge$ts_dft[32]+0.2),'SOS',cex = 1.5,lty = 2,pos = 2, srt = 270)
  text(as.numeric(merge$date[26]-5),as.numeric(merge$ts_dft[26]+0.32),'Adjacent difference',cex = 1.4,col='pink',lty = 2)
  
  text(as.numeric(merge$date[32]-3),as.numeric(merge$ts_dft[32]+0.2),'EOS',cex = 1.5,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[6]),as.numeric(ts_dft[12]+0.023),'SOS_ y',cex = .8)
  
  text(as.numeric(merge$date[7]),as.numeric(merge$ts_dft[7]-0.05),'local valley',cex = 1.5,col= 'orange',lty = 2)
  diff <- data.frame(index=1:(length(merge$date)-1),dif=diff(merge$ts_dft))
  dY <- diff(merge$ts_dft)  # the derivative of your function
  dX <- rowMeans(embed(as.numeric(merge$date),2)) # centers the X values for plotting
  par(new=TRUE)
  plot(dX+10,dY ,type="l",xaxt = "n", yaxt = "n",col='pink',ann=FALSE,xlim=c(date[1],date[37]),ylim = c(-0.5,0.05), lwd=0.5,  ylab = "Differnece", xlab = "") #check
  abline(h =0, col = "black", lwd = 1, lty = 2) 
  Arrows(as.numeric(dX[32]),0,as.numeric(dX[32]),as.numeric(dY[30]),lwd=1, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2,col="green")
  text(as.numeric(dX[33]+7),as.numeric(dY[32]),'>8% amplitude',col= 'black',lty = 8,cex = 1.2,pos = 2, srt = 270)
  axis(side = 4, cex.axis = 1.3)
  axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 1.4)
  dev.off()
  
  
  
  # plot raw and smoothing
  # par(oma=c(0.5, 0.5, 0.5, 0.5))
  # time_series <- read.csv(excel_files[i], na.strings = c("","NA"))
  # ts_raw    <- c(time_series[[3]])
  # time_series$date <- as.Date(time_series$Date, "%d/%m/%Y")
  # plot(axes = FALSE,date,ts_raw,col="black",lwd=2,xaxt = "n", ylim=c(0,0.8),type = "o",ylab = "",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # par(new=TRUE)
  # 
  # plot(axes = FALSE,ts_dft ~ date, merge, xaxt = "n", yaxt = "n",col="black",lwd=2, ylim=c(0,0.8),type = "o",ylab = "",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # axis(2,cex.axis = 2)
  # segments(merge$date[32],merge$ts_dft[6]-1,merge$date[32],merge$ts_dft[32]+1,lty = 2)
  # segments(merge$date[12],merge$ts_dft[12]-1,merge$date[12],merge$ts_dft[12]+1,lty = 2)
  # mtext("NDVI", side = 2, cex.axis = 8, cex=2,line=2.5,at=0.4)
  # 
  # axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 2)
  # par(new=TRUE)
  # df1 <- data.frame(index=1:length(ts_raw),
  #                   diff=round((ts_raw-ts_dft),6))
  # df1 <- dplyr::mutate(df1,diff_square=round(sqrt((ts_raw-ts_dft)^2),3))
  # df1 <- dplyr::mutate(df1,quantile_rank=ntile(desc(df1$diff_square),100)) %>%
  #   dplyr::filter(quantile_rank <= 10, diff < 0)
  # 
  # df1 <- data.frame(index=1:length(ts_raw),
  #                   diff=round((ts_raw-ts_dft),6))
  # df1 <- mutate(df1,diff_square=round(sqrt((ts_raw-ts_dft)^2),3))
  # 
  # df1<- mutate(df1,pr = percent_rank(df1$diff_square)) %>%dplyr::filter(pr>= 0.95, diff < 0)
  # ts_raw[df1$index] <- NA
  # par(new=TRUE)
  # plot(axes = FALSE,merge$date,merge$ts_raw ,type="o",col="green", xaxt = "n", yaxt = "n",lwd=2,ylim=c(0,0.8),ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # ts_int    <- na_inpol  (ts_raw)
  # ts_dft    <- sm_fouri  (ts_int)
  # time_series$Date
  # par(new=TRUE)
  # plot(axes = FALSE,ts_dft ~ date, merge,type="o",col="black",xaxt = "n", yaxt = "n",lwd=2,ylim=c(0,0.8),ylab = "NDVI",xlab = "Date",xlim=c(merge$date[1],merge$date[37]),cex.lab = 2)
  # # par(new=TRUE)
  # par(new=TRUE)
  # ts_int    <- na_inpol  (ts_raw)
  # ts_dft    <- sm_fouri  (ts_int)
  # time_series$Date
  # plot(axes = FALSE,date,ts_raw,col="red",lwd=2,xaxt = "n",yaxt = "n", ylim=c(0,0.8),type = "o",ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # par(new=TRUE)
  # plot(axes = FALSE,ts_dft ~ date, merge,type="o",col="black",xaxt = "n", yaxt = "n",lwd=2,ylim=c(0,0.8),axes = FALSE,ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # 
  # # visual only SOS/EOS/LOS and peak of the season
  # mtext("NDVI", side = 2, cex.axis = 8, cex=2,line=2.5,at=0.4)
  # axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 2)
  # par(new=TRUE)
  # 
  # par(new=TRUE)
  # plot(ts_dft[27]~date[27], merge,xaxt = "n",yaxt = "n",pch=13,xlim=c(date[1],date[37]),axes = FALSE, lwd=5,ylim=c(0,0.8),col="green",ylab = "NDVI",xlab = "Date",cex.lab = 2)
  # par(new=TRUE)
  # plot(ts_dft[7]~date[7], merge,xaxt = "n",yaxt = "n",pch=13,xlim=c(date[1],date[37]),axes = FALSE, lwd=5,ylim=c(0,0.8),col="orange",ylab = "",xlab = "Date",cex.lab = 2)
  # par(new=TRUE)
  # l <- min(which(merge$date >= merge$date[12]))
  # h <- max(which(merge$date <= merge$date[32]))
  # polygon(c(merge$date[c(l, l:h, h)]),
  #         c(0,merge$ts_dft[12:32],0),density = 8, angle = 60,col="blueviolet", ylim=c(0,1),xlim=c(date[12:32]),axes = FALSE,ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]))
  # par(new=TRUE)
  # abline(h =merge$ts_dft[7], col = "orange", lwd = 2, lty = 2)
  # Arrows(as.numeric(merge$date[12]+4),as.numeric(merge$ts_dft[7]-0.15),as.numeric(merge$date[32]-4),as.numeric(merge$ts_dft[7]-0.15),lwd=2, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2)
  # Arrows(as.numeric(merge$date[32]-4),as.numeric(merge$ts_dft[7]-0.15),as.numeric(merge$date[12]+4),as.numeric(merge$ts_dft[7]-0.15),lwd=2, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2)
  # par(new=TRUE)
  # text(as.numeric(merge$date[12]-3),as.numeric(merge$ts_dft[32]+0.2),'SOS',cex = 2,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(merge$date[32]-3),as.numeric(merge$ts_dft[32]+0.2),'EOS',cex = 2,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(merge$date[23]),as.numeric(merge$ts_dft[7]-0.125),'LOS',cex =2)
  # text(as.numeric(merge$date[27]),as.numeric(merge$ts_dft[27]+0.04),'POS',cex =2)
  # text(as.numeric(merge$date[21]),as.numeric(merge$ts_dft[21]-0.1),'Accumulation',cex =1.8,lty = 2,col="blueviolet")
  # text(as.numeric(merge$date[27]-2),as.numeric(merge$ts_dft[10]),'(e)',lty = 2,cex = 2,pos = 2, srt = 270,col='green')
  # 
  # segments(merge$date[32],merge$ts_dft[6]-1,merge$date[32],merge$ts_dft[32]+1,lty = 2)
  # segments(merge$date[12],merge$ts_dft[12]-1,merge$date[12],merge$ts_dft[12]+1,lty = 2)
  #  segments(merge$date[27],merge$ts_dft[7],merge$date[27],merge$ts_dft[27],lty = 2,col="green")
  # text(as.numeric(date[12]-3),as.numeric(ts_dft[32]+0.3),'(a)',cex = 2,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[32]-3),as.numeric(ts_dft[32]+0.3),'(c)',cex = 2,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[23]),as.numeric(ts_dft[7]-0.125),'(d)',cex =2)
  # text(as.numeric(date[27]),as.numeric(ts_dft[27]+0.045),'(b)',cex =2)
  # axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 2)
  # axis(1, merge$date, format(merge$date, "%b %d"), cex.axis =1.4)
  # merge7<-c(0,0,0,0,0,0,0,0,0,0,ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7],ts_dft[7])
  # 
  #  polygon(merge$date,merge$ts_dft,xaxt = "n",density = 7, angle = 45,col="chocolate1", ylim=c(0,1),xlim=c(date[12:32]),axes = FALSE,ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]))
  # sum(12:30)
  # sum(merge$ts_dft[7:32])
  # sum(merge$ts_dft[7:32]*10)
  # text(as.numeric(date[22]),as.numeric(ts_dft[13]+0.045),'(f)',lty = 2,cex =2,col="chocolate1")
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # par(oma=c(1, 1, 1, 1))
  # time_series <- read.csv(excel_files[i], na.strings = c("","NA"))
  # ts_raw    <- c(time_series[[3]])
  # time_series$date <- as.Date(time_series$Date, "%d/%m/%Y")
  # plot(axes = FALSE,date,ts_raw,col="red",lwd=2,xaxt = "n",yaxt = "n", ylim=c(0,1),type = "o",ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # par(new=TRUE)
  # plot(ts_dft ~ date, merge, xaxt = "n", yaxt = "n",col="blue",lwd=2, ylim=c(0,1),type = "o",ylab = "NDVI",xlab = "Date",xlim=c(date[1],date[37]),cex.lab = 2)
  # axis(2,cex.axis = 2)
  # segments(merge$date[32],merge$ts_dft[6]-1,merge$date[32],merge$ts_dft[32]+1,lty = 2)
  # segments(merge$date[12],merge$ts_dft[12]-1,merge$date[12],merge$ts_dft[12]+1,lty = 2)
  # axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 2)
  # 
  # # visual only SOS/EOS/LOS and peak of the season
  # axis(side = 2, cex.axis = 2)
  # par(new=TRUE)
  # plot(ts_dft[27]~date[27], merge,xaxt = "n",pch=13,xlim=c(date[1],date[37]), lwd=5,ylim=c(0,1),col="green",ylab = "NDVI",xlab = "Date")
  # 
  # par(new=TRUE)
  # Arrows(as.numeric(date[12]),as.numeric(ts_dft[7]-0.05),as.numeric(date[32]-2),as.numeric(ts_dft[7]-0.05),lwd=1, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2)
  # Arrows(as.numeric(date[32]-2),as.numeric(ts_dft[7]-0.05),as.numeric(date[12]+2),as.numeric(ts_dft[7]-0.05),lwd=1, arr.type = 'triangle', arr.width=0.1,arr.length = 0.2)
  # par(new=TRUE)
  # text(as.numeric(date[12]-3),as.numeric(ts_dft[32]+0.2),'SOS',cex = 1.3,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[32]-3),as.numeric(ts_dft[32]+0.2),'EOS',cex = 1.3,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[23]),as.numeric(ts_dft[7]-0.023),'LOS',cex =1.3)
  # text(as.numeric(date[27]),as.numeric(ts_dft[27]+0.04),'POS',cex =1.3)
  # text(as.numeric(date[12]-3),as.numeric(ts_dft[32]+0.3),'(a)',cex = 2,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[32]-3),as.numeric(ts_dft[32]+0.3),'(c)',cex = 2,lty = 2,pos = 2, srt = 270)
  # text(as.numeric(date[23]),as.numeric(ts_dft[7]-0.015),'(d)',cex =2)
  # text(as.numeric(date[27]),as.numeric(ts_dft[27]+0.045),'(b)',cex =2)
  # axis(1, merge$date, format(merge$date, "%b %d"), cex.axis = 2)
  # text(as.numeric(merge$date[27]-5),as.numeric(merge$ts_dft[10]),'Amplitude',lty = 2,cex = 1.8,pos = 2, srt = 270,col='green')
  # text(as.numeric(merge$date[7]),as.numeric(merge$ts_dft[7]-0.05),'local valley',cex = 1.8,col= 'orange',lty = 2)
  # 
  # denx <- density(as.numeric(merge$ts_dft))
  # polygon(denx, col = rgb(0.78, 0.89, 1, alpha = 0.6))
  # polygon(merge$date,  c(y[x==max(x)], y[x>=1250] ), col="red")
 
  
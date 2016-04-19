#setwd("---")
D<-read.csv("Data.csv")

PisaMathrows <- D[which(D[4]=='LO.PISA.MAT' & D[16] != ''),1]
PisaScirows <- D[which(D[4]=='LO.PISA.SCI' & D[16] != ''),1]
primSalrows <- D[which(D[4]=='OECD.TSAL.1.E0' & D[16] != ''),1]
dnSecSalrows <- D[which(D[4]=='OECD.TSAL.2.E0' & D[16] != ''),1]
upSecSalrows <- D[which(D[4]=='OECD.TSAL.3.E0' & D[16] != ''),1]
GDProws <- D[which(D[4]=='NY.GDP.MKTP.PP.CD' & D[16] != ''),1]

countries <- intersect(intersect(intersect(intersect(intersect(as.character(PisaMathrows),as.character(upSecSalrows)),as.character(GDProws)),as.character(dnSecSalrows)),as.character(primSalrows)),as.character(PisaScirows))
PisaMath <- as.numeric(D[which(D[4]=='LO.PISA.MAT' & D[,1] %in% countries),16])
PisaSci <- as.numeric(D[which(D[4]=='LO.PISA.SCI' & D[,1] %in% countries),-16])
primSal <- as.numeric(D[which(D[4]=='OECD.TSAL.1.E0' & D[,1] %in% countries),16])
dnSecSal <- as.numeric(D[which(D[4]=='OECD.TSAL.2.E0' & D[,1] %in% countries),16])
upSecSal <- as.numeric(D[which(D[4]=='OECD.TSAL.3.E0' & D[,1] %in% countries),16])
GDP <- as.numeric(D[which(D[4]=='NY.GDP.MKTP.PP.CD' & D[,1] %in% countries),16])
#adjupSecSal <- as.numeric(upSecSal/GDP)

Data <- data.frame(countries,PisaMath,primSal,dnSecSal,upSecSal,GDP)

par(mfrow = c(2,2))
for(j in 2:ncol(Data)){
     hist(Data[,j],main = paste("Histogram of Data$",colnames(Data)[j],sep=""),xlab=paste("Data$",colnames(Data)[j],sep=""))
   }
par(mfrow = c(1,1))

pairs(Data[,2:ncol(Data)])

cor(Data[,2:ncol(Data)])

plot(primSal,PisaMath)
plot(primSal,PisaSci)
thetaD = seq(0,1,0.01)
thetaS = c(1,0.8,0.6,0.4)
thetaP = array(NA,dim=c(length(thetaD),length(thetaS)))
for (i in 1:length(thetaS)){
  thetaP[,i] = thetaD/thetaS[i]
}

thetaP[thetaP>1] = NA

pi = c(0.1,0.5)
rho = c(1,3,9)
omega = 1

tndEst = array(NA,dim=c(length(thetaS),length(rho),length(pi),length(thetaD)))


for (i in 1:length(thetaS)){
  for (j in 1:length(rho)){
    for (k in 1:length(pi))
      for (l in 1:length(thetaD)){
        tndEst[i,j,k,l] = thetaS[i]*(thetaP[l,i]*rho[j] + omega)*(1-pi[k])/((rho[j]+omega)*(1-thetaS[i]*pi[k]))
      }
  }
}

tndEst2 = array(NA,dim=c(length(thetaS),length(pi),length(thetaD)))
for (i in 1:length(thetaS)) for (k in 1:length(pi)) for (l in 1:length(thetaD)){
  tndEst2[i,k,l] = thetaS[i]*thetaP[l,i]*(1-pi[k])/(1-thetaS[i]*pi[k])
}

#plot(y=1-tndEst[1,1,1,],x=1-thetaD,type='l',xlim=c(0,1),ylim=c(0,1)); lines(x=c(-10,10),y=c(-10,10),col='grey')

setwd('~/Google drive (jlewnard@berkeley.edu)/pcv effectiveness/methods paper')

pdf('tnd bias plot.pdf',width=6.5,height=1.5)
layout(matrix(1:5,nrow=1,byrow=T),widths=c(1,1,1,1,0.35))
ltys = c('solid','dashed')
cols = c('darkgoldenrod3','cadetblue4','darkorchid3')
left = c(3,1.5,1.5,1.5)
ymin = 0
par(mgp=c(3,0.35,0))
par(tck=-0.02)
count = 1
par(mar=c(2.5,3.5,2,0.5))

#for (i in 1:length(thetaS)){
##  par(mar=c(2.5,left[i],2,0.5))
#  plot(y=1,x=1,axes=F,ann=F,xlim=c(0,1),ylim=c(ymin,1),type='n') 
#  lines(x=c(-10,10),y=c(-10,10),col='grey',lwd=0.5)
#    for (k in 1:length(pi)){
#      lines(y=1-tndEst2[i,k,],x=1-thetaD,lty=ltys[k],lwd=0.5)
#    }
#  
#  if (i==4){
#    lines(y=rep(0.95,2),x=c(0.05,0.25),lwd=0.5)
#    lines(y=rep(0.85,2),x=c(0.05,0.25),lty='dashed',lwd=0.5)
#    text(x=0.3,y=c(0.95,0.85),adj=0,c(expression(pi==0.1),
#                                   expression(pi==0.5)),
#         cex=0.75)
#  }
#  
#  box(bty='l',lwd=0.5)
#  text(x=seq(0,1,0.25),y=-0.1,seq(0,1,0.25)*100,cex=0.65,srt=45,xpd=T,adj=1)
#  axis(1,at=seq(0,1,0.25),lwd=0,lwd.ticks=0.5,labels=NA)
#  axis(2,at=seq(ymin,1,0.25),lwd=0,lwd.ticks=0.5,labels=seq(ymin,1,0.25)*100,las=1,cex.axis=0.65)
#  #if (i==1){
#    mtext(expression(paste(hat(VE)[italic(D)]^TND,', %',sep='')),side=2,line=1.5,cex=0.55)
#  #}
#  mtext(expression(paste(VE[italic(D)]==1-theta[italic(S)],theta[italic(P)],', %',sep='')),side=1,las=1,line=1.5,cex=0.55)
#  mtext(side=3,paste(LETTERS[count],')',sep=''),adj=0,cex=0.55)
#  if (i==1){mtext(side=3,expression(paste(theta[S]==1,', ',omega==0,sep='')),adj=1,cex=0.55)} else{
#    if (i==2){mtext(side=3,expression(paste(theta[S]==0.8,', ',omega==0)),adj=1,cex=0.55)} else{
#      if (i==3){mtext(side=3,expression(paste(theta[S]==0.7,', ',omega==0)),adj=1,cex=0.55)} else{
#        mtext(side=3,expression(paste(theta[S]==0.6,', ',omega==0)),adj=1,cex=0.55)
#      }
#    }
#  }
#  count = count+1
#}

for (i in 1:length(thetaS)){
  #  par(mar=c(2.5,left[i],2,0.5))
  plot(y=1,x=1,axes=F,ann=F,xlim=c(0,1),ylim=c(ymin,1),type='n') 
  lines(x=c(-10,10),y=c(-10,10),col='grey',lwd=0.5)
  for (j in 1:length(rho)){
    for (k in 1:length(pi)){
      lines(y=1-tndEst[i,j,k,],x=1-thetaD,col=cols[j],lty=ltys[k],lwd=0.5)
    }
  }

  box(bty='l',lwd=0.5)
  text(x=seq(0,1,0.25),y=-0.1,seq(0,1,0.25)*100,cex=0.65,srt=45,xpd=T,adj=1)
  axis(1,at=seq(0,1,0.25),lwd=0,lwd.ticks=0.5,labels=NA)
  axis(2,at=seq(ymin,1,0.25),lwd=0,lwd.ticks=0.5,labels=seq(ymin,1,0.25)*100,las=1,cex.axis=0.65)
  #if (i==1){
  mtext(expression(paste(hat(VE)[italic(D)]^TND,', %',sep='')),side=2,line=1.5,cex=0.55)
  #}
  mtext(expression(paste(VE[italic(D)]==1-theta[italic(S)],theta[italic(P)],', %',sep='')),side=1,las=1,line=1.5,cex=0.55)
  mtext(side=3,paste(LETTERS[count],')',sep=''),adj=0,cex=0.55)
  if (i==1){mtext(side=3,expression(paste(theta[S]==1,', ',omega==1,sep='')),adj=1,cex=0.55)} else{
    if (i==2){mtext(side=3,expression(paste(theta[S]==0.8,', ',omega==1)),adj=1,cex=0.55)} else{
      if (i==3){mtext(side=3,expression(paste(theta[S]==0.7,', ',omega==1)),adj=1,cex=0.55)} else{
        mtext(side=3,expression(paste(theta[S]==0.6,', ',omega==1)),adj=1,cex=0.55)
      }
    }
  }
  count = count+1
}
par(mar=c(2.5,0,2,0))
plot(1,type='n',axes=F,ann=F,xlim=c(0,0.5),ylim=c(0,1))
  lines(y=rep(0.85,2),x=c(0.05,0.25),lwd=0.5,col=cols[1])
  lines(y=rep(0.75,2),x=c(0.05,0.25),lwd=0.5,col=cols[2])
  lines(y=rep(0.65,2),x=c(0.05,0.25),lwd=0.5,col=cols[3])
  text(x=0.05,y=c(0.95),adj=0,expression(pi==0.1),cex=0.75)
  text(x=0.3,y=c(0.85,0.75,0.65),
       c(expression(rho==1),expression(rho==3),expression(rho==9)),cex=0.75,adj=0)
  
  lines(y=rep(0.3,2),x=c(0.05,0.25),lwd=0.5,col=cols[1],lty='dashed')
  lines(y=rep(0.2,2),x=c(0.05,0.25),lwd=0.5,col=cols[2],lty='dashed')
  lines(y=rep(0.1,2),x=c(0.05,0.25),lwd=0.5,col=cols[3],lty='dashed')
  text(x=0.05,y=0.4,adj=0,expression(pi==0.5),cex=0.75)
  text(x=0.3,y=c(0.3,0.2,0.1),
       c(expression(rho==1),expression(rho==3),expression(rho==9)),cex=0.75,adj=0)
  

dev.off()

#0.25*0.75*(1-0.5)/(1-0.25*0.5)


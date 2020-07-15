
omega = 1
alpha0 = c(0.95,0.9,0.8,0.7,0.6)
rho = c(1,3)
thetaS = c(1,0.75,0.5)
alpha1 = c(1,0.95,0.8,0.6)
pi = c(0.1,0.5)
thetaP = seq(0,1.5,0.01)

or0 = or1 = array(NA,dim=c(length(alpha0),
                           length(alpha1),
                           length(rho),
                           length(thetaS),
                           length(pi),
                           length(thetaP)))

for (i in 1:length(alpha0)) for (j in 1:length(alpha1)) for (k in 1:length(rho)) for (l in 1:length(thetaS)) for (m in 1:length(pi)) for (n in 1:length(thetaP)){
  or0[i,j,k,l,m,n] = (alpha1[j]*rho[k] + alpha0[i]*omega)*(1-alpha0[i]*pi[m])/(alpha0[i]*(pi[m]*rho[k]*(1-alpha1[j]) + omega*(1-alpha0[i]*pi[m])))
  or1[i,j,k,l,m,n] = (alpha1[j]*thetaP[n]*rho[k] + alpha0[i]*omega)*(1-alpha0[i]*thetaS[l]*pi[m])/(alpha0[i]*(thetaS[l]*pi[m]*thetaP[n]*rho[k]*(1-alpha1[j]) + omega*(1-alpha0[i]*thetaS[l]*pi[m])))
}


#thetaPest[1,1,1,1,1,]-thetaP

thetaPest = (or1-1)/(or0-1)

setwd('~/Google drive (jlewnard@berkeley.edu)/pcv effectiveness/methods paper')
pdf('sens bias plot.pdf',width=6,height=5.5)
layout(matrix(c(1:(6*3),rep(19,6)),nrow=4,byrow=T),widths=rep(c(1.25,1,1),2),heights=c(1,1,1,0.25))

cols = c('darkgoldenrod3','cadetblue4','darkorchid3')
ltys = c('solid','dashed')
left = c(3,1.5,1.5)
ymin = -0.5
par(mgp=c(3,0.35,0))
par(tck=-0.04)
#### with alpha0 = 1
count = 1
for (i in c(1,3,5)){
  for (k in 1:2){ ### panels for rho values (left right)
    for (l in 1:3){ ### plots for each thetaS value
      par(mar=c(3.5,left[l],3,0.5))
      plot(y=1,x=1,axes=F,ann=F,xlim=c(-0.5,1),ylim=c(ymin,1),type='n') 
      abline(h=0,v=0,lwd=0.5,col='grey')
      lines(x=c(-10,10),y=c(-10,10),col='grey',lwd=0.5)
      for (j in 2:4){ ### colors for each alpha1 value
        for (m in 1:2){ ### dotted/dashed for each pi value
          lines(y=1-thetaPest[i,j,k,l,m,],x=1-thetaP,col=cols[j-1],lty=ltys[m],lwd=0.5)       
        }
      }
      box(bty='l',lwd=0.5)
      text(x=seq(-0.5,1,0.5),y=ymin*1.35,seq(-0.5,1,0.5)*100,cex=0.65,srt=45,xpd=T,adj=1)
      axis(1,at=seq(-0.5,1,0.25),lwd=0,lwd.ticks=0.5,labels=NA)
      axis(2,at=seq(ymin,1,0.25),lwd=0,lwd.ticks=0.5,labels=seq(ymin,1,0.25)*100,las=1,cex.axis=0.65)
      if (l==1){
        mtext(expression(paste(hat(VE)[italic(P)],', %',sep='')),side=2,line=1.5,cex=0.55)
      }
      mtext(expression(paste(VE[italic(P)]==1-theta[italic(P)],', %',sep='')),side=1,las=1,line=1.5,cex=0.55)
      mtext(side=3,paste(LETTERS[count],')',sep=''),adj=0,cex=0.55)
      if (l==1){mtext(side=3,expression(theta[italic(S)]==1),adj=1,cex=0.55)} else{
        if (l==2){mtext(side=3,expression(theta[italic(S)]==0.75),adj=1,cex=0.55)} else{
          mtext(side=3,expression(theta[italic(S)]==0.5),adj=1,cex=0.55)
        }
      }
      if (i==1&l==1&k==1){mtext(expression(paste(alpha[0]==0.95,', ',rho==1,sep='')),side=3,line=1.5,adj=0,cex=0.65)} else{
        if (i==1&l==1&k==2){mtext(expression(paste(alpha[0]==0.95,', ',rho==3,sep='')),side=3,line=1.5,adj=0,cex=0.65)} else{
          if (i==3&l==1&k==1){mtext(expression(paste(alpha[0]==0.8,', ',rho==1,sep='')),side=3,line=1.5,adj=0,cex=0.65)} else{
            if (i==3&l==1&k==2){mtext(expression(paste(alpha[0]==0.8,', ',rho==3,sep='')),side=3,line=1.5,adj=0,cex=0.65)} else{
              if (i==5&l==1&k==1){mtext(expression(paste(alpha[0]==0.6,', ',rho==1,sep='')),side=3,line=1.5,adj=0,cex=0.65)} else{
                if(i==5&l==1&k==2){mtext(expression(paste(alpha[0]==0.6,', ',rho==3,sep='')),side=3,line=1.5,adj=0,cex=0.65)}
              }
            }
          }
        }
      }
      count = count+1
    } 
  }  
}
par(mar=c(0,0,0,0))
plot(1,type='n',axes=F,ann=F,xlim=c(0,1),ylim=c(0,1))
ys = c(1,0.8,0.5,0.2)
for (i in 2:4){
  lines(x=c(0.1,0.2),y=rep(ys[i],2),lwd=0.5,col=cols[i-1])
  lines(x=c(0.4,0.5),y=rep(ys[i],2),lwd=0.5,col=cols[i-1],lty='dashed')
}
text(x=0.22,y=ys[2:4],
     c(expression(paste(alpha[1]==0.95,', ',pi==0.1,sep='')),
       expression(paste(alpha[1]==0.8,', ',pi==0.1,sep='')),
       expression(paste(alpha[1]==0.6,', ',pi==0.1,sep=''))),
     adj=0,cex=0.75)
text(x=0.52,y=ys[2:4],
     c(expression(paste(alpha[1]==0.95,', ',pi==0.5,sep='')),
       expression(paste(alpha[1]==0.8,', ',pi==0.5,sep='')),
       expression(paste(alpha[1]==0.6,', ',pi==0.5,sep=''))),
     adj=0,cex=0.75)
dev.off()



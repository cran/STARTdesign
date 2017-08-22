
findparameter<- function(p0,p1,alpha1,beta1,alpha2,beta2)
{

if (alpha1<alpha2 || beta1>beta2) return("Please ensure alpha1>alpha2 and beta1<beta2.")

findn1andl1<-function(p0,p1,alphatarget,powertarget,n1min=10,n1max=60){

mat=matrix(0,nrow=0,ncol=2)
for (n1 in seq(n1min,n1max,by=1))
{
	for (l1	 in seq(0,n1,by=1))
	{
		if (1-pbinom(l1,n1,p1)>powertarget && 1-pbinom(l1,n1,p0)<alphatarget)
			mat=rbind(mat,c(n1,l1))
	}
}

return(mat)
}





findl1<-function(n1,pminimum,p0,p1,alpha,powertarget,powermin){
l1=0
for (l1 in seq(0,n1,by=1)){
if (pbinom(l1,n1,pminimum)>1-alpha &&1-pbinom(l1,n1,p1)>powertarget&&1-pbinom(l1,n1,pminimum+p1-p0)>powermin) {return(l1)}
}
return(l1)
}

powerpop <- function(p0,p1,t2,n1,n2,l1,u1){
	
	return(rejprob(p1, p0, l1, n1, n2, t2))
}

ssize<-function(ps,pe,aa,bb){
pbar=(ps+pe)/2
2/(pe-ps)/(pe-ps)*(qnorm(1-aa)*sqrt(2*pbar*(1-pbar))+qnorm(1-bb)*sqrt(pe*(1-pe)+ps*(1-ps)))^2
}

alpha =alpha2
powert = 1- beta2

powert1 = 1 - beta1

nn=as.integer(ssize(p0,p1,alpha,1-powert)/2)
mat=matrix(0,nrow=0,ncol=9)
n1l1=findn1andl1(p0,p1,alpha1,powert1)
t2=qnorm(1-alpha)

for (i in seq(1,length(n1l1[,1])))
{
	n1 = n1l1[i,1]
	l1 = n1l1[i,2]
	u1=n1+1

	tempp<-function(n2)
	{return(try(powerpop(p0,p1,t2,n1,n2,l1,u1)+1-pbinom(u1,n1,p1)+dbinom(u1,n1,p1)-powert))}
	
	nmax=nn*2
	if(!(inherits(tempp(nmax), "try-error")))
	{		
		y2root=0
		while(tempp(nmax)<0){nmax=nmax+20}
		if (tempp(10)>0 ) 
		{
			y2root = 10
		}else {
			y2root=as.integer(uniroot(tempp,c(10,nmax))$root)
			while (tempp(y2root)<0) {y2root=y2root+1}
		}

		n2=y2root
		power1=tempp(n2)+powert
		type1=powerpop(p0,p0,t2,n1,n2,l1,u1)+1-pbinom(u1,n1,p0)+dbinom(u1,n1,p0)
		type1_p1=powerpop(p1,p1,t2,n1,n2,l1,u1)+1-pbinom(u1,n1,p0)+dbinom(u1,n1,p0)
		ess0=n1+2*n2*(pbinom(u1-1,n1,p0)-pbinom(l1,n1,p0))
		ess1=n1+2*n2*(pbinom(u1-1,n1,p1)-pbinom(l1,n1,p1))
		
		mat=rbind(mat,c(n1,n2,l1,power1,type1,type1_p1,ess0,ess1,ess0/2+ess1/2))
		 		#write(c(n1,n2,l1,u1,power1,type1,ess0,ess1,essb0,pbinom(l1,n1,(p1+p0)/2),1-pbinom(u1,n1,(p1+p0)/2)+dbinom(u1,n1,(p1+p0)/2)),file=paste(n1,".csv"),append=T,sep=",",ncol=11)
		
			
  }
}

  a=mat[which(mat[,9]==min(mat[,9])),]

  return(list(n1=a[1],n2=a[2],r1=a[3],ess0=a[7],ess1=a[8],asn=a[9]))

}




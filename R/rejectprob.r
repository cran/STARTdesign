
rejectprob<-function(pe,ps,n1,n2,r1,z)
{
	p1=pe
	p0=ps
	l1=r1
	t2=z
	return(rejprob(p1, p0, l1, n1, n2, t2))
}
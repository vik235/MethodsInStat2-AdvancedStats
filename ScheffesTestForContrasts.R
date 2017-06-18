#*******************************************************************************************************#
#Author: Vivek Gupta, Date 6/18/17
#Purpose: To run multiple contrats tests. Find orthogonals estimate them along with standard errors. 


#CONTRASTS under hypothesis : c1,c2,c3,c4 forms the hypothesis matrix under simultaneous tests. 
c1 = c(4 ,-1 ,-1, -1, -1)
c2 = c(-2 ,-1, 0, 1, 2)
c3 = c(2 ,-1 ,-2 ,-1, 2)
c4 = c(-1, 2, 0, -2, 1)


#Measurements 
y1.=c(10.2,10.8,10.1,10.9,11.1,11.8,11.3,11.9,9.3,9.9)
y2.=c(9.2,9.8,9.1,9.9,10.1,10.8,10.3,10.9,9.3,9.8)
y3.=c(9.0,9.9,9.2,9.8,10.0,10.8,10.2,10.7,9.9,9.0)
y4.=c(8.1,8.1,8.0,8.9,8.2,8.9,8.1,8.8,9.2,9.9)
y5.=c(7.2,7.8,7.1,7.9,8.1,8.8,8.3,8.9,9.3,9.8)
sigmaEHat=0.731422 #from SAS AOV
yi.Bar=c(mean(y1.),mean(y2.),mean(y3.),mean(y4.),mean(y5.))

r=10
t=5
alpha=.05
N=40
#a) Orthogonality check 
#Orthogonal to C1 

sum(c1*c2)
sum(c1*c3)
sum(c1*c4)
#none are orthogonal. 

#Orthogonal to C2 
sum(c2*c3)
sum(c2*c4)
#Thus c2, c3, c4 seems to be orthogonal provided c3*c4 is orthogonal as well

#Orthogonal to C3
sum(c3*c4)
#This confirms contrasts c2,c3 , c4 are mututally orthogonal 

#b) Estimates of Constrasts along with standard error 

#C1 estimate 
c1Hat=sum(c1*yi.Bar)

se_c1=sigmaEHat*sqrt(sum((c1^2)/r))

#C2 estimate 
c2Hat=sum(c2*yi.Bar)

se_c2=sigmaEHat*sqrt(sum((c2^2)/r))

#C3 estimate 
c3Hat=sum(c3*yi.Bar)

se_c3=sigmaEHat*sqrt(sum((c3^2)/r))

#C3 estimate 
c4Hat=sum(c4*yi.Bar)
se_c4=sigmaEHat*sqrt(sum((c4^2)/r))

#c)
#Use the Scheffe test at the alpha = :05 level of significance to test the significance of the four contrasts.

#Significance of C1 , Ho: C1=0 H1: C1!=0
Dc1=sqrt(sum((c1^2)/r))
Sc1= Dc1*sigmaEHat*sqrt((t-1)*qf((1- alpha),(t-1),(N-t)))

(abs(c1Hat)>= Sc1) #TRUE, C1 is significant 

#Significance of C2 , Ho: C2=0 H1: C2!=0
Dc2=sqrt(sum((c2^2)/r))
Sc2= Dc2*sigmaEHat*sqrt((t-1)*qf((1- alpha),(t-1),(N-t)))

(abs(c2Hat)>= Sc2) #TRUE, C2 is significant 

#Significance of C3 , Ho: C3=0 H1: C3!=0
Dc3=sqrt(sum((c3^2)/r))
Sc3= Dc3*sigmaEHat*sqrt((t-1)*qf((1- alpha),(t-1),(N-t)))

(abs(c3Hat)>= Sc3) #FALSE, C3 is NOT significant 

#Significance of C4 , Ho: C4=0 H1: C4!=0
Dc4=sqrt(sum((c4^2)/r))
Sc4= Dc4*sigmaEHat*sqrt((t-1)*qf((1- alpha),(t-1),(N-t)))

(abs(c4Hat)>= Sc4) #FALSE, C4 is NOT significant 




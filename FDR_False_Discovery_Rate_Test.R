#*******************************************************************************************************#
#Author: Vivek Gupta
#Purpose: To control for FDR when simultaneous multiple comparisons are done. Other methods, FWER and PCER  

# FDR i.e False Discovery Rate control of simultaneous M tests to control Type I errors when no. of contrasts are large. 
#How large: Not too sure but M>50's or 1000s
#Process = 
#1. There are m null hypotheses, Ho1;Ho2; : : : ;Hom tested, yielding p-values, p1; p2; : : : ; pm
#2. The p-values in ordered form are p(1), p(2) ...,p(m) i.e. Order statistics
#3. Define qi = m*p(i)/i for i = 1,2,.....,m, where i is the rank of p(i) amongst the m P-values
#4. Let FDRi = min(qi,.. qM) be the false discovery rate for the i-th test
#5. Determine the largest i such that FDRi < FDRo, where FDRo is the critical level for the
#false discovery rate (typically .05)
#6. Reject Hoj for the null hypotheses j = 1...i and fail to reject Hoj for all the remaining
#null hypotheses

#This will work if p is already sorted. Havent tested thoroughly for an unordered initial p values. 

p <- c(.0001,.0058,.0132,.0289,.0498,.0911,.2012,.5718,.8912,.9011) #Individual pvalues
p_order <- sort(p) #Order staistics of pvalues
p_orderi <- order(p) #Rank in p

qi <- length(p)*p_order/p_orderi #Step 3 above. 
#If a subet of tests are used change the value of length(p) to apropriate test

fdri <- rep(0,length(p)) #Holds Controlled FDR's

for (i in 1:length(p)) {
  fdri[i]=min(qi[i:length(qi)])
}

fdrCr=.05 #Critical value to be controlled against 

p_orderi[which(fdri < fdrCr)] #lists the test numbers which are significant when controlled for FDR

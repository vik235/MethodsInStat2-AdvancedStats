ods html; ods graphics on;
option ls=75 ps=50 nocenter nodate;
title 'Storage of meat by four methods';
/* Read the data as is int form of a table.
Input records in below fasion for quick input
Discard Y1-Y5 from output*/

data dataset;
array Y Y1-Y5;
input Trt $ Y1-Y5;
do over Y;
Response=Y;
output; end; drop Y1-Y5;
cards;
COMM 7.66 6.98 7.80 . .
VAC 5.26 5.44 5.80 . .
MIXED 7.41 7.33 7.04 7.59 .
CO2 3.51 2.91 3.66 2.87 3.04
run;

/*Box plot of response per treatment*/
proc boxplot data=dataset;
plot Response*Trt;
run;

/*Invoke general linear mized model proc to get Least Squares means and their CI's
Not: If reps are equal width should be same for the CI's*/
proc glimmix data=dataset order=data;
class Trt;
model Response=Trt/ Solution;
lsmeans Trt/plot = meanplot cl ;
run;

/*Run AOV and check for equal variances in reponse*/
proc glm data=dataset order=data;
class Trt;
model Response=Trt/ Solution;
lsmeans Trt /stderr cl ;
means Trt /HOVTEST=bf;
output out=ASSUMP R=RESID P=PRED;
run;

proc univariate def=5 plot normal; var RESID;
run;
ods graphics off; ods html close;

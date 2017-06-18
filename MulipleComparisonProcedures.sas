ods html; ods graphics on;
option ls=80 ps=50 nocenter nodate;
title 'Heat Loss Data Analysis';

data heatloss; array Y Y1-Y10;
input Thickness $ Y1-Y10; do over Y; HeatLoss=Y; output; end;
drop Y1-Y10;
label Thickness  = 'Thickness of Coating' HeatLoss = 'Heat Loss';
cards;
T0 10.2 10.8 10.1 10.9 11.1 11.8 11.3 11.9 9.3 9.9
T20 9.2 9.8 9.1 9.9 10.1 10.8 10.3 10.9 9.3 9.8
T40 9.0 9.9 9.2 9.8 10.0 10.8 10.2 10.7 9.9 9.0
T60 8.1 8.1 8.0 8.9 8.2 8.9 8.1 8.8 9.2 9.9
T80 7.2 7.8 7.1 7.9 8.1 8.8 8.3 8.9 9.3 9.8

run;

/*Plot the data across thickness level to see the association between the two*/
proc plot; 
plot HeatLoss*Thickness='+';
run;

/*Run GLM proc to get estimates of each contrasts and its StdErr*/
proc glm data= heatloss order=data;
class Thickness;
model Heatloss=Thickness;
estimate 'C1:Control vs Average of means of others' Thickness 4 -1 -1 -1 -1;
estimate 'C2:Linear trend across 5 thickness lvls' Thickness -2 -1 0 1 2;
estimate 'C3:Quadratic trend across 5 thickness lvls' Thickness 2 -1 -2 -1 2;
estimate 'C4:Cubic trend across 5 thickness lvls' Thickness -1 2 0 -2 1;

run;

title 'Polynomial trend comparison bewtween treatments.';
proc glm data= heatloss order=data;
class Thickness;
model Heatloss=Thickness /ss3;
contrast 'C2:Linear trend across 5 thickness lvls' Thickness -2 -1 0 1 2;
contrast 'C3:Quadratic trend across 5 thickness lvls' Thickness 2 -1 -2 -1 2;
contrast 'C4:Cubic trend across 5 thickness lvls' Thickness -1 2 0 -2 1;
run;

title'Multiple simulaneous contrasts comparison ';
proc glm data= heatloss order=data;
class Thickness;
model Heatloss=Thickness;
contrast 'ALL' 
		 		Thickness -2 -1 0 1 2,
		 		Thickness 2 -1 -2 -1 2,
 		 		Thickness -1 2 0 -2 1;

run;

title'Dunnetts Multiple pairwise comparison with control';
proc glm data= heatloss order=data;
class Thickness;
model Heatloss=Thickness;
lsmeans thickness /cl pdiff=controll('T0') adjust=DUNNETT alpha=.05;
		 		
run;

title'Tukeys Multiple pairwise comparison ';
proc glm data= heatloss order=data;
class Thickness;
model Heatloss=Thickness;
lsmeans thickness /cl pdiff adjust=tukey alpha=.05;
run;

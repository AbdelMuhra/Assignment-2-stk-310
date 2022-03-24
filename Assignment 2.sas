*import csv data;
goptions reset=all;
title1 '26 music videos randomly selected from the laptop of paul van staden';
proc import out=videos datafile='/home/u58281916/u19273551/STK 310/Assignment 1/videos.csv'
dbms=csv replace;
getnames=yes;
datarow=2;
run;
proc print data=videos; 
run;

*Question 1 a;
goptions reset=all;
title1 'Correlation analysis';
proc corr data=videos;
var FileSize SongLength;
run;

*Question 1 b-g;
goptions reset=all;
symbol1 color=vib value=dot width=2;
title1 'Ordinary Least Squares (OLS) regression';
proc reg data=videos plot=none;
model FileSize=SongLength / covb;
plot FileSize*SongLength;
run;

*Question 2;
data ohnedich; set videos;
od = 1;
if ArtistorBand = "Rammstein" then od = 0;
run;

goptions reset=all;
title1 'Correlation analysis: Ohne Dich excluded';
proc corr data=ohnedich exclnpwgt;
var FileSize SongLength;
weight od;
run;

goptions reset=all;
symbol1 color=bib value=dot width=2;
title1 'Ordinary Least Squares (OLS) regression: Ohne Dich excluded';
proc reg data=videos plot=none;
model FileSize=SongLength / covb;
reweight ArtistorBand="Rammstein";
refit;
plot FIleSize*SongLength;
run;


*Question 3;
goptions reset=all;
title1 'Ordinary Least Squares (OLS) regression with the IML Procedure';
data the_data; set videos;
x=SongLength;
y=FileSize;
keep x y;
run;

proc iml;
use the_data;
read all into x_and_y;
x=x_and_y[,1];
y=x_and_y[,2];
n=nrow(x_and_y); *** (a) ***;
sum_x=sum(x); *** (b) ***;
sum_y=sum(y); *** (c) ***;
sum_x2=ssq(x); *** (d) ***;
sum_xy=x`*y; *** (e) ***;
xbar=sum_x/n; *** (f) ***;
ybar=sum_y/n; *** (g) ***;
betahat2=(n#sum_xy-sum_x#sum_y)/(n#sum_x2-sum_x##2); *** (h) ***;
betahat1=ybar-betahat2#xbar; *** (i) ***;
sum_xd2=ssq(x-xbar); *** (j) ***;
tss=ssq(y-ybar); *** (k) ***;
ess=betahat2##2#sum_xd2; *** (l) ***;
rss=ssq(y-betahat1-betahat2#x); *** (m) ***;
r2=ess/tss; *** (n) ***;
mse=rss/(n-2); *** (o) ***;
se_beta1=sqrt((mse#sum_x2)/(n#sum_xd2)); *** (p) ***;
se_beta2=sqrt(mse/sum_xd2); *** (q) ***;
covbetas=-xbar#se_beta2##2; *** (r) ***;
print n sum_x sum_y sum_x2 sum_xy xbar ybar betahat2 betahat1;
print sum_xd2 tss ess rss r2 mse se_beta1 se_beta2 covbetas;
quit; 


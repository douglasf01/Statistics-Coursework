/*
MSA680 lab 2: linear mixed models in SAS. Written for SAS OnDemand / SAS Studio.
Upload the three .sas7bdat files and set datadir to the folder that holds them.
*/
%let datadir = .;

/*
Encoding the girls as 0
*/
DATA growth2;
set "&datadir/growth2.sas7bdat";
if SEX = 2 then SEX = 0;
RUN;

DATA growthax;
set "&datadir/growthax.sas7bdat";
if SEX = 2 then SEX = 0;
RUN;

DATA growthcc;
set "&datadir/growthcc.sas7bdat";
if SEX = 2 then SEX = 0;
RUN;

/*
Untransformed age variable
Using IDNR as unique identifier
Using V=1 yields the covariance matrix asked for in 2. Models Q6. Note that it gives it
for the idnr 1, but its the same for all idnr. 
*/

proc mixed data=growthcc;
  class IDNR SEX;
  model MEASURE = AGE SEX AGE*SEX / solution ddfm=kr chisq OUTP=pred OUTPM=ave3;	
  random intercept age / subject=IDNR type=un V=1, 3 G;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1,3;
run;


proc mixed data=growthax;
  class IDNR SEX;
  model MEASURE = AGE SEX AGE*SEX / solution ddfm=kr chisq OUTP=pred OUTPM=ave2;	
  random intercept age / subject=IDNR type=un V=1, 3;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1,3;
run;

proc mixed data=growth2;
  class IDNR SEX;
  model MEASURE = AGE SEX AGE*SEX / solution ddfm=kr chisq OUTP=pred OUTPM=ave1;	
  random intercept age / subject=IDNR type=un V=1, 3;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1,3;
run;

/*
Transformed age variable
*/

data transformed_growthcc;
  set growthcc;
  AGE2 = (AGE-11)/3;
run;

data transformed_growthax;
  set growthax;
  AGE2 = (AGE-11)/3;
run;

data transformed_growth2;
  set growth2;
  AGE2 = (AGE-11)/3;
run;


proc mixed data=transformed_growthcc;
  class IDNR SEX;
  model MEASURE = AGE2 SEX AGE2*SEX / solution ddfm=kr chisq OUTP=pred6 OUTPM=ave6;	
  random intercept AGE2 /  subject=IDNR type=un V=1, 3;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1,3;
run;


proc mixed data=transformed_growthax;
  class IDNR SEX;
  model MEASURE = AGE2 SEX AGE2*SEX / solution ddfm=kr chisq OUTP=pred5 OUTPM=ave5;	
  random intercept AGE2 /  subject=IDNR type=un V=1,3;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1,3;
run;

proc mixed data=transformed_growth2;
  class IDNR SEX;
  model MEASURE = AGE2 SEX AGE2*SEX / solution ddfm=kr chisq OUTP=pred4 OUTPM=ave4;	
  random intercept AGE2 / subject=IDNR type=un V=1,3; 
  REPEATED / TYPE=VC SUBJECT = IDNR R=1,3;
run;

/*
Mean profiles over all models used
*/

PROC SORT DATA = ave1;
BY AGE;
RUN;

PROC SORT DATA = ave2;
BY AGE;
RUN;

PROC SORT DATA = ave3;
BY AGE;
RUN;

PROC SORT DATA = ave4;
BY AGE;
RUN;

PROC SORT DATA = ave5;
BY AGE;
RUN;

PROC SORT DATA = ave6;
BY AGE;
RUN;

title "Mean response profiles for growth2 dataset";
PROC SGPLOT DATA = ave1;
SERIES Y= pred X=AGE / GROUP = SEX;
RUN;

title "Mean response profiles for growthax dataset";
PROC SGPLOT DATA = ave2;
SERIES Y= pred X=AGE / GROUP = SEX;
RUN;

title "Mean response profiles for growthcc dataset";
PROC SGPLOT DATA = ave3;
SERIES Y= pred X=AGE / GROUP = SEX;
RUN;

title "Mean response profiles for transformed growth2 dataset";
PROC SGPLOT DATA = ave4;
SERIES Y= pred X=AGE2 / GROUP = SEX;
RUN;

title "Mean response profiles for transformed growthax dataset";
PROC SGPLOT DATA = ave5;
SERIES Y= pred X=AGE2 / GROUP = SEX;
RUN;

title "Mean response profiles for transformed growthcc dataset";
PROC SGPLOT DATA = ave6;
SERIES Y= pred X=AGE2 / GROUP = SEX;
RUN;

/* 
Trying NOBOUND on the full dataset
*/

proc mixed data=growthcc nobound;
  class IDNR SEX;
  model MEASURE = AGE SEX AGE*SEX / solution ddfm=kr chisq OUTP=pred OUTPM=ave3;	
  random intercept age / subject=IDNR type=un V=1 G;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1;
run;

proc mixed data=transformed_growthcc nobound;
  class IDNR SEX;
  model MEASURE = AGE2 SEX AGE2*SEX / solution ddfm=kr chisq OUTP=pred OUTPM=ave3;	
  random intercept age2 / subject=IDNR type=un V=1 G;
  REPEATED / TYPE=VC SUBJECT = IDNR R=1;
run;

/*
2.1, only on complete data
*/

proc mixed data=growthcc;
  class IDNR SEX;
  model MEASURE = AGE SEX AGE*SEX / solution ddfm=kr; 	
  random intercept / subject=IDNR;
  repeated / subject=idnr type=AR(1);
run;

proc mixed data=growthcc;
  class IDNR SEX;
  model MEASURE = AGE SEX AGE*SEX / solution ddfm=kr;  	
  random intercept / subject=IDNR;
  repeated  / subject=idnr type=AR(1) local;
run;

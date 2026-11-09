/*
MSA680 lab 1: ANCOVA in SAS. Written for SAS OnDemand / SAS Studio.
Upload diet.txt and set datadir to the folder that holds it.
*/
%let datadir = .;

DATA diet;
    infile "&datadir/diet.txt" firstobs=2;
    input Diet PostChol PreChol;
  RUN;
  
  PROC MEANS data=diet mean noprint;
    var PreChol;
    output out=mean_prechol(drop=_TYPE_ _FREQ_) mean=mean_prechol;
  RUN;
  
  DATA _NULL_;
    set mean_prechol;
    call symput('mean_prechol', mean_prechol);
  RUN;
  
  DATA diet_centered;
    set diet;
    PreChol_centered = PreChol - &mean_prechol; /* replace mean_prechol with the actual mean */
  RUN;
  
  
  PROC GLM data=diet;
    CLASS Diet;
    MODEL PostChol = PreChol Diet Diet*PreChol /solution;
    LSMEANS Diet /cl;
    LSMEANS Diet / cl pdiff at PreChol=190; 
    LSMEANS Diet / cl pdiff at PreChol=250;
  
  RUN;

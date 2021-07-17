/* CT_6.sas */
/* Source File:  */
/* Source Path: /home/u48661312/MIS581 */
/* Code created on: 7/11/21 */

/* Assign a libname to the MIS543 SAS folder. */
LIBNAME MIS581 '/home/u48661312/MIS581';

%web_drop_table(MIS581.h212_capstone);

proc sql;
select CHGJ3142, YCHJ3142, count(*)
from MIS581.h212
where EMPST31=1 and EMPST42=1 and EMPST53=1
group by CHGJ3142, YCHJ3142
order by CHGJ3142, YCHJ3142
;
quit;

/* Create dataset with necessary attributes to perform tests. */
data MIS581.h212_capstone;
set MIS581.h212(keep=CHGJ3142 CHGJ4253 YCHJ3142 YCHJ4253 inscov19 dntins19
EMPST31 EMPST42 EMPST53 HASFSA31 REGION19 retpln31 retpln42 retpln53
sicpay31 sicpay42 sicpay53 paydr31 paydr42 paydr53 payvac31 payvac42 payvac53);

/* Filter out respondents who have not had the same job throught the surveys */
if empst31=1 AND empst42=1 AND empst53=1;

/* Determine job satisfaction */
job_satisfaction=-1;
if chgj3142=4 and chgj4253=4 then job_satisfaction=1;
if (chgj3142 not in (-15,-1,4) and ychj3142 in (5,6,7,8,9,10)) or 
   (chgj4253 not in (-15,-1,4) and ychj4253 in (5,6,7,8,9,10)) then job_satisfaction=0;

/* Only keep those with known job satusfaction. */	  
if job_satisfaction in (0,1);

/* Define medical benefits as those with health or dental insurance */
if inscov19 in (1,2) or dntins19=1 then med_benefits=1;
else med_benefits=0;

/* Define fringe benefits as those with a retirment plan, sick pay, pay time to doctors,
   pay vacation or has FSA account */
if retpln31=1 or retpln42=1 or retpln53=1 or hasfsa31=1 or
   sicpay31=1 or sicpay42=1 or sicpay53=1 or paydr31 = 1 or 
	paydr42 = 1 or paydr53 = 1 or payvac31 = 1 or payvac42 = 1 or 
	payvac53 = 1 then fringe_benefits=1;
else fringe_benefits=0;

/* Define has FSA */
if HASFSA31=1 then HASFSA_flg=1;
else HASFSA_flg=0;

/* Define Health Insurance */
if inscov19 in (1,2) then inscov=1;
else inscov=0;

/* Define Dental Insurance */
if dntins19=1 then dntins=1;
else dntins=0;

/* Define Retirement Plan */
if retpln31=1 or retpln42=1 or retpln53=1 then retirement=1;
else retirement=0;

/* Define Sick Pay */
if sicpay31=1 or sicpay42=1 or sicpay53=1 then sickpay=1;
else sickpay=0;

/* Define Pay time to Doctors */
if paydr31 = 1 or paydr42 = 1 or paydr53 = 1 the paydr=1;
else paydr=0;

/* Define Paid Vacation */
if payvac31 = 1 or payvac42 = 1 or payvac53 = 1 then payvac=1;
else payvac=0;

run;

/* Display summary statistics */
proc means data=mis581.h212_capstone; 
var region19 job_satisfaction med_benefits fringe_benefits HASFSA_flg inscov
dntins retirement sickpay paydr payvac;
run;

/* Generate GLM to get Interaction Plot for job satisfaction in relation to health and/or dental coverage */
proc reg data=MIS581.h212_capstone;
model job_satisfaction=med_benefits;
Title "LOGISTIC REGRESSION ON Job Satisfaction in relation Health and/or Dental Coverage";
run;
quit;

/* Generate GLM to get Interaction Plot for job satisfaction in relation to fringe benefits */
proc reg data=MIS581.h212_capstone;
model job_satisfaction=fringe_benefits;
Title "LOGISTIC REGRESSION ON Job Satisfaction in relation to Fringe Benefits";
run;
quit;

/* Model used to determine the variables most effecting job satisfaction */
proc logistic data=MIS581.h212_capstone descending;
model job_satisfaction=inscov dntins HASFSA_flg retirement sickpay paydr payvac/
EXPB
SELECTION=stepwise
slentry=0.5
slstay=0.5
risklimits
;
TITLE "Determine attributes attributing to Job Satisfaction"
;
run;
quit;

/* Generate linear regression to determine job satisfaction in relation to US Region */
proc reg data=MIS581.h212_capstone;
model job_satisfaction=region19;
Title "LOGISTIC REGRESSION ON Job Satisfaction ON US Region";
run;
quit;

libname pdata "C:\Users\shrutz\Desktop\SAS_Programs\SAS_data";
run;

proc copy in=pdata out=work;
select attrition;
run;

proc means data=attrition NMISS N;
run;

/* 
proc summary data=sas_data.attrition;
output out=sas_data.attrition_sum;

run;


/*will check correlation after transforming data



proc corr data=sas_data.attrition;
run;
proc corr data=sas_data.attrition cov;
var AGE--EDUCATION; with Attrition;

run;*/

/*Business Travel has inherent order somebody travels more and somebody less*/

data attrition_new;
set attrition;
if (BusinessTravel='Non-Travel')
then BusinessTravel=0;
if (BusinessTravel='Travel_Rarely')
then BusinessTravel=1;
else BusinessTravel=2;

/*sales*/
/*we need not create new variables when we have 2 levels in a variable because we can encode it as binary */
if (Department='Sales')
then Department =0;
else Department=1;

if (Gender='Male')
then Gender=1;
else Gender=0;
if (MaritalStatus='Single')
then single=1;
else single=0;
/*marital status has 3 levels*/
if (MaritalStatus='Married')
then Married=1;
else Married=0;

if (Over18='Y')
then Over18=1;
else Over18=0;

if (OverTime='Yes')
then OverTime=1;
else OverTime=0;

if (Attrition='Yes')
then Attrition=1;
else Attrition=0;
run;


/*Education is already encoded we need not do anything*/ 

/*Education degree is a categorical variable without any order so will encode it*/
data attrition_new;
set attrition_new;
if (EducationField='Technical Deg')
then tech_deg=1;
else tech_deg=0;
if (EducationField='Medical')
then medical=1;
else medical=0;
if (EducationField='Marketing')
then marketing=1;
else marketing=0;
if (EducationField='Life Sciences')
then life_science=1;
else life_science=0;
if (EducationField='Human Resourc')
then human_res=1;
else human_res=0;
run;
/* nothing on env satisfaction, job_level and job involvement*/

data attrition_new;
set attrition_new;
if (JobRole='Sales Representative')
then sales_rep=1;
else sales_rep=0;
if (JobRole='Sales Executive')
then sales_exe=1;
else sales_exe=0;
if (JobRole='Research Scientist')
then res_scientist=1;
else res_scientist=0;
if (JobRole='Research Director')
then res_direct=1;
else res_direct=0;
if (JobRole='Manufacturing Director')
then manf_direct=1;
else manf_direct=0;
if (JobRole='Manager')
then manager=1;
else manager=0;
if (JobRole='Laboratory Technician')
then lab_tech=1;
else lab_tech=0;
if (JobRole='Human Resources')
then human_res=1;
else human_res=0;
run;
data data_Hr;
set attrition_new(drop= EducationField JobRole MaritalStatus);
run;

/* To convert variables to numerical */

proc export data= data_hr 
   outfile="C:\Users\shrutz\Desktop\SAS_Programs\SAS_data\hr_data"
   dbms=dlm; 
   delimiter=',';
 run;

 proc import datafile="C:\Users\shrutz\Desktop\SAS_Programs\SAS_data\hr_data" out=Hr_num dbms=dlm replace;
 delimiter=',';
   getnames=yes;
run;

proc copy in=work out=pdata;
select hr_num;
run;

/*standardizing the variables */

proc standard data=hr_num out=std_hr_num mean=0 std=1; 
var AGE BusinessTravel--lab_tech;
run;

/* Running the PCA on the standardised data */

proc princomp data=std_hr_num out=pca_hr;
var Age BusinessTravel--lab_tech;
run;

/** Running the logistic regression on the principal components. 28 principal components are considered as they explain almost 90%
of the variability **/

proc logistic data= pca_hr;
class attrition;
model attrition = Prin1--Prin28/OUTROC=ROC1;
quit;
/* When PCA is run on 28 principal components then the AUC comes out as 0.8338 */

/** Running PCA on all the standardised variables **/

proc logistic data= pca_hr;
class attrition;
model attrition = Age BusinessTravel--lab_tech/OUTROC=ROC1;
quit;
/* When logistic is run on all the standardised variables then the AUC comes out as 0.8599. */

/* Running the stepwise selection in logistic regression on both the standardized variables and principal components */

/* Running stepwise selection in logistic regression on standardized variables */

proc logistic data= pca_hr;
class attrition;
model attrition = Age BusinessTravel--lab_tech/selection=stepwise slentry=0.05 slstay=0.05 OUTROC=ROC1;
quit;

/* the variables which are getting selected after the stepwise selection on all the standardizd variables are: age, distance from home, 
environment satisfaction,job involvement, job satisfaction, no. of companies worked, overtime, relationship satisfaction, 
total working years, training time last year, work life balance, years at company, years in current role, years since last promotion, 
years with current manager, single, tech_deg, human_res, sales_rep, sales_exe, lab_tech.*/

/* Running logistic on principal components with stepwise selection procedure */

proc logistic data= pca_hr;
class attrition;
model attrition = Prin1--Prin28/selection=stepwise slentry=0.05 slstay=0.05 OUTROC=ROC1;
quit;

/* The Principal components 1,2,3,11,12,13,14,15,18,19,21,26,27 and 28 are being selected after the stepwise logistic regression on the
principal components. These principal components also have more imfluence/coefficient of the variables distance from home, environment
satisfaction, relationship stisfaction, human resource, tech_deg, job involvement, no. of companies worked, overtime, total working
years, years in current role, years since last promotion, sales representative, gender, etc. which is almost same as the variables
which came as the influential when logistic was ran on all the variables. */


/* Finding correlation between the selected variables for the model */

proc corr data=std_hr_num; /* Finding the correlation between the variables */
var Age DistanceFromHome EnvironmentSatisfaction JobInvolvement JobSatisfaction NumCompaniesWorked RelationshipSatisfaction TotalWorkingYears
TrainingTimesLastYear WorkLifeBalance YearsAtCompany YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager single tech_deg 
human_res sales_rep sales_exe lab_tech;
run; 

/* after finding the correlation between the variables it was found that the variables Total working years, Years at company and sales_exe
should be dropped as they increase the multicollinearity */

data pca_hr3;
set pca_hr(drop=TotalWorkingYears YearsAtCompany sales_exe);
run;

/* Running logistic regression with stepwise selection on the newly formed dataset */

proc logistic data= pca_hr3;
class attrition;
model attrition = Age BusinessTravel--lab_tech/selection=stepwise slentry=0.05 slstay=0.05 OUTROC=ROC1;
quit;

/* the variables now selected after this logistic regression are: Age, Business Travel, Department, Distance from home, Environment 
Satisfaction, Gender, Job Involvement, Job Level, Job Satisfaction, No. companies worked, over time, Relationship Satisfaction, 
Training Times last year, work life balance, Years in current role, years since last promotion, years with current manager, single,
tech_deg, human_res, sales_rep, lab_tech. These are almost the same as those selected previously just 2-3 are different.*/

/* Running proc reg to find out the VIF of these variables (Proc reg is run as with proc logistic VIF option is not available) */

proc reg data=pca_hr3;
model attrition =Age BusinessTravel Department DistanceFromHome EnvironmentSatisfaction Gender JobInvolvement JobLevel JobSatisfaction 
NumCompaniesWorked OverTime RelationshipSatisfaction TrainingTimesLastYear WorkLifeBalance YearsInCurrentRole YearsSinceLastPromotion 
YearsWithCurrManager single tech_deg human_res sales_rep lab_tech/vif;
run;

/* These variables have low VIF (all below 10) */

/* So, the final model is achieved by running thee following code: */

proc logistic data= pca_hr3; 
class attrition;
model attrition = Age BusinessTravel--lab_tech/selection=stepwise slentry=0.05 slstay=0.05 OUTROC=ROC1 ctable;
quit;

proc factor data=std_hr_num nfactors=18 scree rotate=Varimax out=fact_all; 
var AGE BusinessTravel--lab_tech;
run;
/* runnig factor analysis on data set - 18 factors generated */
  
proc logistic data= fact_all;
class attrition;
model attrition(event='1') = Factor1 Factor2 Factor3 Factor4 Factor5 Factor6 Factor7 Factor8 Factor9 Factor10 Factor11 Factor12 
Factor13 Factor14 Factor15 Factor16 Factor17 Factor18 
/ selection=stepwise slentry=0.01 slstay=0.01  OUTROC=ROC1 ctable;
quit;
 
/* running logiistic regression on 18 factors for attrition=1 
*/
proc logistic data= std_hr_num;
class attrition;
model attrition(event='1') = AGE BusinessTravel--lab_tech
/ selection=stepwise slentry=0.01 slstay=0.01  OUTROC=ROC1;
quit;

/* dividing dataset into training and test */

proc surveyselect data=std_hr_num method=srs samprate=0.8 out=Training;
run;

Data Validation;
merge std_hr_num(in=a) Training(in=b);
if a and not b;
run;

proc Logistic data=Training outmodel=outmod desc;
model attrition= AGE BusinessTravel--lab_tech
/ selection=stepwise slentry=0.05 slstay=0.05  OUTROC=ROC1;
output out=Outreg p=predicted;
run;

proc logistic inmodel=outmod;
score data=Validation out=Validation_scored;
run;

proc freq data=Validation_scored;
table attrition*P_1/noprint measures;
run;

Data Outreg;
set Outreg;
if predicted>=0.5 then attrition_pred=1;
else
attrition_pred=0;
run;

proc freq data=Outreg;
table attrition*attrition_pred;
run;





data temp;
set std_hr_num;
n=ranuni(8);
proc sort data=temp;
  by n;
  data training1 testing1;
   set temp nobs=nobs;
   if n<=.8*nobs then output training1;
    else output testing1;
   run;


   /* Running the model on training dataset */

data training_set1;
set training1(drop=TotalWorkingYears YearsAtCompany sales_exe);
run;

proc logistic data= training_set1 descending outmodel=training_model1; 
class attrition;
model attrition = Age BusinessTravel--lab_tech/selection=stepwise slentry=0.05 slstay=0.05 OUTROC=ROC1 ctable;
output out=train_reg1 p=predicted;
quit;

proc logistic inmodel=training_model1;
score data=testing1 out=testing_pred1;
run;

proc freq data=testing_pred;
table attrition*P_1/noprint measures;
run;







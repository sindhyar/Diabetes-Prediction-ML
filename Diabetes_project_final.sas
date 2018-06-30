*---------------------------------------------------;
* Project      : Final Project                      ;
* Prof 		   : Kasha Dehnad                		;
* Date		   : 12/13/2017							;
*---------------------------------------------------;


/*Step 1*/

*Importing data from csv File;
PROC IMPORT OUT= WORK.Diabetes 
            DATAFILE= "C:\Users\pares\Desktop\SAS_data\diabetes.csv" 
            DBMS=csv REPLACE; 
RUN;
/*Step 2 Data Understanding*/

/* Checking Contents of the data*/
Title" Contents of the Diabetes Data";
proc contents data=diabetes out=diabetes_z;
run;
/* Calculating Mean of the Variables*/
 proc means data=diabetes;
 run;
 /* calculating the missing values*/
 proc means data=diabetes nmiss;
 run;
/*Variable Exploration: Counting the freq of the predictors */
proc freq data=diabetes;
tables age BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction;
run; 
/*Step 3 Data Preparation*/

/*Replacing zero values with mean*/

data diabetes_z;
  set diabetes;
  if BloodPressure=0 then call missing(BloodPressure);
  if SkinThickness=0 then call missing(SkinThickness);
  if Insulin=0 then call missing(Insulin);
  if BMI=0 then call missing(BMI);
  if Glucose=0 then call missing(Glucose);
  run;
proc stdize data=diabetes_z out=diabetes_y
            method=mean reponly;
    var BloodPressure SkinThickness Insulin BMI Glucose;
run;
Title "Verfying the Dataset Diabetes_Y after replacing missing Value with the mean";
proc means data=diabetes_y;
run;
/*Outlier detection & Removal*/
proc reg data=diabetes_y;
model outcome=pregnancies glucose bloodpressure skinthickness insulin bmi diabetespedigreefunction age;
output out=diabetes_x (keep= outcome pregnancies glucose bloodpressure skinthickness insulin bmi diabetespedigreefunction age r) rstudent=r;
run;
quit;
proc univariate data=diabetes_x plot;
var r;
run;
quit;
data diabetes_new;
set diabetes_x;
if abs(r) >2 then delete;
run;
proc print data=diabetes_new;
run;
proc freq data=diabetes_new;
table age;
run;
/*Age== Age_Group

data diabetes_new;
set diabetes;
if Age>=21 and Age<=29 then Age_Group="21-30";
if Age>=30 and Age<=39 then Age_Group="31-40";
if Age>=40 and Age<=49 then Age_Group="41-50";
if Age>=50 and Age<=59 then Age_Group="51-60";
if Age>=60 and Age<=69 then Age_Group="61-70";
if Age>=70 and Age<=79 then Age_Group="71-80";
if Age>=80 and Age<=89 then Age_Group="81-90";
run;
Title "Checking the diabetes_new after Binning";
proc contents data=diabetes_new;
run; */
/* Calculating Correlation*/
ods graphics on;
title 'Diabetes Data';
proc corr data=diabetes_new nomiss plots=matrix(histogram);
   var pregnancies glucose age BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction outcome;
run;
ods graphics off;

ods graphics on;
proc corr data=diabetes_new nomiss
          plots=scatter(alpha=.20 .30);
   var pregnancies glucose age BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction outcome;
run;
ods graphics off;
ods graphics on;
proc sgscatter data=diabetes_new;
  title "Scatterplot Matrix for diabetes Data";
  matrix BloodPressure SkinThickness Insulin DiabetesPedigreeFunction pregnancies Glucose
         / group=outcome;
run; 
ods graphics off;
/* Variable Exploration with Univariate*/

proc univariate data=diabetes_new ;
var Pregnancies Glucose Insulin BMI SkinThickness DiabetesPedigreeFunction Bloodpressure ;
histogram Pregnancies Glucose Insulin BMI SkinThickness DiabetesPedigreeFunction Bloodpressure /normal;
run;
quit;
proc means data=diabetes_new;
run;

/*  Logistic Regression before splitting dataset

proc logistic data=diabetes_new desc;
model outcome=Pregnancies Glucose BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction / selection=stepwise;
run;
quit; */

/* Splitting Data into Training and Testing*/
Data Diabetes_Train;
set Diabetes_new;
n=ranuni(8);
proc sort data=Diabetes_Train;
  by n;
  data training testing;
   set Diabetes_new nobs=nobs;
   if _n_<=.7*nobs then output training;
    else output testing;
   run;
proc means data=training;
run;
    ods graphics on;

/*checking for multicollinearity*/
proc reg data=training;
model outcome=Pregnancies Glucose BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction age/vif;
run;

/*stepwise selection*/
proc logistic data=training desc;
model outcome=Pregnancies Glucose BloodPressure SkinThickness Insulin BMI DiabetesPedigreeFunction age /selection=stepwise;
run;

/*  Logistic Regression(using training data) and scoring of model using test data*/
proc logistic data=training;
        model outcome(event="1") = pregnancies glucose bmi diabetespedigreefunction / outroc=troc;
        score data=testing out=valpred outroc=vroc;
        run;
 data a; 
        set troc(in=training) vroc;
        data="valid"; 
        if training then data="training";
        run;
  proc sgplot data=a noautolegend aspect=1;
        xaxis values=(0 to 1 by 0.25) grid offsetmin=.05 offsetmax=.05; 
        yaxis values=(0 to 1 by 0.25) grid offsetmin=.05 offsetmax=.05;
        lineparm x=0 y=0 slope=1 / transparency=.7;
        series x=_1mspec_ y=_sensit_ / group=data;
       run;




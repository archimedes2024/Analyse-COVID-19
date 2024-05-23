					

**********************************************************************************;
* 							Question 1											  ;
**********************************************************************************;
%let Path=D:\Bureau\HEC\CERT\LOG SATISTIQUES_AUTOMNE_2020;

libname lib "&Path.";

PROC IMPORT DATAFILE="&path.\ISO_CODE_TABLE.csv"
            OUT=ISO_CODE_TABLE
			DBMS=CSV
            REPLACE;
			DELIMITER=",";
			GETNAMES=yes;
RUN;


PROC IMPORT DATAFILE="&path.\OWID_COVID_DATA_2019.csv"
            OUT=OWID_COVID_DATA_2019
			DBMS=CSV
            REPLACE;
			DELIMITER=",";
			GETNAMES=yes;
RUN;


PROC IMPORT DATAFILE="&path.\OWID_COVID_DATA_2020.csv"
            OUT=OWID_COVID_DATA_2020
			DBMS=CSV
            REPLACE;
			DELIMITER=",";
			GETNAMES=yes;
RUN;

**********************************************************************************;
* 							Question 2											  ;
**********************************************************************************;
* changer la formate des collones;

 data OWID_COVID_DATA_2019;
   set OWID_COVID_DATA_2019;
   new_tests2 = input(new_tests, 8.);
   drop new_tests;
   rename new_tests2=new_tests;
   total_tests2 = input(total_tests, 8.);
   drop total_tests;
   rename total_tests2=total_tests;
 run ;

data OWID_COVID_DATA_2020;
   set OWID_COVID_DATA_2020;
   new_tests2 = input(new_tests, 8.);
   drop new_tests;
   rename new_tests2=new_tests;
   total_tests2 = input(total_tests, 8.);
   drop total_tests;
   rename total_tests2=total_tests;
   female_smokers2= input(female_smokers, 8.);
   drop female_smokers;
   rename female_smokers2=female_smokers;
   male_smokers2= input(male_smokers, 8.);
   drop male_smokers;
   rename male_smokers2=male_smokers;
 run ;



* remplacer les données manquants avec 0;


 data OWID_COVID_DATA_2019;
   set OWID_COVID_DATA_2019;
   array change _numeric_;
        do over change;
            if change=. then change=0;
        end;
 run ;

 data OWID_COVID_DATA_2020;
   set OWID_COVID_DATA_2020;
   array change _numeric_;
        do over change;
            if change=. then change=0;
        end;
 run ;




 * union des deux table 2019 et 2020;
data Covid_Data;/*1*/
	set OWID_COVID_DATA_2019 OWID_COVID_DATA_2020;
run;

* joindre les tables;

proc sort data=Covid_Data ;by iso_code;run;
proc sort data=ISO_CODE_TABLE ;by iso_code;run;

data Covid_Data;/*1*/
	merge Covid_Data ISO_CODE_TABLE ;
	by iso_code;
run;


* renomer les colonne;

data Covid_Data;
set Covid_Data 
				(keep= iso_code 
				location 
				continent 
				date 
				new_cases 
				new_deaths 
				new_tests 
				total_tests
				population
				female_smokers
				male_smokers);
	rename location=pays;
	rename new_cases=nouveaux_cas;
	rename new_deaths=nouveaux_deces;
	rename new_tests=nouveaux_tests;
	rename total_tests=nb_total_tests;
	rename female_smokers=femmes_fumeuses;
	rename male_smokers=hommes_fumeurs;
where continent <>" ";
run;


**********************************************************************************;
* 							Question 3											  ;
**********************************************************************************;
*---------------------------------------------------------------------------------;
* 3_1;
*---------------------------------------------------------------------------------;

data Covid_Data;
set Covid_Data;
Mois= month(date);
Trimestre =QTR(date);
run;

*---------------------------------------------------------------------------------;
* 3_2 ;
*---------------------------------------------------------------------------------;

data Covid_Data;
set Covid_Data;
test_par_population= nb_total_tests/population;
run;

*---------------------------------------------------------------------------------;
* 3_3;
*---------------------------------------------------------------------------------; 

proc sort data=Covid_Data;by date; run;
proc means data=Covid_Data mean;
by date;
var nouveaux_cas;
OUTPUT out=lib.Moy_Glob ;
run;

data lib.Moy_Glob (keep= date Moyen_affecte_par_date); set lib.Moy_Glob ;
rename nouveaux_cas= Moyen_affecte_par_date;
where _STAT_ eq "MEAN";
run;

proc sort data=lib.Moy_Glob  ;by date;run;
proc sort data=Covid_Data ;by date;run;

data Covid_Data;/*1*/
	merge Covid_Data lib.Moy_Glob ;
	by date;
run;


data Covid_Data;
set Covid_Data;

if nouveaux_cas>Moyen_affecte_par_date then comparaison_moyenne_globale=1;
else comparaison_moyenne_globale=0;

run;


**********************************************************************************;
* 							Question 4											  ;
**********************************************************************************;

*---------------------------------------------------------------------------------;
* 4_1;
*---------------------------------------------------------------------------------; 
proc sort data=Covid_Data ;by  date continent;run;

proc means data=Covid_Data; 
by  date continent ; var nouveaux_cas nouveaux_deces; output out=lib.Q_4_1;
run;


data Q_4_1_min ( drop= _freq_ _stat_ _type_ )  ; set lib.Q_4_1  ;
rename nouveaux_cas = nb_min_affecte;
rename nouveaux_deces = nb_min_deces;
where _stat_ = "MIN";
run;

data Q_4_1_max ( drop= _freq_ _stat_ _type_ )  ; set lib.Q_4_1  ;
rename nouveaux_cas = nb_max_affecte;
rename nouveaux_deces = nb_max_deces;
where _stat_ = "MAX";
run;


data Q_4_1_mean ( drop= _freq_ _stat_ _type_ )  ; set lib.Q_4_1  ;
rename nouveaux_cas = nb_moy_affecte;
rename nouveaux_deces = nb_moy_deces;
where _stat_ = "MEAN";
run;

data Q_4_1_total; 
merge Q_4_1_min Q_4_1_max Q_4_1_mean;
by date continent;
run;

proc sort data= Q_4_1_total; by descending date descending nb_max_deces ;run;

*---------------------------------------------------------------------------------;
* 4_2;
*---------------------------------------------------------------------------------; 

proc sort data= Covid_Data; by pays ; run;

proc means data=Covid_Data; by pays ; var nouveaux_deces; where nouveaux_deces>0;output out= Q4_2;run;


data Q_4_2( drop= _freq_ _stat_ _type_ )  ; set Q4_2 ;
rename nouveaux_deces=nb_jours;
where _stat_ = "N";
run;
**********************************************************************************;
* 							Question 5											  ;
**********************************************************************************;
*---------------------------------------------------------------------------------;
* 5_1;
*---------------------------------------------------------------------------------; 

%MACRO Q_4_3(Mois,Pays);

	proc univariate data=Covid_Data; 
	var nouveaux_cas; 
	where Mois=&Mois. and pays=&pays.;
	run;


%MEND;	

%q_4_3(6,"Canada");

*---------------------------------------------------------------------------------;
* 5_2;
*---------------------------------------------------------------------------------; 
%let Path2=D:\Bureau\HEC\CERT\LOG SATISTIQUES_AUTOMNE_2020\new;
libname lib2 "&Path2.";

%MACRO Q44;

	%do i=1 %to 4;
		data lib2.Covid_Data_&i; set Covid_Data;
		where Trimestre=&i;
		run;

		proc export data=lib2.Covid_Data_&i  dbms=xlsx
		outfile="&Path2.\Covid_Data_&i..csv" dbms=csv
		replace;

		
	%END;

%MEND;

%Q44;


*---------------------------------------------------------------------------------;
* 5_3;
*---------------------------------------------------------------------------------;


PROC DATASETS lib=lib2;
DELETE _all_;
RUN;

	

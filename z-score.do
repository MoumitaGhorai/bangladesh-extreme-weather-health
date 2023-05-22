adopath + "C:\Users\moumi\Desktop\0 Bangladesh project\WHO\igrowup_update-master\"

/* We use 'reflib' to specify the package directory where the .dta files 
containing the WHO Child Growth Standards are stored. Note that we use 
strX to specify the length of the path in string. If the path is long, 
you may specify str55 or more, so it will run. */	
gen str100 reflib="C:\Users\moumi\Desktop\0 Bangladesh project\WHO\igrowup_update-master\"
lab var reflib "Directory of reference tables"


/* We use datalib to specify the working directory where the input STATA 
dataset containing the anthropometric measurement is stored. */
gen str100 datalib = "C:\Users\moumi\Desktop\0 Bangladesh project\" 
lab var datalib "Directory for datafiles"


/* We use datalab to specify the name that will prefix the output files that 
will be produced from using this ado file (datalab_z_r_rc and datalab_prev_rc)*/
gen str30 datalab = "children_nutri_bgd" 
lab var datalab "Working file"


*** Next check the variables that WHO ado needs to calculate the z-scores:
*** sex, age, weight, height

*** Variable: SEX ***
tab b4, miss 
	//"1" for male ;"2" for female
tab  b4, nol 
clonevar gender = b4
desc gender
tab gender


*** Variable: AGE ***
tab hw1, miss 
codebook hw1
	//Age is measured in months
clonevar age_months = hw1  
desc age_months
summ age_months
gen  str6 ageunit = "months" 
lab var ageunit "Months"
// gen mdate = mdy(hc18, hc17, hc19)
// gen bdate = mdy(hc30, hc16, hc31) if hc16 <= 31
// 	//Calculate birth date in days from date of interview
// replace bdate = mdy(hc30, 15, hc31) if hc16 > 31 
// 	//If date of birth of child has been expressed as more than 31, we use 15
// gen age = (mdate-bdate)/30.4375
// 	//Calculate age in months with days expressed as decimals
// replace age = 0 if age<0
//
// *gen age2=hc1a/30.4375
// *compare age age2
// *drop age2
gen age=age_months

	
*** Variable: BODY WEIGHT (KILOGRAMS) ***
codebook hw2, tab (10000)
gen	weight2 = hw2/10 
	//We divide it by 10 in order to express it in kilograms 
tab hw2 if hw2>9990,m nol   
	//Missing values are 994 to 996
replace weight2 = . if hw2>=9990 
	//All missing values or out of range are replaced as "."
// tab	hc13 hc2 if hc2>=9990 | hc2==., miss 
// 	//hc13: result of the measurement
// desc weight 
// summ weight


*** Variable: HEIGHT (CENTIMETERS)
codebook hw3, tab (10000)
gen	height2 = hw3/10 
	//We divide it by 10 in order to express it in centimeters
tab hw3 if hw3>9990,m nol   
	//Missing values are 9994 to 9996
replace height2 = . if hw3>=9990 
// 	//All missing values or out of range are replaced as "."
// tab	hc13 hc3   if hc3>=9990 | hc3==., miss
// desc height 
// summ height


*** Variable: MEASURED STANDING/LYING DOWN ***
codebook hw15
gen measure = "l" if hw15==1 
	//Child measured lying down
replace measure = "h" if hw15==2 
	//Child measured standing up
replace measure = " " if hw15==9 | hw15==0 | hw15==. 
	//Replace with " " if unknown
desc measure
tab measure

	
*** Variable: OEDEMA ***
lookfor oedema
gen str1 oedema = "n"  
	//It assumes no-one has oedema
desc oedema
tab oedema	


*** Variable: INDIVIDUAL CHILD SAMPLING WEIGHT ***
gen  sw = v005/1000000 
	//For DHS sample weight has to be divided 1000000
desc sw
summ sw


/*We now run the command to calculate the z-scores with the adofile */
igrowup_restricted reflib datalib datalab gender age ageunit weight height ///
measure oedema sw


/*We now turn to using the dta file that was created and that contains 
the calculated z-scores to create the child nutrition variables following WHO 
standards */
use "C:\Users\moumi\Desktop\0 Bangladesh project\children_nutri_bgd_z_rc.dta", clear 



*** Standard MPI indicator ***
	//Takes value 1 if the child is under 2 stdev below the median & 0 otherwise
	
gen	underweight = (_zwei < -2.0) 
replace underweight = . if _zwei == . | _fwei==1
lab var underweight  "Child is undernourished (weight-for-age) 2sd - WHO"
tab underweight, miss


gen stunting = (_zlen < -2.0)
replace stunting = . if _zlen == . | _flen==1
lab var stunting "Child is stunted (length/height-for-age) 2sd - WHO"
tab stunting, miss


gen wasting = (_zwfl < - 2.0)
replace wasting = . if _zwfl == . | _fwfl == 1
lab var wasting  "Child is wasted (weight-for-length/height) 2sd - WHO"
tab wasting, miss


	//Retain relevant variables:
keep ind_id child_KR underweight stunting wasting 

order ind_id child_KR underweight stunting wasting 

sort ind_id

duplicates report ind_id
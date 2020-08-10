*:::::::::::::::::::::::::::::::::::::::::::::::::::::::
* Spreading the word!
* Initial analysis of data and adjusment
* Author: Jose David Lopez-Rivas
* Last modification: 23/07/2020 
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::
clear all
cd "~/Documents/Research/Spreading the word/Project/Results"
import excel "database_complete.xlsx", firstrow

*** Village identificator (villid)
egen vill = group(village), label
drop village
rename vill village
label define villages 1 "Mo 75" 2 "Bi 50" 3 "Bi 75" 4 "Bi 0" 5 "Mo 0" 6 "Bi 25" 7 "Mo 50" 8 "Mo 25"
label values village villages
label var village "Village"

*** Households Identificator (hhid)
gen hh=_n 
label var hh "Household"

*** Street Identificator
egen street=group(street_port)
label var street "Street Identificator"

*** Street and village identificator 
egen street_village= group(street village)
label var street_village "Street and village"

*** SES and village identificator
rename strata ses
label var ses "Socioeconomic strata"
sort ses village
egen ses_village = group(ses village)
label var ses_village "ses and village"


*** Variables labels
label var bill "Billing frequency"
label define bill 0 "Monthly" 1 "Bimonthly"
label values bill bill

rename treated d_treated
label var d_treated "Direclty"
label define d_treated 0 "Not directly" 1 "Direclty"
label values d_treated d_treated

rename spill i_treated
label var i_treated "Spillovers"
label define i_treated 0 "Not spillover" 1 "Spillover"
label values i_treated i_treated 

label var control "Control"

label var status "Treatment status"
label define status 1 "Control" 2 "Spillovers" 3 "Directly"
label values status status

*************************************************
** Setting the panel data 
*************************************************

reshape long cons above norm, i(hh) j(m) string
gen year = substr(m,1,4)
gen month = substr(m,5,6)
destring year, replace
destring month, replace
drop m

gen period=ym(year, month)
format period %tm

label var cons "Water consuption (m3)"
label var period "Month"
label var above "Above social norm"
label var norm "Social norm standard"

sort hh period
xtset hh period

** Time variables and post treatment (post)
bys hh: generate time=_n
label var time "Time" // Time indicator

gen time_ad = time-20
label var time_ad "Time" // Time normalized

gen post=1 if time>20 // After August
replace post=0 if post==.
label var post "Post"
label define post 0 "Before" 1 "After"
label value post post

replace post=. if time>25 & bill==0 // After january for the monthyl group

** Interaction of treated and post variable
gen d_post= d_treated*post 
label var d_post "Directly x Post"
gen i_post= i_treated*post
label var i_post "Indirectly x Post"


** HH Consumption in liters per day
replace cons=. if cons==0
gen cons_daily= (cons/30)*1000 if bill==0
replace cons_daily=(cons/60)*1000 if bill==1
label var cons_daily "Liters per day"

** Social Norm standard variable (Average consumption per ses, village and time)
replace norm=. if norm==0

bys ses village period: egen norm_cf = mean(cons) // Generating the coounterfactual of the norm for pretreatment

replace norm_cf=norm if time==20
replace norm_cf=norm if time==21
replace norm_cf=norm if time==22
replace norm_cf=norm if time==23
replace norm_cf=norm if time==24

gen norm_daily_cf =(norm_cf/30)*1000 if bill==0
replace norm_daily_cf=(norm_cf/60)*1000 if bill==1
label var norm_daily_cf "Social norm (L/day)"

** Distance to social norm standard (Difference between consumption minus sns)
sort hh period
gen dist_norm = cons_daily-norm_daily_cf
label var dist_norm "Distance to sns (L/day)"

** positive feedback (1 if hhs consumes below or equal to the sns in the social comparison)
gen positive=1 if (cons_daily-norm_daily_cf<=0)
replace positive=0  if (cons_daily-norm_daily_cf>0)
label var positive "Positive feedback"
replace positive=. if dist_norm==.

** Negative feedback (1 if hhs consumes above the sns in the social comparison)
gen negative=1 if (cons_daily-norm_daily_cf>0) 
replace negative=0  if (cons_daily-norm_daily_cf<=0)
label var negative "Negative feedback"
replace negative=. if dist_norm==.


*************************************************
** Standardized water consumption by status
*************************************************

bys period status: egen cons_avg=mean(cons_daily) // Average per period and status
bys period status: egen cons_sd=sd(cons_daily) // Std. deviations per period and status
gen cons_std=(cons_daily-cons_avg)/cons_sd // WC standardized
label var cons_std "Water consumption (Std.)"

** Average water consumption before and after by status
bys hh: egen cons_bt=mean(cons_daily) if time<=20 // Consumption before treatment
label var cons_bt "Water consumption Before" 
bys hh: egen cons_at=mean(cons_daily) if time>20 // Consumption after treatment
label var cons_at "Water consumption After"
bys hh: egen prom_cons_bt = mean(cons_bt)  // Average Consumption before treatment
label var prom_cons_bt "Avg. water consumption Before"
bys hh: egen prom_cons_at = mean(cons_at) // Average Consumption after treatment
label var prom_cons_at "Avg. water consumption After"
bys hh: gen ch_cons =  prom_cons_at-prom_cons_bt // Cange in average water consumptio
label var ch_cons "Change in water consumption" 

*************************************************
** Standardized water consumption by village
*************************************************
bys period village: egen avg_cons_vill=mean(cons_daily) // Average per period and village
bys period village: egen sd_cons_vill=sd(cons_daily) // Std. deviations per period and village
gen cons_std_vill=(avg_cons_vill-sd_cons_vill)/sd_cons_vill //  standardized consumption
label var cons_std_vill "Water consumption (Std.Village)"


*************************************************
**  Saturation per village ID
*************************************************
gen direct_sat = 0
replace direct_sat = 1 if village==8 & d_treated==1 & post==1 // 25%
replace direct_sat = 1 if village==6 & d_treated==1 & post==1 // 25%
replace direct_sat = 2 if village==7 & d_treated==1 & post==1 // 50%
replace direct_sat = 2 if village==2 & d_treated==1 & post==1 // 50%
replace direct_sat = 3 if village==3 & d_treated==1 & post==1 // 75%
replace direct_sat = 3  if village==1 & d_treated==1 & post==1 // 75%
label var direct_sat "Treated by Saturation"
label define sat 1 "25\%" 2 "50\%" 3 "75\%"
label values direct_sat sat

tab direct_sat, gen(d_sat)


gen indirect_sat = 0
replace indirect_sat = 1 if village== 8 & i_treated== 1 & post==1 // 25%
replace indirect_sat = 1 if village== 6 & i_treated== 1 & post==1 // 25%
replace indirect_sat = 2 if village== 7 & i_treated== 1 & post==1 // 50%
replace indirect_sat = 2 if village== 2 & i_treated== 1 & post==1 // 50%
replace indirect_sat = 3 if village== 3 & i_treated== 1 & post==1 // 75%
replace indirect_sat = 3 if village== 1 & i_treated== 1 & post==1 // 75%
label var indirect_sat "Spillover by Saturation"
label values indirect_sat sat

tab indirect_sat, gen(i_sat)

gen saturation = 0
replace saturation = 0.25 if village==8 
replace saturation = 0.25 if village==6 
replace saturation = 0.5 if village==7 
replace saturation = 0.5 if village==2 
replace saturation = 0.75 if village==3 
replace saturation = 0.75  if village==1
label var saturation "Level of Saturation"

*************************************************
** Fixed Effects combinations with time
*************************************************
egen time_village = group(time village) // Time & village
egen time_street = group(time street) // Time & Street
egen time_ses = group(time ses) // Time & SES

egen month_year = group(month year) // month & year

egen month_ses = group(month ses)
egen year_ses = group(month ses)
*************************************************
** Consumption Outliers
*************************************************
bys village time: egen p75=  pctile(cons_daily), p(75)
bys village time: egen p25=  pctile(cons_daily), p(25)
gen ric = p75 - p25
gen riul =  p75+(1.5*ric)
gen outlier = 1 if cons_daily > riul
replace outlier=. if cons_daily==.
replace outlier=0 if outlier==.


*************************************************
** Adjunting Consumption
*************************************************

// The water consumption is normalized by dividing it into the average post-treatment control group consumption and multiplying by 100

egen cons_cont=mean(cons_daily), by(post control)
replace cons_cont=. if control==0
replace cons_cont=. if post==0
replace cons_cont=. if post==.

sort hh time
gen cons_control = cons_cont
replace cons_control = cons_cont[21] if missing(cons_cont)
drop cons_cont

gen cons_adj = (cons_daily/cons_control)*100

label var cons_adj "Normalized consumption"

*************************************************
** Saving data frame in txt file
*************************************************
drop id street_port  norm_cf norm cons

save "~/Documents/GitHub/spreading_the_word/data_adjusted.dta", replace

export delimited using "~/Documents/GitHub/spreading_the_word/data_adjusted.csv",nolabel replace

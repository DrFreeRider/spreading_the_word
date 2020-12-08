*:::::::::::::::::::::::::::::::::::::::::::::::::::::::
* Spreading the word!
* Initial analysis of data and adjusment
* Author: Jose David Lopez-Rivas
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::
clear all
cd "~/Documents/Research/Spreading the word/Results"
import excel "database_complete.xlsx", firstrow

********************************************************
*** Households identificator (HH)
gen hh=_n 
label var hh "Household"

*** Village identificator
egen vill = group(village), label
drop village
rename vill village

*** Street Identificator
egen street=group(street_port)
label var street "Street"

*** Street & village identificator 
egen street_village= group(street village)
label var street_village "Street & Village"

*** SES & village identificator
rename strata ses
label var ses "Socioeconomic strata"
sort ses village
egen ses_village = group(ses village)
label var ses_village "SES & Village"

*** Variables labels
label var bill "Frequency"
label define bill 0 "30 days" 1 "60 days"
label values bill bill

rename treated d_treated
label var d_treated "Directly treated"
label define d_treated 0 "Not direct" 1 "Direct"
label values d_treated d_treated

rename spill i_treated
label var i_treated "Spillovers"
label define i_treated 0 "Not spillover" 1 "Spillover"
label values i_treated i_treated 

label var control "Control"

label var status "Treatment status"
label define status 1 "Control" 2 "Spillover" 3 "Directly"
label values status status

label define village 1 "A" 2 "B" 3 "C" 4 "D" 5 "E" 6 "F" 7 "G" 8 "H"
label values village village
label var village "Village"

label define saturation_bill 1 "30(75%)" 2 "60(50%)" 3 "60(75%)" 4 "60(0%)" 5 "30(0%)" 6 "60(25%)" 7 "30(50%)" 8 "30(25%)"


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

label var cons "Consumption (m3)"
label var period "Month"
label var above "Above social norm"
label var norm "Social norm standard"

sort hh period
egen h_v= group(village hh)
xtset hh period

*################################################
* Time and post treatment variables
bys hh: generate time=_n
label var time "Time" // Time indicator

// Time normalized where 0 represents September 2017. 
gen time_ad = time-21
label var time_ad "Time" 

// Treatment started in september 2019. The "30 days" group received 5 deliveries every month (until january 2018). The "60 days" group received 3 message deliveries (until january-febreuary 2018).

gen post=(time>20)
label var post "Post"
label define post 0 "Before" 1 "After"

label value post post
//replace post=. if time>25 & bill==0 // Removing treatment last period for the "30 days"

** Interaction for the treatment status and post variable. 
gen d_post= d_treated*post 
label var d_post "Direct"
gen i_post= i_treated*post
label var i_post "Spillover"

*################################################
* Consumption Variable

** Consumption in liters per day (First, I treat the zero consumption as missing. Zero consumtpion represents an epty household)

replace cons=. if cons==0 // Replacing 
gen cons_daily= (cons/30)*1000 if bill==0
replace cons_daily=(cons/60)*1000 if bill==1
label var cons_daily "Liters per day"

*################################################
** Social Norm standard variable
replace norm=. if norm==0 
gen norm_daily = (norm/30)*1000 if bill==0
replace norm_daily =(norm/60)*1000 if bill==1

label var norm_daily "Social norm"

** Generating a counterfactual of the social norm (at pre-treatment and for the control group). Average consumption by SES, village and time.
bys ses village time: egen norm_cf = mean(cons)
label var norm_cf "Social norm (counterfactual)"
replace norm_cf=norm if time==20
replace norm_cf=norm if time==21
replace norm_cf=norm if time==22
replace norm_cf=norm if time==23
replace norm_cf=norm if time==24

gen norm_daily_cf =(norm_cf/30)*1000 if bill==0
replace norm_daily_cf=(norm_cf/60)*1000 if bill==1
label var norm_daily_cf "Social norm (counterfactual)"

** Distance to social norm (Difference between consumption minus SNS)
sort hh time
gen dist_norm = (cons_daily - norm_daily)
label var dist_norm "Difference (L/day)"

** Approval feedback (1 if hhs consumption is below or equal to the SN)

gen  positive=(cons_daily-norm_daily<=0)
label var positive "Approval feedback"
replace positive=. if dist_norm==.

** Disapproval feedback (1 if hhs consumes above the sns in the social comparison)

gen negative=(cons_daily-norm_daily>0) 
label var negative "Disapproval feedback"
replace negative=. if dist_norm==.


*************************************************
** Change consumption before and after by status
*************************************************

** Average water consumption before and after by status
sort hh period
bys hh: egen prom_cons_bt=mean(cons_daily) if time<=20 // Consumption before treatment
bys hh: egen prom_cons_at=mean(cons_daily) if time>20 // Consumption after treatment

bys hh: egen cons_bt = mean(prom_cons_bt)  // Average Consumption before treatment
label var cons_bt "Consumption Before" 
bys hh: egen cons_at = mean(prom_cons_at) // Average Consumption after treatment
label var cons_at "Consumption After"

bys hh: gen ch_cons =  cons_at-cons_bt // Average change in  consumption.
label var ch_cons "Change consumption" 

drop prom_cons_bt prom_cons_at

*************************************************
** Normalized Consumption
*************************************************
** The water consumption is normalized by dividing it into the average post-treatment control group consumption and multiplying by 100.
egen cons_cont=mean(cons_daily), by(post status)
replace cons_cont=. if post==1
replace cons_cont=. if control==0
replace cons_cont=. if post==.
replace cons_cont = cons_cont[2] if missing(cons_cont)
label var cons_cont "Average control at baseline"

sort hh time
gen cons_adj = (cons_daily/cons_cont)*100
label var cons_adj "Normalized consumption"


*************************************************
**  Saturation Variables
*************************************************
gen saturation = 1
replace saturation = 2 if village==8 
replace saturation = 2 if village==6 
replace saturation = 3 if village==7 
replace saturation = 3 if village==2 
replace saturation = 4 if village==3 
replace saturation = 4  if village==1
label var saturation "Level of Saturation"

** Variables of treatment x saturation x post
gen d_sat1=0 
replace d_sat1=1 if saturation==2 & d_post==1
replace d_sat1=. if post==.

gen d_sat2=0 
replace d_sat2=1 if saturation==3 & d_post==1
replace d_sat2=. if post==.

gen d_sat3=0 
replace d_sat3=1 if saturation==4 & d_post==1
replace d_sat3=. if post==.

gen i_sat1=0 
replace i_sat1=1 if saturation==2 & i_post==1
replace i_sat1=. if post==.

gen i_sat2=0 
replace i_sat2=1 if saturation==3 & i_post==1
replace i_sat2=. if post==.

gen i_sat3=0 
replace i_sat3=1 if saturation==4 & i_post==1
replace i_sat3=. if post==.


** Billing frequency indicators
gen post_bill = 0
replace post_bill = 1 if post == 1 & bill == 1
label var post_bill "Post X Billing"
gen d_post_bill1 = 0
replace d_post_bill1 = 1 if d_post == 1 & bill == 0
label var d_post_bill1 "Direct X Monthly"
gen d_post_bill2 = 0
replace d_post_bill2 = 1 if d_post == 1 & bill == 1
label var d_post_bill2 "Direct X Bimonthly"
gen i_post_bill1=0
replace i_post_bill1=1 if i_post==1 & bill==0
label var i_post_bill1 "Spillover X  Monthly"
gen i_post_bill2=0
replace i_post_bill2=1 if i_post==1 & bill==1
label var i_post_bill2 "Spillover X Bimonthly"

** Billing frequency and saturation indicators
gen direct_satbill = 0
replace direct_satbill = 1 if saturation==2 & d_treated==1 & post==1 & bill==0 // 25%
replace direct_satbill = 2 if saturation==3 & d_treated==1 & post==1 & bill==0 // 50%
replace direct_satbill = 3  if saturation==4 & d_treated==1 & post==1 & bill==0 // 75%

replace direct_satbill = 4 if saturation==2 & d_treated==1 & post==1 & bill==1 // 25%
replace direct_satbill = 5 if saturation==3 & d_treated==1 & post==1 & bill==1 // 50%
replace direct_satbill = 6 if saturation==4 & d_treated==1 & post==1 & bill==1 // 75%
label var direct_satbill "Treated by Saturation"
tab direct_satbill, gen(d_sat_bill)


gen indirect_satbill = 0
replace indirect_satbill = 1 if saturation== 2 & i_treated== 1 & post==1 & bill==0 // 25%
replace indirect_satbill = 2 if saturation== 3 & i_treated== 1 & post==1 & bill==0 // 50%
replace indirect_satbill = 3 if saturation== 4 & i_treated== 1 & post==1 & bill==0 // 75%
replace indirect_satbill = 4 if saturation== 2 & i_treated== 1 & post==1 & bill==1 // 25%
replace indirect_satbill = 5 if saturation== 3 & i_treated== 1 & post==1 & bill==1 // 50%
replace indirect_satbill = 6 if saturation== 4 & i_treated== 1 & post==1 & bill==1 // 75%
label var indirect_satbill "Spillover by Saturation"
tab indirect_satbill, gen(i_sat_bill)

*************************************************
** Collapsing two-months variables
*************************************************
gen time_bi=0
forval j = 1(1)13{
	replace time_bi= `j' if  time <=(`j'*2) & time > (`j'*2)-2
}
label var time_bi "Time"

** Aggregate consumption (two months)
bys hh time_bi: egen cons_bi = sum(cons)
replace cons_bi=. if cons_bi==0

** Daily consumption (two months)
gen cons_daily_bi=(cons_bi/60)*1000 
replace cons_daily_bi=. if cons_daily_bi==0

** Normalized consumption (two months)
//bys hh time_bi: egen cons_adj_bi = sum(cons_adj)
//replace cons_adj_bi=. if cons_adj_bi==0

egen cons_cont_bin=mean(cons_daily_bi), by(post status)
replace cons_cont_bin=. if post==1
replace cons_cont_bin=. if control==0
replace cons_cont_bin=. if post==.
replace cons_cont_bin = cons_cont_bin[2] if missing(cons_cont_bin)
label var cons_cont "Average control at baseline"

sort hh time
gen cons_adj_bi = (cons_daily_bi/cons_cont_bin)*100
label var cons_adj_bi "Normalized consumption"



** Aggregate Social Norm (two months)
bys hh time_bi: egen norm_bi = sum(norm)
replace norm_bi=. if norm_bi==0

** identificator of two-months duplicates
quietly by hh time_bi:  gen dup_bi = cond(_N==1,0,_n)
//drop if dup==2

*************************************************
** Fixed Effects combinations with time
*************************************************
egen time_village = group(time village) // Time & village
egen time_street = group(time street) // Time & Street
egen time_ses = group(time ses) // Time & SES

egen time_ses_village = group(time ses village) // Time & SES & Village


*************************************************
** Probability weights
*************************************************

*SES distribution by village
bys ses village: egen ses_n1 = count(ses)
gen ses_n=ses_n1/26
drop ses_n1
bys village: egen vill_n1= count(village)
gen vill_n=vill_n1/26
drop vill_n1
gen ses_p=ses_n/vill_n

*************************************************
** Saving data frame in txt file
*************************************************
drop id street_port norm_cf

save "~/Documents/GitHub/spreading_the_word/data_adjusted.dta", replace

export delimited using "~/Documents/GitHub/spreading_the_word/data_adjusted.csv",nolabel replace

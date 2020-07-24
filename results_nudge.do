*:::::::::::::::::::::::::::::::::::::::::::::::::::::::
* Spreading the word!
* Initial analysis of data and adjusment
* Author: Jose David Lopez-Rivas
* Last modification: 23/07/2020 
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::

clear all
cd "/Users/mrfreerider/Documents/Research/Social Nudge/Project/Results"
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


******************************************************
** Socioeconomic strata distribution (SES)
******************************************************
tabout ses village using "/Users/mrfreerider/Documents/Research/Social Nudge/Project/Results/tabs and graphs/strata_dist.tex", cells(col) format(1) clab(%) ///
replace style(tex) botstr(Local water managers 2017)

******************************************************
** Differences in mean between HHS 
******************************************************

//Directly vs indirectly 
eststo clear
estpost ttest cons201701 cons201702 cons201703 cons201704 cons201705 ///
 cons201706 cons201707 ses if control==0, by(d_treated) unequal
esttab using "/Users/mrfreerider/Documents/Research/Social Nudge/Project/Results/tabs and graphs/diff_cons_hhs_dvsi.tex", replace label booktabs nomtitle nonumbers ///
 cell("mu_1(label(Mean Directly) fmt(a1)) mu_2(label(Mean Indirectly) fmt(a1)) se(label(SD) fmt(a1)) b(label(Diff.) star fmt(a2))  t(label(t-stat) fmt(a2))") ///
 coeflabel( cons201701 "Jan 2017" cons201701 "Feb 2017" cons201703 "Mar 2017" cons201704 "Apr 2017" cons201705 "May 2017" cons201706 "Jun 2017" cons201707 "Jul 2017" ses "Socioeconomic Strata")

// Treated (anyway) versus control group
eststo clear
estpost ttest cons201701 cons201702 cons201703 cons201704 cons201705 ///
cons201706 cons201707 ses, by(control) unequal
esttab using "/Users/mrfreerider/Documents/Research/Social Nudge/Project/Results/tabs and graphs/diff_cons_hhs__tvsc.tex",  replace label booktabs nomtitle nonumbers  ///
cell("mu_1(label(Mean Treated) fmt(a1)) mu_2(label(Mean  Control) fmt(a1)) se(label(SD) fmt(a1)) b(label(Diff.) star fmt(a2))  t(label(t-stat) fmt(a2))") ///
 coeflabel( cons201701 "Jan 2017" cons201701 "Feb 2017" cons201703 "Mar 2017" cons201704 "Apr 2017" cons201705 "May 2017" cons201706 "Jun 2017" cons201707 "Jul 2017" ses "Socioeconomic Strata")


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

** Compliance to norm (1 if hh reduces contemporary consumption respect to the sns from the previous period)

sort  hh period
gen comply=1 if (cons_daily-L.norm_daily_cf<=0) 
replace comply=0 if (cons_daily-L.norm_daily_cf>0) 
replace comply=. if dist_norm==.
label var comply "Compliance with previous sns"

gen rate_comply  = (cons_daily-L.norm_daily_cf) / cons_daily



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
replace direct_sat = 1 if village==8 & d_treated==1 // 25%
replace direct_sat = 1 if village==6 & d_treated==1 // 25%
replace direct_sat = 2 if village==7 & d_treated==1 // 50%
replace direct_sat = 2 if village==2 & d_treated==1 // 50%
replace direct_sat = 3 if village==3 & d_treated==1 // 75%
replace direct_sat = 3  if village==1 & d_treated==1 // 75%
label var direct_sat "Treated by Saturation"
label define sat 1 "25\%" 2 "50\%" 3 "75\%"
label values direct_sat sat

gen indirect_sat = 0
replace indirect_sat = 1 if village== 8 & i_treated== 1 // 25%
replace indirect_sat = 1 if village== 6 & i_treated== 1 // 25%
replace indirect_sat = 2 if village== 7 & i_treated== 1 // 50%
replace indirect_sat = 2 if village== 2 & i_treated== 1 // 50%
replace indirect_sat = 3 if village== 3 & i_treated== 1 // 75%
replace indirect_sat = 3 if village== 1 & i_treated== 1 // 75%
label var indirect_sat "Spillover by Saturation"
label values indirect_sat sat

gen saturation = 0
replace saturation = 0.25 if villageid==8 
replace saturation = 0.25 if villageid==6 
replace saturation = 0.5 if villageid==7 
replace saturation = 0.5 if villageid==2 
replace saturation = 0.75 if villageid==3 
replace saturation = 0.75  if villageid==1
label var saturation "Level of Saturation"

*************************************************
** Fixed Effects combinations with time
*************************************************
egen time_village = group(time village) // Time & village
egen time_street = group(time street) // Time & Street


*************************************************
*************************************************
** Saving data frame in txt file

save "/Users/mrfreerider/Documents/Research/Social Nudge/Paper/data_exp_merged.dta", replace
export delimited using "/Users/mrfreerider/Documents/Research/Social Nudge/Paper/data_exp_merged.csv",nolabel replace

**********************************************
**********************************************
cd "/Users/mrfreerider/Documents/Research/Social Nudge/Project/Results/tabs and graphs"

*******************************************
** Preliminar Figures 
*******************************************

*** Preliminar Graphs (Hist, consumption)
do "preliminar_graphs.do"

*******************************************
** Parallel trends checking 
*******************************************
* Checking the prallel trends assumption between groups
do "parallel trends checking.do"


*******************************************
** Sum stat by status only and by billing frequency 
*******************************************
*** Summary statistics between tratment status and by billing groups
//do "summary_stat.do"


*******************************************
** R E G R E S S I O N S  
*******************************************

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Regressions pooled and sloped model
do "reg_aggregate_slope.do" // This includes regressions of both models and plot the coefficients.


*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Regressions pooled and sloped model by BILLING

do "reg_bill_freq.do"

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Regressions pooled and sloped model over time

do "reg_evol_model.do"

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Regressions pooled and sloped model by
** type of consumer

do "reg_percentile.do"

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Regressions pooled and sloped spatial proximity
do "reg_mechanism_sn.do"

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Regressions pooled and sloped by SES
do "reg_het_ses.do" 




** Neighbors with Positive Feedback
gen positive_d=positive 
replace positive_d=0 if d_treated!=1
bys street_num villageid period: gen positive_sum=sum(positive_d)
bys i period: gen positive_neigh=positive_sum-positive_d
gen perc_pos_neigh=positive_neigh/total_neigh
replace perc_pos_neigh=0 if perc_pos_neigh==.

gen negative_d=negative 
replace negative_d=0 if d_treated!=1
bys street_num villageid period: gen negative_sum=sum(negative_d)
bys i period: gen negative_neigh=negative_sum-negative_d
gen perc_neg_neigh=negative_neigh/total_neigh
replace perc_neg_neigh=0 if perc_neg_neigh==.


gen t_neighpos= d_treated*perc_pos_neigh
label var t_neighpos "Directly X \% Neighbors (+)"
replace t_neighpos=0 if t_neighpos==.
gen s_neighpos= i_treated*perc_pos_neigh
label var s_neighpos "Indirectly X \% Neighbors (+)"
replace s_neighpos=0 if s_neighpos==.

gen t_neighneg= d_treated*perc_neg_neigh
label var t_neighneg "Directly X \% Neighbors (-)"
replace t_neighneg=0 if t_neighneg==.
gen s_neighneg= i_treated*perc_neg_neigh
label var s_neighneg "Indirectly X \% Neighbors (-)"
replace s_neighneg=0 if s_neighneg==.


eststo clear
xtreg cons_daily  t_neighpos s_neighpos , fe vce(cluster ses_vill)
estadd local "Time" Y
eststo all_pos
xtreg cons_daily  t_neighneg s_neighneg  i.period, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo all_neg
xtreg cons_daily  t_neighpos s_neighpos  i.period if bill==0, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo m_pos
xtreg cons_daily  t_neighneg s_neighneg  i.period if bill==0, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo m_neg
xtreg cons_daily  t_neighpos s_neighpos  i.period if bill==1, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo b_pos
xtreg cons_daily  t_neighneg s_neighneg  i.period if bill==1, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo b_neg


esttab all_pos all_neg m_pos m_neg b_pos b_neg   using"iest_panel_feed_neigh.tex", replace booktabs keep(t_neighpos s_neighpos t_neighneg s_neighneg) stats(Time N_g N vce,labels("Time FE" "Households" "Observations") layout( @ @ @ @)) label star(* 0.10 ** 0.05 *** 0.01) compress gap mtitle("Positive" "Negative" "Positive" "Negative" "Positive" "Negative" ) ///
mgroups("All" "Monthly" "Bi-monthly" , pattern( 1 0  1 0 1 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))

*****
bys i villageid ses: egen cons_prom_before = mean(cons) if period<=692
by i:  replace cons_prom_before=cons_prom_before[_n-1] if cons_prom_before==.
egen cons_pctil=xtile(cons_prom_before), by(villageid) nq(2) 
gen cons_change=1 if ch_cons<=0
replace cons_change=2 if ch_cons>0
gen altruist=1 if cons_pctil==1 & cons_change==1
replace altruist=0 if altruist==.
label var altruist "Type: Altruist"
gen individualist=1 if cons_pctil==2 & cons_change==2
replace individualist=0 if individualist==.
label var individualist "Type: Individualist"
gen competitive=1 if cons_pctil==1 & cons_change==2
replace competitive=0 if competitive==.
label var competitive "Type: Competitive"
gen cond_coop=1 if cons_pctil==2 & cons_change==1
replace cond_coop=0 if cond_coop==.
label var cond_coop "Type: Cond. cooperator"



local type_ind altruist individualist competitive cond_coop
foreach x in `type_ind'{
bys street_num villageid period: gen `x'_sum=sum(`x')
bys i period: gen `x'_neigh=(`x'_sum - `x')
gen perc_`x'_neigh=`x'_neigh/total_neigh
replace perc_`x'_neigh=0 if total_neigh==0
label var perc_`x'_neigh "\% `x' neighbors"
}

local perc_type perc_altruist_neigh perc_individualist_neigh perc_competitive_neigh perc_cond_coop_neigh
foreach y in `perc_type'{
gen t_neigh`y'= treated*`y'
label var t_neigh`y' "Treated X \% neighbors"
replace t_neigh`y'=0 if t_neigh`y'==.
gen s_neigh`y'= spill*`y'
label var s_neigh`y' "Spill X \% neighbors"
replace s_neigh`y'=0 if s_neigh`y'==.
}


eststo clear
xtreg cons_daily post##c.t_neighperc_altruist_neigh post##c.s_neighperc_altruist_neigh  i.period, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo alt
xtreg cons_daily post##c.t_neighperc_individualist_neigh post##c.s_neighperc_individualist_neigh  i.period, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo ind

xtreg cons_daily post##c.t_neighperc_competitive_neigh post##c.s_neighperc_competitive_neigh  i.period, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo com

xtreg cons_daily post##c.t_neighperc_cond_coop_neigh post##c.s_neighperc_cond_coop_neigh  i.period, fe vce(cluster ses_vill)
estadd local "Time" Y
eststo coo

label var t_neighperc_altruist_neigh "Alt: treated X \% Neighbors Type"
label var t_neighperc_individualist_neigh "Ind: treated X \% Neighbors Type"
label var t_neighperc_competitive_neigh "Com: treated X \% Neighbors Type"
label var t_neighperc_cond_coop_neigh "Coo: treated X \% Neighbors Type"
label var s_neighperc_altruist_neigh "Alt: spill X \% Neighbors Type"
label var s_neighperc_individualist_neigh "Ind: spill X \% Neighbors Type"
label var s_neighperc_competitive_neigh "Com: spill X \% Neighbors Type"
label var s_neighperc_cond_coop_neigh "Coo: spill X \% Neighbors Type"


esttab alt ind com coo  using"iest_panel_type_neigh.tex", replace  booktabs keep(1.post#c.t_neighperc_altruist_neigh 1.post#c.s_neighperc_altruist_neigh 1.post#c.t_neighperc_individualist_neigh 1.post#c.s_neighperc_individualist_neigh 1.post#c.t_neighperc_competitive_neigh 1.post#c.s_neighperc_competitive_neigh 1.post#c.t_neighperc_cond_coop_neigh 1.post#c.s_neighperc_cond_coop_neigh ) mtitle("Altruist" "Individualist" "Competitive" "Conditional Cooperator") stats(Time N_g N vce,labels("Time FE" "Households" "Observations") layout( @ @ @ @)) mtitle label star(* 0.10 ** 0.05 *** 0.01) 
restore



**********************************************
gen post_post=1 if bill==0 & post==0 & year==2018
label var post_post "After intervention"
replace post_post=0 if post_post==.
xtreg cons_daily post_post##d_treated post_post##i_treated  i.month i.year $village_period, fe vce(cluster ses_vill)


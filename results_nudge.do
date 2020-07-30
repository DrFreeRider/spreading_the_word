*:::::::::::::::::::::::::::::::::::::::::::::::::::::::
* Spreading the word!
* Initial analysis of data and adjusment
* Author: Jose David Lopez-Rivas
* Last modification: 23/07/2020 
*:::::::::::::::::::::::::::::::::::::::::::::::::::::::


clear all
cd "~/Documents/GitHub/spreading_the_word"
use "data_adjusted.dta", clear 


******************************************************
** Summary statistics
******************************************************
tabout ses village using "~/Documents/GitHub/spreading_the_word/ses_distribution.tex", cells(col) format(1) clab(%) replace style(tex) botstr(Local water managers 2017)

******************************************************
** Differences in mean between HHS 
******************************************************

//Directly vs indirectly 

eststo clear
estpost ttest cons201701 cons201702 cons201703 cons201704 cons201705 ///
 cons201706 cons201707 ses if control==0, by(d_treated) unequal
esttab using "~/Documents/Research/Spreading the word/Project/Results/tabs and graphs/diff_cons_hhs_dvsi.tex", replace label booktabs nomtitle nonumbers ///
 cell("mu_1(label(Mean Directly) fmt(a1)) mu_2(label(Mean Indirectly) fmt(a1)) se(label(SD) fmt(a1)) b(label(Diff.) star fmt(a2))  t(label(t-stat) fmt(a2))") ///
 coeflabel( cons201701 "Jan 2017" cons201701 "Feb 2017" cons201703 "Mar 2017" cons201704 "Apr 2017" cons201705 "May 2017" cons201706 "Jun 2017" cons201707 "Jul 2017" ses "Socioeconomic Strata")

// Treated (anyway) versus control group
eststo clear
estpost ttest cons201701 cons201702 cons201703 cons201704 cons201705 ///
cons201706 cons201707 ses, by(control) unequal
esttab using "~/Documents/Research/Spreading the word/Project/Results/tabs and graphs/diff_cons_hhs__tvsc.tex",  replace label booktabs nomtitle nonumbers  ///
cell("mu_1(label(Mean Treated) fmt(a1)) mu_2(label(Mean  Control) fmt(a1)) se(label(SD) fmt(a1)) b(label(Diff.) star fmt(a2))  t(label(t-stat) fmt(a2))") ///
 coeflabel( cons201701 "Jan 2017" cons201701 "Feb 2017" cons201703 "Mar 2017" cons201704 "Apr 2017" cons201705 "May 2017" cons201706 "Jun 2017" cons201707 "Jul 2017" ses "Socioeconomic Strata")

 
**********************************************
**********************************************
cd "/Users/mrfreerider/Documents/Research/Spreading the word/Project/Results/tabs and graphs"

*******************************************
** Graphs 
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

do "summary_stat.do"


*******************************************
** Regressions 
*******************************************
** Pooled and sloped model
do "reg_aggregate_slope.do" 
// This includes regressions of both models and plot the coefficients.

** Pooled and sloped model by Billing Frequency
do "reg_bill_freq.do"

** Pooled and sloped model over time
do "reg_evol_model.do"

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


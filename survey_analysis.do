******************************************************
*** Paper: Spreading the word!
**  Survey analysis
*** Author: Jose David Lopez-Rivas  
******************************************************

cd "/Users/mrfreerider/Documents/Research/PEB drivers"

use data_survey_fixed.dta, clear

******************************************************
* Graphs
******************************************************
cd "/Users/mrfreerider/Dropbox/Aplicaciones/Overleaf/Spreading the word Paper"

* 1. Recalling messages by treatment status

preserve
collapse (mean) meanapp= image_1 (sd) sdapp=image_1 (count) napp=image_1  (mean) meandis= image_2 (sd) sddis=image_2 (count) ndis=image_2 , by(status)
generate hiapp = meanapp + invttail(napp-1,0.025)*(sdapp / sqrt(napp))
generate lowapp = meanapp - invttail(napp-1,0.025)*(sdapp / sqrt(napp))
generate hidis = meandis + invttail(ndis-1,0.025)*(sddis / sqrt(ndis))
generate lowdis = meandis - invttail(ndis-1,0.025)*(sddis / sqrt(ndis))

gen status_1 = status - 0.2
gen status_2 = status + 0.2

twoway (bar meanapp status_1 , color(navy%70) barwidth(0.4)) ///
(bar meandis status_2, color(maroon%70) barwidth(0.4) xlabel(1 "Control" 2"Indirectly" 3 "Directly")) ///
(rcap hiapp lowapp status_1, color(navy)) ///
(rcap hidis lowdis status_2, color(maroon)) ///
(scatter  meanapp status_1, msym(none) mlab(meanapp) mlabpos(6) mlabcolor(black) msize(*.5)) ///
(scatter  meandis status_2, msym(none) mlab(meandis) mlabpos(6) mlabcolor(black) msize(*.5)), ///
title(Recalling messagges, pos(11) size(4)) legend(order(1 "Approval" 2 "Disapproval") pos(11) ring(0) col(2))
graph export "/Users/mrfreerider/Dropbox/Aplicaciones/Overleaf/Spreading the word Paper/survey_recall_messages.pdf", replace
restore


* 2. Where did you hear about the messages (by treatment status)
preserve
collapse (mean) mean1= net_family (sd) sd1=net_family (count) n1=net_family (mean) mean2= net_neigh (sd) sd2=net_neigh (count) n2=net_neigh (mean) mean3= net_public (sd) sd3=net_public (count) n3=net_public  , by(status)
generate hi1= mean1 + invttail(n1-1,0.025)*(sd1 / sqrt(n1))
generate low1 = mean1 - invttail(n1-1,0.025)*(sd1 / sqrt(n1))
generate hi2 = mean2 + invttail(n2-1,0.025)*(sd2 / sqrt(n2))
generate low2 = mean2 - invttail(n2-1,0.025)*(sd2 / sqrt(n2))
generate hi3 = mean3 + invttail(n3-1,0.025)*(sd3 / sqrt(n3))
generate low3 = mean3 - invttail(n3-1,0.025)*(sd3 / sqrt(n3))

gen status_1 = status - 0.1
gen status_2 = status + 0.1
gen status_3 = status + 0.3

twoway (bar mean1 status_1 , color(navy%70) barwidth(0.2)) ///
(bar mean2 status_2 , color(green%70) barwidth(0.2)) ///
(bar mean3 status_3, color(maroon%70) barwidth(0.2) xlabel( 1 "Control" 2"Indirectly" 3 "Directly")) ///
(rcap hi1 low1 status_1, color(navy)) ///
(rcap hi2 low2 status_2, color(green)) ///
(rcap hi2 low3 status_3, color(maroon)) ///
(scatter  mean1 status_1, msym(none) mlab(mean1) mlabpos(6) mlabcolor(black) msize(*.5)) ///
(scatter  mean2 status_2, msym(none) mlab(mean2) mlabpos(6) mlabcolor(black) msize(*.5)) ///
(scatter  mean3 status_3, msym(none) mlab(mean3) mlabpos(6) mlabcolor(black) msize(*.5)), ///
title(Where did you hear about messages?, pos(11) size(4)) legend(order(1 "Family" 2 "Neighbors" 3 "Public Places") pos(11) ring(0) row(1)) ylabel(0(0.05)0.2)
graph export "/Users/mrfreerider/Dropbox/Aplicaciones/Overleaf/Spreading the word Paper/survey_hear_messages.pdf", replace
restore

******************************************************
* Difference of means between groups
******************************************************

global var trust_others fair_others help_others trust_neigh h_trust_water h_trust_gov h_trust_energy h_trust_envi inter_1 inter_2 image_1 image_2 net_neigh net_family net_public  

** Directly vs. Indirectly
eststo clear
eststo DIFF: estpost ttest $var if status!=1 , by(i_treated) unequal

** Directly vs. Control
eststo D: estpost ttest $var  if d_sample==1 , by(status) unequal

** Indirectly vs. Control
eststo I: estpost ttest $var if i_sample==1, by(status) unequal



//estout test2 test3 test1 using "/Users/mrfreerider/Dropbox/Aplicaciones/Overleaf/Spreading the word Paper/survey_diff_means.tex", replace style(tex) cells(b(star fmt(%9.3f)) se(fmt(%9.3f) layout("(@)")))  mlabels("Directly" "Indirectly" "Difference" , span prefix(\multicolumn{@span}{c}{) suffix(}))  collabels(none)  varlabels(image_1 "Approval" image_2 "Disapproval" net_neigh "From neighbors" net_family "From relatives" net_public "At public spaces" trust_others "Trustworthy" fair_others "Fair" help_others "Helpful" trust_neigh "Reliable neighbors (\#)" h_trust_water "Water utility" h_trust_gov "Local government" h_trust_energy "Energy utility" h_trust_envi "Environmental Agency" inter_1 "My consumption affects others" inter_2 "Others consumption affects me") refcat(image_1 "\emph{Recall message:}" net_neigh "\emph{Heard about messages:}" trust_others "\emph{Others are:}"  h_trust_water "\emph{Trust in:}" inter_1 "\textit{Reciprocity:}", nolabel) prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) prefoot() postfoot("\hline" "\end{tabular}")


esttab D I DIFF using "survey_diff_means.tex", replace se compress nogaps booktabs nonumber  mtitle("Directly" "Indirectly" "Difference") coeflabels(image_1 "Approval" image_2 "Disapproval" net_neigh "From neighbors" net_family "From relatives" net_public "At public spaces" trust_others "Most people can be trusted" fair_others "Most people would try to be fair" help_others "Most people would try to be helpful" trust_neigh "Reliable neighbors (\#)" h_trust_water "Water utility" h_trust_gov "Local government" h_trust_energy "Energy utility" h_trust_envi "Environmental Agency" inter_1 "My consumption affects others" inter_2 "Others consumption affects me") refcat(trust_others "\emph{Social capital:}"  h_trust_water "\emph{Trust in institutions:}" inter_1 "\emph{Reciprocity:}" image_1 "\emph{Messages:}" net_neigh "\emph{Channels:}", nolabel) 



** Differences in message recalling for the same group.  
eststo clear

eststo img1: quietly reg image_1 d_treated i_treated
eststo img2: quietly reg image_2 d_treated i_treated

suest img1 img2
lincom [img1_mean]d_treated - [img1_mean]i_treated, or
lincom [img2_mean]d_treated - [img2_mean]i_treated, or

******************************************************

reshape long cons above norm, i(i) j(m) string
gen year = substr(m,1,4)
gen month = substr(m,5,6)
destring year, replace
destring month, replace
drop m
gen period=ym(year, month)
format period %tm
xtset i period
sort i period 
label var cons "WC (m3)"
label var above "Higher than the Others"


** Generate Post-Treatment variable
bys i: generate time=_n
label var time "Time"
gen time_ad = time-20
label var time_ad "Time"
gen post=1 if period>=692 // after August
replace post=0 if post==.
replace post=0 if period>=697 & bill==0
label var post "Post"
label define post 0 "Before" 1 "After"
label value post post


**Setting the  panel
xtset i period
sort i period 
label var cons "Water consumption (m3/period)"
label var period "Period"
label var villageid "Village"

** norm activation
label var norm "Social norm"
replace norm=. if norm==0


gen t_post= d_treated*post 
label var t_post "D treated X Post"
gen s_post= i_treated*post
label var s_post "I treated X Post"

** Social Norm variable
replace norm=. if norm==0
replace norm=. if control
gen norm_daily= (norm/30)*1000 if bill==0
replace norm_daily=(norm/60)*1000 if bill==1
label var norm_daily "SN average (L/day)"

** Consumption variable in liters per day
gen cons_daily= (cons/30)*1000 if bill==0
replace cons_daily=(cons/60)*1000 if bill==1
label var cons_daily "WC (L/day)"

** Distance to norm
sort i period
gen dist_norm = cons_daily-norm_daily if !control
label var dist_norm "Distance to the SN (L/day)"

** Compliance to norm
gen comply=1 if dist_norm<=0 & post
replace comply=0 if dist_norm>0 & post
replace comply=. if dist_norm==.
label var comply "Compliance to the SN"

** Type of feedback
gen positive=0
replace positive=1 if comply==1 & d_treated==1 
label var positive "Positive feedback"
gen negative=0
replace negative=1 if comply==0 & d_treated==1
label var negative "Negative feedback"

replace cons=. if cons==0

**average consumption
bys period village: egen cons_prom=mean(cons_daily)
bys period village: egen cons_sd=sd(cons_daily)
gen cons_std=(cons_daily-cons_prom)/cons_sd
label var cons_std "WC (Std.)"

bys i: egen cons_bt=mean(cons_daily) if time<=20
label var cons_bt "WC before T" 
bys i: egen cons_at=mean(cons_daily) if time>20
label var cons_at "WC after T"

bys i: egen prom_cons_bt = mean(cons_bt) 
label var prom_cons_bt "WC before T"
bys i: egen prom_cons_at = mean(cons_at)
label var prom_cons_at "WC after T"
bys i: gen ch_cons =  prom_cons_at-prom_cons_bt
label var ch_cons "Change in WC" 


** Number of deliverys 
gen delivery=1 if post & d_treated
local t_bill 22 24 26
foreach x in `t_bill'{
replace delivery=. if time==`x' & bill==1
}
replace delivery=0 if delivery==.
bys i: egen n_delivery=sum(delivery)
label var n_delivery "Reports (\#)"

** Cluster variable by SES and Village
sort ses villageid
egen ses_vill = group(ses villageid)
label var ses_vill "Group SES village"


egen time_vill = group(time villageid)


** generate number of village indicator
gen n_vill = 2 
label var n_vill "Villages (\#)"






global varsurvey index_beh_avg index_pcb_avg index_gui_avg index_emb_avg index_pri_avg index_snc_avg index_snn_avg index_mon_avg index_int_avg trust_othe trust_inst trust_neigh

foreach x in $varsurvey{
xtreg cons_daily post  t_post##c.`x' s_post##c.`x'  i.period, fe vce(cluster ses_vill)
est store reg`x'
}

coefplot (regindex_beh_avg) (regindex_pcb_avg) (regindex_gui_avg) (regindex_emb_avg) (regindex_pri_avg) (regindex_snc_avg) (regindex_snn_avg) (regindex_mon_avg) (regindex_int_avg) (regtrust_othe) (regtrust_inst) (regtrust_neigh) , ///
keep(1.t_post#c.index_beh_avg 1.t_post#c.index_pcb_avg 1.t_post#c.index_gui_avg 1.t_post#c.index_emb_avg 1.t_post#c.index_pri_avg 1.t_post#c.index_snc_avg 1.t_post#c.index_snn_avg 1.t_post#c.index_mon_avg 1.t_post#c.index_int_avg 1.t_post#c.trust_othe 1.t_post#c.trust_inst 1.t_post#c.trust_neigh ) ///
xline(0,lcolor(red) lwidth(0.09)) ///
xtitle("Liters per day per household") ///
coeflabels(1.t_post#c.index_beh_avg="Pro-environmental idx" 1.t_post#c.index_pcb_avg="PCB idx" 1.t_post#c.index_gui_avg="Guilt idx" 1.t_post#c.index_emb_avg="Embarrassment idx" 1.t_post#c.index_pri_avg="Pride idx" 1.t_post#c.index_snc_avg="SN closest idx" 1.t_post#c.index_snn_avg="SN neighbors idx" 1.t_post#c.index_mon_avg="Monetary displacement idx"  1.t_post#c.index_int_avg="Future Intention idx" 1.t_post#c.trust_othe="Trust in others idx" 1.t_post#c.trust_inst="Trust in local inst idx" 1.t_post#c.trust_neigh="Trustworthy neighbors" ) format(%10.1f) ///
legend(off) title(Direct, pos(11) size(4)) ///
name(treated)



coefplot (regindex_beh_avg) (regindex_pcb_avg) (regindex_gui_avg) (regindex_emb_avg) (regindex_pri_avg) (regindex_snc_avg) (regindex_snn_avg) (regindex_mon_avg) (regindex_int_avg) (regtrust_othe) (regtrust_inst) (regtrust_neigh) , ///
keep(1.s_post#c.index_beh_avg 1.s_post#c.index_pcb_avg 1.s_post#c.index_gui_avg 1.s_post#c.index_emb_avg 1.s_post#c.index_pri_avg 1.s_post#c.index_snc_avg 1.s_post#c.index_snn_avg 1.s_post#c.index_mon_avg 1.s_post#c.index_int_avg 1.s_post#c.trust_othe 1.s_post#c.trust_inst 1.s_post#c.trust_neigh ) ///
xline(0,lcolor(red) lwidth(0.09)) ///
xtitle("Liters per day per household") ///
coeflabels(1.s_post#c.index_beh_avg="Pro-environmental idx" 1.s_post#c.index_pcb_avg="PCB idx" 1.s_post#c.index_gui_avg="Guilt idx" 1.s_post#c.index_emb_avg="Embarrassment idx" 1.s_post#c.index_pri_avg="Pride idx" 1.s_post#c.index_snc_avg="SN closest idx" 1.s_post#c.index_snn_avg="SN neighbors idx" 1.s_post#c.index_mon_avg="Monetary displacement idx"  1.s_post#c.index_int_avg="Future Intention idx" 1.s_post#c.trust_othe="Trust in others idx" 1.s_post#c.trust_inst="Trust in local inst idx" 1.s_post#c.trust_neigh="Trustworthy neighbors" ) format(%10.1f) ///
legend(off)  title(Spillover, pos(11) size(4) ) ///
name(spill)
graph combine treated spill
graph export "/Users/mrfreerider/Documents/Research/Social Nudge/Paper/survey_effects.tif", replace
graph drop treated spill Graph


**********************************************************************************************
****   Purpose:      Calculates features and Complexity Index for input lotteries.           *
****   Output:       CSV with problems, features and complexity index.                       *
****   Notes:        -                                                                       *
****   Author:       Sebastian Redl                                                          *
****   Last updated: 25 Aug, 2024.                                                           * 
**********************************************************************************************


*SET WORKING DIRECTORY HERE:
global root "/Users/sebastianredl/Dropbox (Harvard University)/ML_complexity/Complexity_Tool"


*--------------Install Packages --------------* 

*packages: ssc install project, mata, 
ssc install egenmore, replace
ssc install distinct, replace
ssc install unique, replace
ssc install estout, replace
ssc install ftools, replace
ssc install matsort, replace
ssc install missings, replace
ssc install rangestat, replace
ssc install matmap, replace


clear all

frame create index_df
frame change index_df

*--------------Loading Data--------------* 
import delimited "$root/sample_data/sample_all_indices_calculation_1.csv", clear



qui: destring *, ignore("NA") replace

*-----------------------------------------------------------
*               Preparing and Checking Data 
*-----------------------------------------------------------

*Missing columns per state and probability columns in each lottery
local x_a_n = 0
local p_a_n = 0
local x_b_n = 0
local p_b_n = 0

*number of maximal states to insert 
global num_st_max = 7


*Indicates which inidices can be calculated out of the supplied data
global indices_sel PC




capture describe x_* p_*

	if (_rc == 0){
		di "Checking if more than one lotterie in the data set in order to calculate OPC/SPC/OAC/SAC"
		capture describe x_a_* p_a_* x_b_* p_b_*
		if (_rc == 0){
			di "Two lotteries per problem found!"
		}
		else{
			di "Just one lottery found. Only OLC/SLC can be calculated"
			global indices_sel LC
			forval i = 1(1)$num_st_max{
			*transfrom data set in the x_a_ and x_b format! 
				capture confirm variable x_`i'
				if (_rc ==0){
					rename x_`i' x_a_`i' 	
					rename p_`i' p_a_`i'	
				}
			}
		gen x_b_1 = 0
		gen p_b_1 = 1
		}
	}
	else{
		di "Not even one lottery to calculate just OLC/SLC with x_* and p_* entered"
		assert _rc != 0
		
	}


*check if columns per lotterie is missing and how many are missing
forval i = 1(1)$num_st_max{
	capture confirm variable x_a_`i'
	if (_rc !=0){
		loc x_a_n = `x_a_n' + 1
		di `x_a_n'
		gen x_a_`i' = .
		
	}
	capture confirm variable p_a_`i'
	if (_rc !=0){
		loc p_a_n = `p_a_n' + 1
		di `p_a_n'
		gen p_a_`i' = .
		
	}
	capture confirm variable x_b_`i'
	if (_rc !=0){
		loc x_b_n = `x_b_n' + 1
		di `x_b_n'
		gen x_b_`i' = .
		
	}
	capture confirm variable p_b_`i'
	if (_rc !=0){
		loc p_b_n = `p_b_n' + 1
		di `p_b_n'
		gen p_b_`i' = .
		
	}
	*if probability is 0. then remove state
	replace x_a_`i' = cond(p_a_`i' == 0, ., x_a_`i')
	replace p_a_`i' = cond(p_a_`i' == 0, ., p_a_`i')
	
	replace x_b_`i' = cond(p_b_`i' == 0, ., x_b_`i')
	replace p_b_`i' = cond(p_b_`i' == 0, ., p_b_`i')
	

	
	di "Ceck if payout and probabilities match in data set for each lottery:"
	assert p_a_`i' == x_a_`i' if missing(p_a_`i') |  missing(x_a_`i')
	assert p_b_`i' == x_b_`i' if missing(p_b_`i') | missing(x_b_`i')
}



di "Check if a minimum number of two columns per lottery is in the data set"
assert (`x_a_n' < 6) | (`x_b_n' < 6)

di "Created "`x_a_n' "additional columns to have in total 7 columns for states / probabilities in lottery A"
di "Created "`x_b_n' "additional columns to have in total 7 columns for states / probabilities in lottery B"

*--------------Checking if compound is in the data frame --------------* 

capture confirm variable compound
if (_rc !=0){
	gen compound = 0
}
else{
	capture confirm string compound
			if (_rc !=0 ){
					di "Classifying compound string TRUE/FALSE as boolean" 
					gen compound_bool = cond(compound == "TRUE", 1, 0)
					drop compound
					rename compound_bool compound 
			}
}

	


*-----------------------------------------------------------
*                    Build Features 
*-----------------------------------------------------------


*--------------Bring lotteries in correlated space--------------* 

gen id = _n

global num_corr = 20

forval i = 1(1)$num_corr{
	gen cor_x_a_`i' = .
}
forval i = 1(1)$num_corr{	
	gen cor_x_b_`i' = .
}
forval i = 1(1)$num_corr{
	gen cor_p_ab_`i' = .
}


*mata function for getting unquie cdf steps and calculate correlated space
mata:
mata set matastrict off
function corrstates(a,b,cumu_m)
{
	cumu_m = uniqrows(cumu_m)
	
	m_a = J(rows(cumu_m),1,0)
	m_b = J(rows(cumu_m),1,0)

	p_ab_cor = J(rows(cumu_m),1,0)
	p_ab_cor[1,1] = cumu_m[1,1]

	for (i = 1; i <= rows(cumu_m); i++){

		a_tmp = select(a, a[.,3]:>=cumu_m[i,1])
		b_tmp = select(b, b[.,3]:>=cumu_m[i,1])

		m_a[i,1] = colmin(a_tmp[.,1])
		m_b[i,1] = colmin(b_tmp[.,1])

		if(i > 1){
			p_ab_cor[i,1] = cumu_m[i,1] - cumu_m[i-1,1]
			}
		}

		st_matrix("p_ab_cor_st", p_ab_cor)
		st_matrix("m_a_st", m_a)
		st_matrix("m_b_st", m_b)
	return (0)
	}	
end


local N = _N

*iterate over problems
forval row = 1(1)`N'{
	preserve
	
	*get problem and just payouts which dont have an NA
	qui keep if id == `row'	
	qui missings dropvars, force 

	*extract lotteries as matrix
	foreach lot in a b{
		foreach char in x p{
			mkmat `char'_`lot'_*, matrix(`char'_`lot')
			matmap `char'_`lot' `char'_`lot', map(round(@, 0.001))	
		}

	*combine matrix	
	matrix m_`lot' = (x_`lot' \ p_`lot')'

	*sort matrix
	matsort m_`lot' 1 "up"
	matrix colnames m_`lot' = x p
	mat cumu = m_`lot'[1, "p"]

	*get cdfs
	local row_num = (rowsof(m_`lot'))
	if(rowsof(m_`lot') > 1){
		forval i = 2(1)`row_num'{
					 matrix cumu =  cumu \  m_`lot'[`i',2] + cumu[`i' - 1, 1]
		}
	}

	matrix colnames cumu =  cumu
	matrix m_`lot' = (m_`lot' , cumu)

	}


	*combine cdfs
	matrix cumu = m_a[1..., "cumu"] \ m_b[1..., "cumu"]
	
	*make mata compatible
	mata: a = st_matrix("m_a")
	mata: b = st_matrix("m_b")
	mata: cumu_m = st_matrix("cumu")
	*get correlated states
	mata: a = corrstates(a,b,cumu_m)

	restore

	*safe in the specific cells and leave rest empty
	local row_num_cor = rowsof(p_ab_cor_st)
	forval i = 1(1)`row_num_cor'{
		qui replace cor_x_a_`i' = cond(id == `row', m_a_st[`i',1], cor_x_a_`i')
		qui replace cor_x_b_`i' = cond(id == `row', m_b_st[`i',1], cor_x_b_`i')
		qui replace cor_p_ab_`i' = cond(id == `row', p_ab_cor_st[`i',1], cor_p_ab_`i')
	}
	
	di "Correllated states: (`row'/`N')"

}


*--------------Expected values--------------* 

gen double ev_a = 0
gen double ev_b = 0 

forval i = 1(1)$num_st_max{
	replace ev_a = ev_a + x_a_`i' * p_a_`i' if !missing(x_a_`i') & !missing(p_a_`i' )	
	replace ev_b = ev_b + x_b_`i' * p_b_`i' if !missing(x_b_`i') & !missing(p_b_`i' )		
}


*--------------Excess Dissimilarity--------------* 

gen double ln_cdf_diff_abs = 0

forval i = 1(1)$num_corr{
	replace ln_cdf_diff_abs =  ln_cdf_diff_abs + abs(cor_x_a_`i' - cor_x_b_`i') * ///
	cor_p_ab_`i' if !missing(cor_x_a_`i') & !missing(cor_x_b_`i') & !missing(cor_p_ab_`i')
}


replace ln_cdf_diff_abs = ln(ln_cdf_diff_abs - abs(ev_a - ev_b) + 1)


*--------------No Dominance--------------* 

gen aGb = 1
gen bGa = 1
forval i = 1(1)$num_corr{
	replace aGb = 0 if (cor_x_a_`i' < cor_x_b_`i') ///
	& (!missing(cor_x_b_`i') & !missing(cor_x_a_`i'))
	replace bGa = 0 if (cor_x_b_`i' < cor_x_a_`i') ///
	& (!missing(cor_x_b_`i') & !missing(cor_x_a_`i'))
}

gen nodom = cond((bGa > aGb | bGa < aGb), 0, 1)


drop aGb bGa


*--------------Gains --------------* 

gen double gains_a = 1
gen double gains_b = 1
forval i = 1(1)$num_st_max{
	replace gains_a = cond((gains_a == 1 ) & (x_a_`i' >= 0 ), 1, 0) if !missing(x_a_`i')
	replace gains_b = cond((gains_b == 1 ) & (x_b_`i' >= 0 ), 1, 0) if !missing(x_b_`i')
}

gen double not_gains_a = 1 - gains_a
gen double not_gains_b = 1 - gains_b
gen double ave_not_gains = 1 - (gains_a + gains_b) / 2
drop gains_a gains_b


*--------------Average log payout magnitude--------------* 

gen double ln_scale_a = 0
gen double ln_scale_b = 0
gen n_a = 0
gen n_b = 0
forval i = 1(1)$num_st_max{
	replace ln_scale_a =  ln_scale_a  + abs(x_a_`i') if !missing(x_a_`i')
	replace ln_scale_b =  ln_scale_b  + abs(x_b_`i') if !missing(x_b_`i')
	
	replace n_a =  n_a  + 1 if  !missing(x_a_`i')
	replace n_b =  n_b  + 1 if !missing(x_b_`i')	
}

replace ln_scale_a = ln(1 + ln_scale_a / n_a)
replace ln_scale_b = ln(1 + ln_scale_b / n_b)

gen double ave_ln_scale = (ln_scale_a + ln_scale_b) / 2


*--------------log number of states--------------* 

gen double ln_nstates_a = ln(n_a + 1)
gen double ln_nstates_b = ln(n_b + 1)
gen double ave_ln_nstates = (ln_nstates_a + ln_nstates_b) /2
drop n_a n_b


*--------------Absolut expected value difference --------------* 

gen double abs_ev_diff = abs(ev_a - ev_b)


*--------------Squared absolut expected value difference --------------* 

gen double abs_ev_diff_sq = abs_ev_diff^2

*--------------Log Variance --------------* 


gen double ln_var_a = 0
gen double ln_var_b = 0

forval i = 1(1)$num_st_max{
	replace ln_var_a = ln_var_a + (x_a_`i' * x_a_`i') * p_a_`i'  if ///
	!missing(p_a_`i') & !missing(x_a_`i')
	replace ln_var_b = ln_var_b + (x_b_`i' * x_b_`i') * p_b_`i' if ///
	!missing(p_b_`i') & !missing(x_b_`i')
	
}
	
replace ln_var_a = ln(ln_var_a - ev_a^2 + 1)
replace ln_var_b = ln(ln_var_b - ev_b^2 + 1)

drop ev_a ev_b

count
global obs_n = r(N)


*-----------------------------------------------------------
*                    Calculate Indices
*-----------------------------------------------------------

*features problems complexity indices
local features_pc abs_ev_diff abs_ev_diff_sq ave_ln_scale ave_ln_nstates ///
                 compound nodom ave_not_gains ln_cdf_diff_abs 

*features aggregation complexity indices
local features_ac ave_ln_scale ave_ln_nstates ///
                 compound nodom ave_not_gains ln_cdf_diff_abs 

*features lottery complexity of lottery A				 
local features_lc_a ln_scale_a ln_nstates_a compound not_gains_a  ln_var_a

*features lottery complexity of lottery B
local features_lc_b  ln_scale_b ln_nstates_b compound not_gains_b ln_var_b

*Indices to calculate
local indices OPC SPC OAC SAC OLC_a OLC_b SLC_a SLC_b
if ("$indices_sel" == "LC"){
	local indices OLC_a SLC_a
}

*import coefficients
frame create coef
frame change coef
clear
import delimited "$root/coef/all_coef.csv"
destring, replace

*iterate over indicies, select correct features and calculate convex
*combination
foreach index in `indices'{
di "`index'"
*local index OLC_a
local indices_name `index'

	if("`index'" == "OPC"){
		local features_used `features_pc'
		local indices_name opc
	}
	if("`index'" == "SPC"){
		local features_used `features_pc'
		local indices_name spc
	}
	if("`index'" == "OAC"){
		local features_used `features_ac'
		local indices_name oac
	}
	if("`index'" == "SAC"){
		local features_used `features_ac'
		local indices_name sac
	}
	if("`index'" == "OLC_a"){
		local features_used `features_lc_a'
		local indices_name olc
	}
	if("`index'" == "OLC_b"){
		local features_used `features_lc_b'
		local indices_name olc
	}
	if("`index'" == "SLC_a"){
		local features_used `features_lc_a'
		local indices_name slc
	}
	if("`index'" == "SLC_b"){
		local features_used `features_lc_b'
		local indices_name slc
	}
	
	frame change coef
	preserve

	*transpose dataframe of the coefficients of the selected index
	di "`indices_name'"
	keep features `indices_name'
	rename `indices_name' coefficient1
	reshape long coefficient, i(features) j(id) 
	reshape wide coefficient, i(id) j(features) string
	rename (coefficient*) (*_`index') 
	rename (_cons_`index') (constant_`index') 
	expand $obs_n
	replace id = _n


	*Merge all feature coefficients to lottery dataframe
	frame change index_df
	frlink 1:1 id, frame(coef)
	frget *, from(coef)
	drop coef

	frame change coef
	restore 
	
	*calculate index
	frame change index_df
	*intercept:
	gen double `index' =  constant_`index'
	*convex combination:
	foreach var in `features_used'{
		local varind = "`var'_`index'"
		di "`varind'"
		di "`index'"
		*for OLC_b and SLC_B use OLC_a and SLC_a lottery coeffients
		*as they are for each lottery the same
		if (("`index'" == "OLC_b" )| ("`index'" == "SLC_b")){
			if ("`var'" != "compound"){
			local varind = substr("`var'", 1, length("`var'") - 1)
			local varind = "`varind'a_`index'"
			}
		}
		replace `index' = ( `varind' ) * ( `var' ) + `index'  if !missing(`var')
		
	}
	*winsorize index at 0
	replace `index' = 0 if `index' < 0
	*drop coefficients from the dataframe
	drop *_`index'

}



*---------------Labeling drop variables----------------------*

label var compound "1 if compound prob."
label var abs_ev_diff "Absolute difference in expected values"
label var abs_ev_diff_sq "Absolute difference in expected values sq."
label var ln_cdf_diff_abs "Log excess dissimilarity"
label var ave_ln_scale "Average log payout magnitude"
label var ave_ln_nstates "Average log number of states"
label var ave_not_gains "Frac. lotteries involving loss"
label var nodom "No dominance"

order x_a* p_a* x_b* p_b* `inidices' `features_pc' `features_ac' `features_lc_a' ///
 `features_lc_b'
capture confirm variable problem 
if (_rc ==0){
	order problem x_a* p_a* x_b* p_b* `indices'
	label var problem "Problem ID"
}


if ("$indices_sel" == "LC"){
	drop `features_pc' `features_ac' `features_lc_b' cor_* x_b* p_b* id
	local keep_features x_a* p_a_* `indices' `features_lc_a'
	label var OLC_a "Objective Lottery Complexity"
	label var SLC_a "Subjective Lottery Complexity"
	label var ln_var_a "Log variance"
	label var ln_scale_a "Log payout magnitude"
	label var ln_nstates_a "Log number of states"
	label var not_gains_a "1 if involves loss"
	forval i = 1(1)$num_st_max{
	label var x_a_`i' "Payout of state `i'"
	label var p_a_`i' "Probability for state `i'"
	}
	rename ln_var_a ln_variance_a
	rename ln_scale_a ln_payout_magn_a
	rename ln_nstates_a ln_num_numstates_a
	rename not_gains_a involves_loss_a
}
else{
	drop cor_* id
	label var OPC "Objective Problem Complexity"
	label var SPC "Subjective Problem Complexity"
	label var OAC "Objective Aggregation Complexity"
	label var SAC "Subjective Aggregation Complexity"
	label var OLC_a "Objective Lottery Complexity of Lottery A"
	label var SLC_a "Subjective Lottery Complexity of Lottery A"
	label var OLC_b "Objective Lottery Complexity of Lottery B"
	label var SLC_b "Subjective Lottery Complexity of Lottery B"
	label var ln_var_a "Log variance of Lottery A"
	label var ln_scale_a "Log payout magnitude of Lottery A"
	label var ln_nstates_a "Log number of states of Lottery A"
	label var not_gains_a "1 if involves loss of Lottery A"
	label var ln_var_b "Log variance of Lottery B"
	label var ln_scale_b "Log payout magnitude of Lottery B"
	label var ln_nstates_b "Log number of states of Lottery B"
	label var not_gains_b "1 if involves loss of Lottery B"
	foreach lot in a b{
		forval i = 1(1)$num_st_max{
			label var x_`lot'_`i' "Lottery `lot' payout of state `i'"
			label var p_`lot'_`i' "Lottery `lot' probability for state `i'"
		}
	}
	rename ln_cdf_diff_abs ln_excess_dissimilarity
	rename ln_var_a ln_variance_a
	rename ln_var_b ln_variance_b
	rename ln_scale_a ln_payout_magn_a
	rename ln_scale_b ln_payout_magn_b
	rename ln_nstates_a ln_num_numstates_a
	rename ln_nstates_a ln_num_numstates_b
	rename not_gains_a involves_loss_a
	rename not_gains_b involves_loss_b
	

rename ave_ln_scale ave_ln_payout_magn_a
label var ave_ln_nstates ave_ln_num_numstates_a
label var ave_not_gains "Frac. lotteries involving loss"
label var nodom "No dominance"
}


*drop added states and probabilities with just NA

foreach var of varlist _all {
     capture assert mi(`var')
     if !_rc {
        drop `var'
     }
 }
 
capture assert compound == 0
if _rc == 0 drop `v'
 
if ("$indices_sel" == "LC"){
	rename *_a *
	rename *_a_* *_*
}
else{

}


 
 
*-------------- Save dataframe --------------* 

save "$root/output/index_calculated_stata.dta", replace
export delimited "$root/output/index_calculated_stata.csv", replace

*---------------------   End of file  ---------------------*


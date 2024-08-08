
**********************************************************************************************
****   Purpose:      Calculates features and Complexity Index for input lotteries.           *
****   Output:       CSV with problems, features and complexity index.                       *
****   Notes:        -                                                                       *
****   Author:       Sebastian Redl                                                          *
****   Last updated: 01 Aug, 2024.                                                           * 
**********************************************************************************************


*SET WORKING DIRECTORY HERE:
global root "/Users/benjaminenke/Dropbox/Research/Projects/ML_complexity/Complexity_Tool"

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
import delimited "$root/sample_data/jpe_randomization.csv", clear
/* import delimited "$root/sample_data/sample_all_indices_calculation_1.csv", clear */
/* import delimited "$root/sample_data/sample_just_OLC_SLC_calculation_1.csv", clear */

// List all variables in the dataset and save them in a local macro
ds
local initial_vars `r(varlist)'

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
global num_st_max = 9


* Initialize global variable for single_lottery
global single_lottery "false"


capture describe x_* p_*

	if (_rc == 0){
		di "Checking if more than one lotterie in the data set in order to calculate OCI/SCI/OAC/SAC"
		capture describe x_a_* p_a_* x_b_* p_b_*
		if (_rc == 0){
			di "Two lotteries per problem found!"
		}
		else{
			di "Just one lottery found. Only OLCI/SLCI can be calculated"
			global single_lottery "true"
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
		di "Not even one lottery to calculate just OLCI/SLCI with x_* and p_* entered"
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
	

	
	di "Check if payout and probabilities match in data set for each lottery:"
	assert p_a_`i' == x_a_`i' if missing(p_a_`i') |  missing(x_a_`i')
	assert p_b_`i' == x_b_`i' if missing(p_b_`i') | missing(x_b_`i')
}



di "Check if a minimum number of two columns per lottery is in the data set"
assert (`x_a_n' < 8) | (`x_b_n' < 8)

di "Created "`x_a_n' "additional columns to have in total 9 columns for states / probabilities in lottery A"
di "Created "`x_b_n' "additional columns to have in total 9 columns for states / probabilities in lottery B"

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

*--------------Expected values--------------* 

gen double ev__a = 0
gen double ev__b = 0 

forval i = 1(1)$num_st_max{
	replace ev__a = ev__a + x_a_`i' * p_a_`i' if !missing(x_a_`i') & !missing(p_a_`i' )	
	replace ev__b = ev__b + x_b_`i' * p_b_`i' if !missing(x_b_`i') & !missing(p_b_`i' )		
}

* Check if single_lottery is true and make adjustments if needed
if "$single_lottery" == "true" {
	replace ev__b = ev__a
	replace x_b_1 = ev__a
}

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
	
	di "Correlated states: (`row'/`N')"
}

*--------------Excess Dissimilarity--------------* 

gen double ln_cdf_diff_abs__ab = 0

forval i = 1(1)$num_corr{
	replace ln_cdf_diff_abs__ab =  ln_cdf_diff_abs__ab + abs(cor_x_a_`i' - cor_x_b_`i') * ///
	cor_p_ab_`i' if !missing(cor_x_a_`i') & !missing(cor_x_b_`i') & !missing(cor_p_ab_`i')
}


replace ln_cdf_diff_abs__ab = ln(ln_cdf_diff_abs__ab - abs(ev__a - ev__b) + 1)

*--------------Gains --------------* 

gen double gains_a = 1
gen double gains_b = 1
forval i = 1(1)$num_st_max{
	replace gains_a = cond((gains_a == 1 ) & (x_a_`i' >= 0 ), 1, 0) if !missing(x_a_`i')
	replace gains_b = cond((gains_b == 1 ) & (x_b_`i' >= 0 ), 1, 0) if !missing(x_b_`i')
}

gen double ave_gains__ab = (gains_a + gains_b) / 2
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

gen double ave_ln_scale__ab = (ln_scale_a + ln_scale_b) / 2

*--------------Average log number of states--------------* 

gen double ln_nstates_a = ln(n_a + 1)
gen double ln_nstates_b = ln(n_b + 1)
gen double ave_ln_nstates__ab = (ln_nstates_a + ln_nstates_b) / 2
drop n_a n_b


*--------------Absolute expected value difference --------------* 

gen double abs_ev_diff__ab = abs(ev__a - ev__b)

gen double ln_abs_ev_diff__ab = ln(1 + abs_ev_diff__ab)

drop ev__a ev__b abs_ev_diff__ab

count
global obs_n = r(N)

*-----------------------------------------------------------
*                    Calculate Indices
*-----------------------------------------------------------

*features problems complexity indices
local features_ci ln_abs_ev_diff__ab ave_ln_scale__ab ///
								 ave_ln_nstates__ab compound ave_gains__ab ln_cdf_diff_abs__ab

*features aggregation complexity indices
local features_ac ave_ln_scale__ab ave_ln_nstates__ab compound ave_gains__ab ///
									ln_cdf_diff_abs__ab

*Indices to calculate
local indices OCI SCI OAC SAC

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
local indices_name `index'

	if("`index'" == "OCI"){
		local features_used `features_ci'
		local indices_name oci
	}
	if("`index'" == "SCI"){
		local features_used `features_ci'
		local indices_name sci
	}
	if("`index'" == "OAC"){
		local features_used `features_ac'
		local indices_name oac
	}
	if("`index'" == "SAC"){
		local features_used `features_ac'
		local indices_name sac
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

		replace `index' = ( `varind' ) * ( `var' ) + `index'  if !missing(`var')
		
	}
	*winsorize index at 0
	replace `index' = 0 if `index' < 0

	if("`index'" == "OCI"){
		replace `index' = 0.5 if `index' > 0.5
	}
	if("`index'" == "SCI"){
		replace `index' = 0.5 if `index' > 0.5
	}

	*drop coefficients from the dataframe
	drop *_`index'
}

*---------------Labeling variables----------------------*

order x_a* p_a* x_b* p_b* `indices' 
capture confirm variable problem 
if (_rc ==0){
	order problem x_a* p_a* x_b* p_b* `indices'
	label var problem "Problem ID"
}


if "$single_lottery" == "true"{
	drop x_b* p_b* id
	local keep_features `initial_vars' OLCI SLCI
	rename OCI OLCI
	rename SCI SLCI
	label var OLCI "Objective Lottery Complexity"
	label var SLCI "Subjective Lottery Complexity"
	forval i = 1(1)$num_st_max{
	label var x_a_`i' "Payout of state `i'"
	label var p_a_`i' "Probability for state `i'"
	}
}
else{
	local keep_features `initial_vars' OCI SCI OAC SAC
	label var OCI "Objective Problem Complexity"
	label var SCI "Subjective Problem Complexity"
	label var OAC "Objective Aggregation Complexity"
	label var SAC "Subjective Aggregation Complexity"
	foreach lot in a b{
		forval i = 1(1)$num_st_max{
			label var x_`lot'_`i' "Lottery `lot' payout of state `i'"
			label var p_`lot'_`i' "Lottery `lot' probability for state `i'"
		}
	}
}

capture assert compound == 0
 
if ("$single_lottery" == "true"){
	rename *_a *
	rename *_a_* *_*
}
 
*-------------- Save dataframe --------------* 
* keep only keep_features
ds `keep_features', not
drop `r(varlist)'
order problem `keep_features'

save "$root/output/index_calculated_stata.dta", replace
export delimited "$root/output/index_calculated_stata.csv", replace

*---------------------   End of file  ---------------------*


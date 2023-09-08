# Complexity-Index

Exploring the role of complexity for reasoning in choice problems. Provided are R (`calc_index.R`) and Stata (`calc_index.do`) files to calculated complexity indices from Quantifying Lottery Choice Complexity (https://benjamin-enke.com/pdf/Quant_complexity.pdf):

**OPC**: Objective Problem Complexity
**SPC**: Subjective Problem Complexity
**OAC**: Objective Aggregation Complexity
**SAC**: Subjective Aggregation Complexity
**OLC**: Objective Lottery Complexity
**SLC**: Subjective Lottery Complexity

Depending on the Input, the tool calculates all these inidcies and saves it in the `output' folder, including the features which where necessary to obtain the indices. 

## Input 
Max number of lotteries: 2
Max number of states per lottery: 7
Payout value is ignore if its probability is 0

If two lotteries are supplied, the column names should be 


| **problem** | **x_a_1** | **x_a_2** | **x_a_3** | **...** | **p_a_1** | **p_a_2** | **p_a_3** | **...** | **x_b_1** | **x_b_2** | **x_b_3** | **...** | **p_a_1** | **p_a_2** | **p_a_3** | **...** | **compound** |
|-------------|-----------|-----------|-----------|---------|-----------|:---------:|----------:|---------|-----------|-----------|-----------|---------|-----------|:---------:|----------:|---------|--------------|
| 1           | 10        | 5         |           |         | 0.5       |    0.5    |           |         | 3         |           |           |         | 1         |           |           |         | 0            |
| 2           | 2         | 4         |           |         | 0.3       |    0.7    |           |         | 2         |           |           |         | 1         |           |           |         | 1            |
| 3           | 1         | 2         | 3         |         | 0.2       |    0.2    |       0.6 |         | 1         | 2         | 3         |         | 0.2       |    0.5    |       0.3 |         | 1            |

Same number of state payouts as state probabilites should be supplied. 

Column typer of csv should be numeric with US number format. 

If one lotterie is supplied, naming can be either x_a_1, x_a_2, … with corresponding p_a_1, p_a_2, … up to x_a_7 with p_a_7 or less columns. Or without the _a and just x_1, x_2, with P_1, p_2. Then, just the Objective Lottery Complexity index (OLC) and Subjective Lottery Complexity Index (SLC) is provided by the code. 

If two lotteries are supplied by the data set, than the distinguishing between the lotteries should be with “a” and “b”, specifically: x_a_1, x_a_2, … with corresponding p_a_1, p_a_2, … and x_b_1, x_b_2, … up to x_a_7 with p_a_7 and x_b_7 with p_b_7  or less column.

Problem ID  as “problem"

if columns are given as strings, empty strings are interpreted as NA and converted to Numeric

calculates choice complexity if all lottery B are a safe payment

Compound Lotteries?

	-> if nothing supplied, than code will assume just … lotteries
	
output is structures as:

local keep_features x_a* p_a_* x_b* p_b*  `indices' features_pc features_ac features_lc_a features_lc_b
See muster dataframes… 

can have NA holes!

output folder 

sample folder

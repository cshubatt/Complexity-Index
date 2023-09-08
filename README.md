# Complexity-Index

Exploring the role of complexity for reasoning in choice problems. Provided are R (`calc_index.R`) and Stata (`calc_index.do`) files to calculate complexity indices from Quantifying Lottery Choice Complexity (https://benjamin-enke.com/pdf/Quant_complexity.pdf):

**OPC**: Objective Problem Complexity
**SPC**: Subjective Problem Complexity
**OAC**: Objective Aggregation Complexity
**SAC**: Subjective Aggregation Complexity
**OLC**: Objective Lottery Complexity
**SLC**: Subjective Lottery Complexity

Depending on the Input, the tool calculates all these indices and saves them in the `output' folder, including the necessary features to obtain the indices. 

## Input 
- Max number of lotteries: 2
- Max number of states per lottery: 7
- Payout value is ignored if its probability is 0
- Problem ID is optional and should be indicated by a column name `problem`.
- CSV
- Not existing probabilities and states can be indicated by `""`, `"NA"` or ``

### Choice Complexity
Two lotteries are supplied, the column names should be as displayed in the table below where *_a_*/*_b_* indicates different lotteries and the number **i** payout x_a_**i** to probability x_a_**i** up to **i = 7 ** (or less columns). Each state has to match a probability. Also see sample `SAMPLE` or `SAMPLE` (sample folder).

| **problem** | **x_a_1** | **x_a_2** | **x_a_3** | **...** | **p_a_1** | **p_a_2** | **p_a_3** | **...** | **x_b_1** | **x_b_2** | **x_b_3** | **...** | **p_a_1** | **p_a_2** | **p_a_3** | **...** | **compound** |
|-------------|-----------|-----------|-----------|---------|-----------|:---------:|----------:|---------|-----------|-----------|-----------|---------|-----------|:---------:|----------:|---------|--------------|
| 1           | 10        | 5         |           |         | 0.5       |    0.5    |           |         | 3         |           |           |         | 1         |           |           |         | 0            |
| 2           | 2         | 4         |           |         | 0.3       |    0.7    |           |         | 2         |           |           |         | 1         |           |           |         | 1            |
| 3           | 1         | 2         | 3         |         | 0.2       |    0.2    |       0.6 |         | 1         | 2         | 3         |         | 0.2       |    0.5    |       0.3 |         | 1            |


### Compound 
An additional optional column can be used to indicate that any lottery is a compound lottery. However, we just used for estimating the complexity coefficients a specfic type of compound lotteries:

### Lottery Complexity
If Just one lottery is supplied, the column name of the lotteries can be either as displayed above or just **x_1, x_2, ... p_1, p_2** up to 7 states.  
See sample `SAMPLE` or `SAMPLE` (sample folder)

## Output
The results are saved in `output` with `index_calculated_R.csv` or `index_calculated_stata.csv` respectively, including features which are necessary for the calculations.

### Choice Complexity
If two lotteries are supplied as above, all 6 indices are automatically calculated. The results are ordered in the CSV as follows: Problem [Optional] | Supplied probabilities and payouts | OPC | SPC | OAC | SAC | OLC_a | SLC_a |  OLC_a | SLC_a | and then in the same order of the indices the necessary features for their calculations|. _a, _b of for OLC and SLC indicates to which lottery the complexity index is referring too. 
## Choice Complexity
If  just one lottery is supplied the lottery complexity is calculated (OLC/SLC). In principle, the ordering of the output is the same as for the **Choice Complexity** output. However, _a as a working labelling is introduced when lottery states and probabilities are supplied with labelling **x_1, x_2, ... p_1, p_2**. Additionally, the indices have the appendix _a referring to these lottery fundamentals. 

## Running the R Script

### Running the Stata Script
ROOT




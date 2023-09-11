# Complexity-Index

Exploring the role of complexity for reasoning in choice problems. Provided are R (`calc_index.R`) and Stata (`calc_index.do`) files to calculate complexity indices from Quantifying Lottery Choice Complexity (https://benjamin-enke.com/pdf/Quant_complexity.pdf):

- **OPC**: Objective Problem Complexity
- **SPC**: Subjective Problem Complexity
- **OAC**: Objective Aggregation Complexity
- **SAC**: Subjective Aggregation Complexity
- **OLC**: Objective Lottery Complexity
- **SLC**: Subjective Lottery Complexity

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

The features for each indices are the following. Please see `Section 4` for the development of the indices and appendix *Potential Complexity Features* for details about feature definition in [Quantifying Lottery Choice Complexity](https://benjamin-enke.com/pdf/Quant_complexity.pdf). <br>
Consider a choice between two lotteries indexed by $j$ and denoted by letters $A$ and $B$. Each lottery is characterized by payout probabilities $(p_1^j,...p^j_{k_j}$ and payoff $(x_1^j,...x^j_{k_j}$ where $k_j$ denotes the number of distinct payout states of lottery j.

#### `OPC` and `SPC`
**Log excess dissimilarity (`ln_cdf_diff_abs`):**<br>
When $F_A(x)$ and $F_B(x)$ are the CDFs of Lottery A and B with EV(.) indicating the excpected value of a lottery then *Log excess dissimilarity* is defined as 

$$log\Big( 1+\int_\mathbb{R} |F_A(x) - F_B(x)|dx - |EV(A) - EV(B)|\Big) $$

**No dominance (`nodom`):**<br>
$$\exists x_1 , x_2: F_A(x_1) < F_B(x_1) \land F_A(x_2) >F_B(x_2)  $$
**Average log payout magnitude (`ave_ln_scale`):**<br>
$$log \Big( 1+ 0.5 (1/k_A\sum_{s=1}^{k_A}|x_s^A| +  1/k_B\sum_{s=1}^{k_B}|x_s^B|) \Big)$$
**Average log number of states (`ave_ln_nstates`)**:<br>
$$log(1 + \frac{k_A + k_B}{2})$$
**Frac. lotteries involving loss (`ave_not_gains`):**<br>
**If one lottery choice is compound (according to definition [above](###Compound))**
**Absolute expected value difference (`abs_ev_diff`)**:<br>
$$|EV(A) - EV(B)|$$
**Absolute expected value difference sqaured (`abs_ev_diff_sq`)**:<br>
$$|EV(A) - EV(B)|^2$$

### `OAC` and `SAC`
As above but without `abs_ev_diff` and `abs_ev_diff_sq` features.

### `OLC_a/b` and `SLC_a/b`
In the following, features are defined for both lotteries inidicated with $j\in\{A,B\}$.<br>

**Log Variance (`ln_var_a/b`):**<br>
$$log\Big ( 1+  \sum_{s=1}^{k_j} p_s^j(x_i^j)^2 -( \sum_{s=1}^{k_j} p_s^jx_s^j)^2 \Big)$$

**Log payout magnitude (`ln_scale_a\b`):***<BR>
$$log\Big( 1 + 1/k_j\sum_{s=1}^{k_j}|x_s^j|  \Big)$$

		

**Log number of states (`ln_nstates_a/b`):**<br>

$$ log \Big ( 1 + k_j \Big )$$

**1 if involves loss (`not_gains_a/b`)**<br>
**1 if involves compound probability(`compound`)**<br> 

### Lottery Complexity
If  just one lottery is supplied the lottery complexity is calculated (OLC/SLC). In principle, the ordering of the output is the same as for the **Choice Complexity** output. However, _a as a working labelling is introduced when lottery states and probabilities are supplied with labelling **x_1, x_2, ... p_1, p_2**. Additionally, the indices have the appendix _a referring to these lottery fundamentals. 

## Running the R Script

The R Script `calc_index.R` should be able to run using the isolated environment stored in `renv`. In order to activate this environment, you will first need to ensure that `R` and the package `renv` are installed on your machine. In the R console, navigate to the project directory and run `renv::restore()`, to automatically install all required packages by using the `renv.lock` file. Then calling `renv::activate()` should be all you need to do to be ready to run the R code. All necessary code to activate your package environment can be found at the beginning of `calc_index.R` within the first `IF` brackets.

Run `calc_index.R`. Which inicides will be computed is automatically determined by the supplied data ([See Section Input](#input))

## Running the Stata Script

1. Set in `global root = ""`  a working directory
2. To the best of our knowledge, we included all necessary ssc install commands in the the beginning of the `calc_index.do` Stata Script
3. Run `calc_index.do`. Which inicides will be computed is  automatically determined by the supplied data ([See Section Input](#input)). 




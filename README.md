# Complexity-Index

Provided are R (`calc_index.R`) and Stata (`calc_index.do`) files to calculate complexity indices from [Quantifying Lottery Choice Complexity (Enke & Shubatt 2023)](https://benjamin-enke.com/pdf/Quant_complexity.pdf):

If pairs of lotteries are input (i.e. `A` and `B`), we calculate:

- **OCI**: Objective Complexity Index
- **SCI**: Subjective Complexity Index
- **OAC**: Objective Aggregation Complexity
- **SAC**: Subjective Aggregation Complexity

If single lotteries are input, we calculate:

- **OLCI**: Objective Lottery Complexity Index
- **SLCI**: Subjective Lottery Complexity Index

Depending on the Input, the tool calculates these indices and saves them in the `output` folder.

## Input

The code automatically recognizes the number of lotteries in the input data and therefore calculates either complexity indices of the choice problem (see [subsection Choice Complexity](#choice-complexity) or just the complexity of the lottery if just one lottery is supplied (see [subsection Lottery Complexity](#lottery-complexity)).

- Max number of lotteries: 2
- Max number of states per lottery: 9
- Payout value is ignored if its probability is 0
- Payouts should be distinct in each lottery
- Problem ID is optional and should be indicated by the column name `problem`.
- CSV format
- Not existing probabilities and states can be indicated by `,"",`, `,"NA",` or `,,`
- Additional columns can be in the input data and will not be manipulated by code, as long as it does not match the pattern `x_`, `p_`, `_a_`, `_a`, `_b_`, `_b` and `cor_`

### Choice Complexity

If two lotteries are supplied, the column names should be as displayed in the table below. The columns indicating payouts should take the form `x_{l}_{i}`, where $l \in \{a,b\}$ indicates which lottery the payout belongs to, and $i$ indexes the lottery states. Similarly, the column names indicating probabilities should take the form `p_{l}_{i}`. The state index $i$ should take values between 1 and $k_{\{l\}}$, where $k_{\{l\}}$ is the maximum number of states in any of the $l$ lotteries. As written, the code can process a maximum of $k_{\{l\}} = 9$ distinct states. If a payout is repeated in a lottery, they will be treated as separate states. To calculate the complexity-indices in [(Enke & Shubatt 2023)](https://benjamin-enke.com/pdf/Quant_complexity), we always first collapsed problems to treat same payouts as same state. If both lotteries take on a maximum of two states, then you would include the columns `x_a_1`, `x_a_2`, `p_a_1`, `p_a_2`, and similarly for $b$. See sample `sample_all_indices_calculation_1.csv` or `sample_all_indices_calculation_2.csv` in the `sample_data` folder for an example of a correct input format.

| **[problem]** | **x_a_1** | **x_a_2** | **x_a_3** | **...** | **p_a_1** | **p_a_2** | **p_a_3** | **...** | **x_b_1** | **x_b_2** | **x_b_3** | **...** | **p_a_1** | **p_a_2** | **p_a_3** | **...** | **[compound]** | **Any**  |
| ------------- | --------- | --------- | --------- | ------- | --------- | :-------: | --------: | ------- | --------- | --------- | --------- | ------- | --------- | :-------: | --------: | ------- | -------------- | -------- |
| 1             | 10        | 5         |           |         | 0.5       |    0.5    |           |         | 3         |           |           |         | 1         |           |           |         | 0              | other    |
| 2             | 2         | 4         |           |         | 0.3       |    0.7    |           |         | 2         |           |           |         | 1         |           |           |         | 1              | column   |
| 3             | 1         | 2         | 3         |         | 0.2       |    0.2    |       0.6 |         | 1         | 2         | 3         |         | 0.2       |    0.5    |       0.3 |         | 1              | possible |

### Compound

An additional optional column can be used to indicate that one of the lotteries is a compound lottery. However, we use a very specific type of compoundness in our estimation procedure. Specifically, we considered two-state lotteries where the probabilities `p_{l}_1` and `p_{l}_2` were not given explicitly to participants. Instead we told them that the probability of the first state $p$ would be drawn randomly from a uniform distribution on the interval $[p_{\min}, p_{\max}]$, and we varied the value of $p_{\min}$ and $p_{\max}$. The probability of the second state is then, of course, given by $1-p$. We allowed for at most one of the two options to have this type of compoundness. The compound indicator on which the complexity index model is trained _only_ uses this definition of compoundness; we cannot speak to its robustness to alternative definitions. If you do not include `compound` in your column names, it will automatically be set to `False` for all problems.

### Lottery Complexity

If only one lottery is supplied, the lottery column names can be either as displayed above (using only the `_a_` columns and no `_b_` columns). Alternatively, the `_a_` segment may be omitted, in which case payoff columns will take the form `x_{i}` and probability columns will take the form `p_{i}`. Again, $i$ will range between 0 and $k$, the largest number of distinct states in any lottery; the code can handle a maximum value of $k = 9$. For an example of the correct input format, see `sample_just_OLC_SLC_calculation_1.csv` or `sample_just_OLC_SLC_calculation_2.csv` in the `sample_data` folder.

## Output

The results will be saved in `output` with `index_calculated_R.csv` or `index_calculated_stata.csv` depending on which script you run, including features which are necessary for the calculations. (Additionally `.dta` is saved if the Stata script is executed).

### Choice Complexity

If two lotteries are supplied as above, all 4 indices are automatically calculated. The results are ordered in the CSV as follows: Problem [Optional] | Supplied features (including columns not used in the code) | OCI | SCI | OAC | SAC |. The variable `compound` is optional.

The features for each index are the following. Please see `Section 4` for the development of the indices and appendix _Potential Complexity Features_ for details about feature definition in [Quantifying Lottery Choice Complexity](https://benjamin-enke.com/pdf/Quant_complexity.pdf). <br>
Consider a choice between two lotteries indexed by $j$ and denoted by letters $A$ and $B$. Each lottery is characterized by payout probabilities $(p_1^j,...p^j_{k_j})$ and payoff $(x_1^j,...x^j_{k_j})$ where $k_j$ denotes the number of distinct payout states of lottery $j$.

#### Features of `OCI` and `SCI`

- <u>Log excess dissimilarity (`ln_excess_dissimilarity`):</u><br>
  When $F_A(x)$ and $F_B(x)$ are the CDFs of Lottery A and B with EV(.) indicating the expected value of a lottery then _Log excess dissimilarity_ is defined as

$$log\Big( 1+\int_\mathbb{R} |F_A(x) - F_B(x)|dx - |EV(A) - EV(B)|\Big) $$

- <u>Average log payout magnitude (`ave_ln_scale__ab`):</u><br>
  $$\frac{1}{2} \Big [ log \Big(1 +  1/k_A \sum_{s=1}^{k_A} |x_s^A| \Big) + log \Big (1 + 1/k_B \sum_{s=1}^{k_B} |x_s^B|) \Big) \Big ]$$
- <u>Average log number of states (`ave_ln_nstates__ab`):</u><br>
  $$\frac{log(1 + k_A) + log(1 + k_B)}{2}$$
- <u>Frac. lotteries involving pure gains (`ave_gains__ab`)</u><br>
- <u>If one lottery in the choice is compound (according to definition [above](#Compound))</u><br>
- <u>Log Absolute expected value difference (`ln_abs_ev_diff__ab`):</u><br>
  $$log\left(1+|EV(A) - EV(B)|\right)$$

#### Features of `OAC` and `SAC`

As above but without `ln_abs_ev_diff__ab`.

### Lottery Complexity

If just one lottery is supplied the lottery complexity is calculated (OLCI/SLCI). The ordering of the output is the same as for the [Choice Complexity](#Choice-Complexity) output. The lottery-specific indices `OLCI` and `SLCI` are given by the `OLC` and `SLC` we get from pairing the lottery with its own expected value as a safe payment. For details, see Section 4.2 of the paper.

## Running the R Script

The R Script `calc_index.R` runs by using the isolated environment stored in `renv`. In order to activate this environment, you will first need to ensure that `R` and the package `renv` are installed on your machine. In the R console, navigate to the project directory and run `renv::restore()`, to automatically install all required packages by using the `renv.lock` file. Then calling `renv::activate()` should be all you need to do to be ready to run the R code. All necessary code to activate your package environment can be found at the beginning of `calc_index.R` within the first `if` brackets.

Run `calc_index.R`. Which indices will be computed is automatically determined by the supplied data ([See Section Input](#input))

## Running the Stata Script

1. Set in `global root = ""` a working directory
2. To the best of our knowledge, we included all necessary ssc install commands in the the beginning of the `calc_index.do` Stata Script
3. Run `calc_index.do`. Which indices will be computed is automatically determined by the supplied data ([See Section Input](#input)).

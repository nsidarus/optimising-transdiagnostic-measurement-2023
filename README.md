# Optimising the measurement of anxious-depressive, compulsivity and intrusive thought and social withdrawal transdiagnostic symptom dimensions

<https://psyarxiv.com/q83sh>

Alexandra Kathryn Hopkins, Claire Gillan, Jonathan Roiser, Toby Wise & Nura Sidarus

psyarxiv, 2022

## Code

Analyses for this project are R markdown files and some use a python interface, using reticulate. All packages are outlined in the initial code setup but the python environment may require some manual installations for packages if using for the first time e.g. py_install("sklearn”).

## Analysis scripts

**1. Exploratory factor analysis**

`analysis/EFA/exploratory_factor_analysis.R`

This script conducts an exploratory factor analysis on the whole dataset n = 4782 and the substudies independently. It saves the factor scores for the 3 factor model (Gillan et al. 2016, <https://elifesciences.org/articles/11305>) for the item reduction to use.

**2. Item reduction**

`analysis/itemReduction/Predict scores.ipynb`

This analysis trains a classifier to predict factor scores from the original item scores, similar to Wise & Dolan (2020) <https://www.nature.com/articles/s41467-020-17977-w>. This is done using multi-target regression (i.e. predicting scores on the 3 factors based on the individual questions). The classifier is then used to predict factor scores for the reduced item set.

**3. External validation**

`analysis/externalValidation/externalValidationRegressions.R`

This script uses data from Rouault et al. (2018) <https://www.sciencedirect.com/science/article/pii/S0006322318300295> and runs regression analyses examining relationships between the predicted factor scores for the 3 transdiagnostic factors and behavioural variables.

## Using the reduced items to predict your own factor scores

A script (`scripts/calculate_factor_scores.py`) is provided to estimate factor scores for the 3 transdiagnostic factors based on an input dataset with a reduced item set.

### Data validation

If using your own data, note that the input dataset needs to follow the same format as the original dataset.

* The exact items included does not matter as long as they are present in the original dataset (e.g., you could estimate factor scores with a subset of the original items.)
* The item names need to be identical to the original dataset (see the original dataset `data/items.csv` for the item names).
* Reverse-coded items from the SDS, STAI, BIS, AES should have their values reverse
* OCI-R & LSAS should start from 0, all other measurss should start from 1
* EAT has a specific scoring system for coding severity, such that rating 1 (always) should be coded as "3", rating 2 (usually) as "2", rating 3 (often) as "1", and ratings 4:6 (sometimes to never) as "0"
* If you have a subject identifier column this should be named "subjectID" - this is so we know which column doesn't represent an item

> **NOTE**: The script will train a new classifier on your dataset, so it doesn't matter if the items are different to those described in the paper. However, the smaller the dataset, the less accurate the factor scores will be.

### Estimating factor scores

To use the script, you will need to have a working Python environment. Assuming you have this set up, you will need to install the required packages. This can be done by running the following in the terminal/command line:

```bash
pip install numpy
pip install pandas
pip install scikit-learn
pip install git+https://github.com/the-wise-lab/FACSIMILE.git
```

You can then run the script by providing the path to your input dataset and the path to the output file. For example:

```bash
python scripts/calculate_factor_scores.py data/my_data.csv data/predicted_factor_scores.csv
```

This will take your input dataset `data/my_data.csv` and output the predicted factor scores to `data/predicted_factor_scores.csv`.

You can try this with the example dataset provided in the `scripts` folder:

```bash
python scripts/calculate_factor_scores.py scripts/example_data.csv scripts/predicted_factor_scores.csv
```

The script performs some simple validation checks:

1. Checks that the columns in the input dataset are present in the original dataset (raises an error if they are not)
2. Checks that the values in the input dataset are within the expected range (prints out a warning if they are not)
3. Compares the correlation matrices of the input dataset and the original dataset to ensure they look similar. This can help identify items that have not been reverse coded (raises an error if they are not similar)

These checks can fail with small datasets, but they are useful for larger datasets to ensure that the input data is correct. The correlation matrix check can be disabled by setting the `--ignore_correlations` flag.

```bash
python scripts/calculate_factor_scores.py data/my_data.csv data/predicted_factor_scores.csv --ignore_correlations
```

import argparse
import os

import numpy as np
import pandas as pd
from facsimile.eval import FACSIMILEOptimiser
from facsimile.plotting import plot_predictions
from facsimile.utils import train_validation_test_split
from sklearn.metrics import r2_score

if __name__ == "__main__":

    # Set up argument parser
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "data",
        type=str,
        help="Path to the data from which to calculate factor scores",
    )
    parser.add_argument(
        "output",
        type=str,
        help="Path to save the factor scores to",
    )
    parser.add_argument(
        "--n_iter",
        type=int,
        default=1000,
        help="Number of iterations to run the optimiser for",
    )
    parser.add_argument(
        "--ignore_correlations",
        action="store_true",
        help="Ignore differences in correlation matrices between the "
        "training and new data",
    )
    args = parser.parse_args()

    try:
        items_url = (
            "https://raw.githubusercontent.com/tobywise/FACSIMILE/"
            "35b15c71523055ed78d7f6c50a1e7fb875382817/data/items.csv"
        )
        factor_scores_url = (
            "https://raw.githubusercontent.com/tobywise/FACSIMILE/"
            "35b15c71523055ed78d7f6c50a1e7fb875382817/data/factor_scores.csv"
        )
        items = pd.read_csv(items_url)
        factor_scores = pd.read_csv(factor_scores_url)
    except:
        raise ValueError(
            "Could not load the training data from GitHub. "
            "Please ensure you are connected to the internet."
        )

    print("Loaded training data from GitHub")

    # Laod in our questionnaire dataframe
    questionnaire_df = pd.read_csv(
        args.data,
    )

    print("Loaded questionnaire data")

    # Check that the subjectID column is present
    if not "subjectID" in questionnaire_df.columns:
        # warn
        print(
            "subjectID column not found in the input data. If a column is "
            "present that uniquely identifies each row, please rename it to "
            "'subjectID'"
        )

    # Check that each item in questionnaire_df is present in items
    for col in questionnaire_df.columns:
        if col == "subjectID":
            continue
        assert col in items.columns, (
            f"{col} in provided data not found in original data used for "
            "training the model"
        )

    # Get the columns to include
    included_columns = [
        i for i in items.columns if i in questionnaire_df.columns
    ]

    # Select these columns in the items dataframe
    items = items[included_columns]

    # Get the number of items
    n_items = len(included_columns)

    # Make sure the dataframes have columns in the same order
    questionnaire_df = questionnaire_df[["subjectID"] + included_columns]

    # Check that the columns line up
    assert (
        questionnaire_df.columns[1:] == items.columns
    ).all(), "Columns do not match"

    # Check that the maximum values look right across both dataframes
    if not (questionnaire_df.max()[1:] == items.max()).all():
        # Find out which columns are different
        diff = questionnaire_df.max()[1:] != items.max()
        print("> WARNING")
        print("Max values do not match for the following columns:")
        print(diff[diff].index)
        print(
            "Check that the range of values for these items is correct. "
            "Note: This may simply be due to a samll sample size, where "
            "it is unlikely that the maximum value is observed for every "
            "item."
        )

    # Same for minimum values
    if not (questionnaire_df.min()[1:] == items.min()).all():
        # Find out which columns are different
        diff = questionnaire_df.min()[1:] != items.min()
        print("> WARNING")
        print("Min values do not match for the following columns:")
        print(diff[diff].index)
        print(
            "Check that the range of values for these items is correct. "
            "Note: This may simply be due to a samll sample size, where "
            "it is unlikely that the minimum value is observed for every "
            "item."
        )

    # Check that we don't have any items that might be inversely coded To check
    # this we look for item pairs that have different correlations (|r difference|
    # > 0.3) and different signs - e.g., 0.6 in the training data and -0.6 in the
    # testing data
    if not args.ignore_correlations:
        assert (
            ~(
                ~np.isclose(
                    questionnaire_df.iloc[:, 1:].corr(), items.corr(), atol=0.3
                )
                & (
                    np.sign(questionnaire_df.iloc[:, 1:].corr())
                    != np.sign(items.corr())
                )
            )
            .any()
            .any()
        ), (
            "The correlation matrix of the provided data does not match the "
            "correlation matrix of the original data used for training the model. "
            "Some correlations have inverse signs, which may indicate that reverse "
            "coded items have not had their scores reversed.\n"
            "If you have a small dataset, this error may occur incorrectly, and you "
            "can ignore it by passing the --ignore_correlations flag."
        )

    print("Validated input data")

    # Scale factor scores to ahve mean 0 and std 1
    factor_scores.iloc[:, 2:] = (
        factor_scores.iloc[:, 2:] - factor_scores.iloc[:, 2:].mean()
    ) / factor_scores.iloc[:, 2:].std()

    X_train, X_val, X_test, y_train, y_val, y_test = (
        train_validation_test_split(
            items,  # Drop the first two columns, which are just IDs
            factor_scores.iloc[
                :, 2:
            ],  # Drop the first two columns, which are just IDs
            train_size=0.6,
            test_size=0.2,
            val_size=0.2,
        )
    )

    # Initialise the optimiser
    optimiser = FACSIMILEOptimiser(n_iter=args.n_iter, n_jobs=-1)

    # Fit
    optimiser.fit(
        X_train, y_train, X_val, y_val, factor_names=["AD", "Compul", "SW"]
    )

    print("Optimisation complete")

    # Get the best classifier
    best_clf = optimiser.get_best_classifier(metric="min_r2")

    # Fit
    best_clf.fit(X_train, y_train)

    # Get predictions
    y_pred = best_clf.predict(X_test)

    # Get the R^2 scores
    r2s = r2_score(y_test, y_pred, multioutput="raw_values")
    print("R^2 scores for the trained model:")
    print(f"AD: {r2s[0]:.2f}")
    print(f"Compul: {r2s[1]:.2f}")
    print(f"SW: {r2s[2]:.2f}")

    # Retrain classifier on all data
    best_clf.fit(items, factor_scores.iloc[:, 2:])

    # Predict scores
    y_pred = best_clf.predict(questionnaire_df.iloc[:, 1:])

    # Add subjectID to y_pred
    if "subjectID" in questionnaire_df.columns:
        y_pred["subjectID"] = questionnaire_df["subjectID"]

    # Save the factor scores
    y_pred.to_csv(args.output, index=False)

    print(f"Factor scores saved to {args.output}")
    print("Finished")

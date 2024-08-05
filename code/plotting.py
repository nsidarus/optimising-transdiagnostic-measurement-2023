import matplotlib.pyplot as plt
import colormaps as cmaps
import numpy as np
from typing import List
import pandas as pd


def plot_regression_results(dfs: List[pd.DataFrame], titles: List[str] = None):
    """Plot the results of a regression analysis.

    Args:
        dfs (List[pd.DataFrame]): A list of DataFrames each containing the results of a regression analysis.
        titles (List[str], optional): A list of titles for each subplot. Defaults to None.
    """
    # Set the number of subplots to the number of DataFrames
    n = len(dfs)

    # Create the necessary number of subplots
    fig, axs = plt.subplots(n, 1, figsize=(5.5, 2.3 * n), dpi=100)
    if (
        n == 1
    ):  # Convert axs to a list of a single axis object for consistent indexing
        axs = [axs]

    # If no titles are provided, create a list of empty strings
    if titles is None:
        titles = [""] * n

    for ax, df in zip(axs, dfs):
        unique_labels = df["Label"].unique()
        unique_types = df["Type"].unique()

        # Define a colormap
        colors = cmaps.bold[2:5].colors  # Replace with your preferred colormap

        # Loop over each unique label in the DataFrame
        for idx, label in enumerate(unique_labels):
            sub_df = df[df["Label"] == label]

            for j, (index, row) in enumerate(sub_df.iterrows()):
                bar_value = row["Estimate"]
                error = row["StdError"]
                star_y = (
                    bar_value + error + 0.01
                )  # 0.01 is added for a small gap between the error bar and the star
                star_y = (
                    max(star_y, 0.01) if bar_value < 0 else star_y
                )  # Ensure the star is above the bar

                rect = ax.bar(
                    idx + j * 0.2,
                    bar_value,
                    0.2,
                    yerr=error,
                    label=f"{row['Type']}",
                    color=colors[j],
                    edgecolor="black",
                    capsize=2,
                )

                # Add significance star above the bar
                p_value = row["Pr...t.."]
                if p_value < 0.05:
                    star = (
                        "*"
                        if p_value >= 0.01
                        else "**" if p_value >= 0.001 else "***"
                    )
                    ax.text(
                        idx + j * 0.2, star_y, star, ha="center", va="bottom"
                    )

        # Customize the axes and labels
        ax.axhline(0, color="black", linewidth=0.8)
        ax.set_xticks(np.arange(len(unique_labels)) + 0.2)  # Center the ticks
        ax.set_xticklabels(
            [label.replace(" ", "\n") for label in unique_labels]
        )
        ax.set_ylabel(r"$\beta$" + " ± SE")

        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)

        # Create a custom legend
        from matplotlib.lines import Line2D

        legend_elements = [
            Line2D(
                [0], [0], color=colors[i], lw=2, label=type_.replace(" ", "\n")
            )
            for i, type_ in enumerate(unique_types)
        ]
        ax.legend(
            handles=legend_elements, loc="center left", bbox_to_anchor=(1, 0.5)
        )

    # Add titles to each subplot
    for ax, title in zip(axs, titles):
        # Make sure title is a little bit above the top of the plot
        ax.set_title(title, pad=15)

    plt.tight_layout()
    plt.show()

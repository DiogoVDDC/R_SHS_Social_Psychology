# README

## Description

This projects is associated with the study looking into influence of the gender towards pro environmental behavior.

The R script provided loads a CSV file named `Group_EF_data.csv`. This corresponds to the data of the survey. We make use of the data related to the attitude and intentions columns. Refer to the specific scripts and comments to learn more.

## Packages Used

- **readr**: used to read the CSV data.
- **dplyr**: used for data manipulation and filtering.
- **ggplot2**: used for creating the histogram.

## Environment

We used VS Code and the VSCode plugin to run the script. Learn more about how to set this up at: https://code.visualstudio.com/docs/languages/r

## Usage

1. Install the necessary R packages using `install.packages(c("readr", "dplyr", "ggplot2"))` if you haven't done it yet.
2. Ensure that `Group_EF_data.csv` is in the working directory (specifically, in the `./` directory relative to where the script is located).
3. Run the script in the root directory. As the the Group_EF_data.csv is referenced using relative paths.

## Outputs

Each script outputs a plot. The plots is saved to the temp folder of your computer and not in the current directory.

# shinyBio

## Description

This repository contains an R package designed for educational purposes.

The included dashboard application, written using the *shiny* package, demonstrates the underlying principles of (generalized) linear models:

* It generates simulated data.
* It displays marginal plots to visualize relationships between variables.
* It fits the most suitable model to the data.
* It presents a summary of the fitted model, including relevant statistics and measures.
* It plots the fitted data overlaid with the model.
* It provides diagnostic plots to assess the model's assumptions.

 Furthermore, the package offers insights into how the fitted data and diagnostic plots appear when the model's assumptions are not satisfied. This feature allows users to gain an understanding of the potential consequences and implications of violating these assumptions

To install this package on your computer, you have to install the {devtools} package, load it and then you can isntall the package from the github repository.

Follow these steps:

install.packages(devtools);
library(devtools);
install_github("zurich-data-scientists/shinyBio");
shinyBio::run_app()


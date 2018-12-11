# SIEO4150-FinalProject

## Overview
This is the accompanying implementation in R for the "Global Education Stock Returns" final project. This implementation requires interactive mode as it takes user input -- per the instructions, if a user wants to do a single-stock analysis, two-stock analysis or just run the preconfigured global educational sector analysis, they have to select the appropriate option.

The first prompt appears as such:
```
Please choose an option: 
	1) Single-stock Analysis 
	2) Two-stock Analysis 
	3) Pre-configured Educational Analysis
```

## Single-Stock Analysis
In this function, a user is able to enter a single stock symbol as well as a lookback (start) date and return the histogram for the stock, the normal probability plot to test for normality, the confidence interval of the mean and the variance, the log returns and regression of the log returns of the stock within the lookback window and the residuals.

## Two-Stock Analysis
In this function, a user is able to conduct the t-test to determine population mean equivalence among two distinct stocks. I also run linear regression to test whether there is a significant difference in the log returns provided by the user.

## How to Run
1. Open RStudio IDE, open the WorkingSpace.R file within the SIEO4150-FinalProject folder.
2. Ensure the following packages are loaded into the RStudio environment:
	- arms
	- xts
	- zoo
	- quantmod
	- randtests
3. Source the script into the console environment within RStudio. 
4. Follow the prompts from the script.


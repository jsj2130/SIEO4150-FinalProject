##### Student Name/UNI: Jisha Jacob (jsj2130)
##### Start Date: November 2, 2018
##### Due Date: December 10, 2018

require('quantmod')
require('randtests')
require('arm')

############################################################
# Source Stock Data for Education Stocks                   #
############################################################

# Region-specific educational stock symbols
chinese_tickers = c('TAL','EDU')
usa_tickers = c('ATGE','BFAM')
brazil_tickers = c('KROTY','ESTC3.SA')
india_tickers = c('CLEDUCATE.BO','CAREERP.NS')

# Global educational stock symbols
global_tickers = c('TAL','EDU','ATGE','BFAM','KROTY','ESTC3.SA','CLEDUCATE.BO','CAREERP.NS')

############################################################
# One-Stock Analysis                                	   #
#   - Display Histogram for data by symbol                 #
#   - Display normal probability plot to test normality    #
#   - Create confidence intervals for means and variances  #
#   - Display Graph of Log Returns                         #
############################################################

one_stock_analysis <- function(single_ticker, start_date, end_date) {
	# Create an Environment
	single_stock_data <- new.env()
	start_date <- as.Date(start_date)

	# For a single stock, get data
	getSymbols(single_ticker, src = 'yahoo', from = start_date, to = end_date, env = single_stock_data)

	str = sprintf("Performing analysis for %s", single_ticker)
	print(str)

	# Get the Log Returns for the stock
	if (length(nrow(single_stock_data[[single_ticker]])) != 0) {
		nrows <- nrow(single_stock_data[[single_ticker]])
		single_stock_data[[single_ticker]]$logreturns <- 0
		single_stock = coredata(single_stock_data[[single_ticker]])
		for(i in 2:nrows) {
		  single_stock_data[[single_ticker]]$logreturns[i] <- log(single_stock[,4][i]) - log(single_stock[,4][i-1])
		}
	}

	# Display Histogram
	print("Displaying Histogram...")
	chartSeries(single_stock_data[[single_ticker]], name = single_ticker)
	Sys.sleep(3)

	# Display Normal Probability Plot
	print("Displaying Normal Probability Plot...")
	single_stock = coredata(single_stock_data[[single_ticker]])
	qqnorm(single_stock[,7])
    qqline(single_stock[,7])
    Sys.sleep(3)

    # Test for Randomness (Runs Test)
    print("Performing Runs Test for Randomness...")
    randomness_results <- runs.test(single_stock[,7])
	if (randomness_results[2]$p.value > 0.05) {
		str = sprintf("The Sample is Random, with a p-value of %f", randomness_results[2]$p.value)
		print(str)
	} else {
		str = sprintf("The Sample is Non-Random, with a p-value of %f", randomness_results[2]$p.value)
		print(str)
	}

	# Calculate the Sample Mean
	sample_means <- mean(single_stock[,7], na.rm = TRUE)
	str = sprintf("Sample Means: %f",sample_means)
	print(str)

	# Calculate the Sample Standard Deviation
	sample_stddev <- sd(single_stock[,7], na.rm = TRUE)
	str = sprintf("Sample Std Dev: %f",sample_stddev)
	print(str)

	# Calculate the Sample Variance
	sample_var <- var(single_stock[,7], na.rm = TRUE)
	str = sprintf("Sample Variance: %f",sample_stddev)
	print(str)
	
	# Calculate the Sample Error
	sample_size <- nrow(single_stock_data[[single_ticker]])
	error <- qt(0.975, df = sample_size - 1) * sample_stddev / sqrt(sample_size)
	str = sprintf("Sample Error: %f",error)
	print(str)

	# Create a 95 Percent Confidence Interval for Means
	ci_min <- sample_means - error
	ci_max <- sample_means + error
	str = sprintf("95%% Confidence Interval for Means: (%f,%f)",ci_min,ci_max)
	print(str)

	# Create a 95 Percent Confidence Interval for Variance
	ci_vmin <- sample_var * (nrows - 1) / qchisq(0.05/2, (nrows - 1), lower.tail = FALSE)
	ci_vmax <- sample_var * (nrows - 1) / qchisq(1 - 0.05/2, (nrows - 1), lower.tail = FALSE)
	str = sprintf("95%% Confidence Interval for Variance: (%f,%f)",ci_vmin,ci_vmax)
	print(str)

	# Linear Regression
	print("Linear Regression Analysis of Log-Returns")
	index <- index(single_stock)
	lm_logreturns <- lm(single_stock[,7] ~ index)
	print(summary(lm_logreturns))

	y.hat <- fitted(lm_logreturns)
	res_logreturns <- resid(lm_logreturns)
	sigma <- sigma.hat(lm_logreturns)
	residual.plot(y.hat, res_logreturns, sigma)

}

############################################################
# Two-Stock Analysis                                       #
#   - Display Histogram for data by symbol                 #
#   - Display normal probability plot to test normality    #
#   - Create confidence intervals for means and variances  #
#   - Display Graph of Log Returns                         #
############################################################

two_stock_analysis <- function(two_tickers, start_date, end_date) {
	# Create an Environment
	two_stock_data <- new.env()

	# For a two-stock analysis, get data
	getSymbols(two_tickers, src = 'yahoo', from = start_date, env = two_stock_data)

	# Get the Log Returns for each stock
	for(n in names(two_stock_data)) {
	  if (n != 'NULL' && length(nrow(two_stock_data[[n]])) != 0) {
	    nrows <- nrow(two_stock_data[[n]])
	    two_stock_data[[n]]$logreturns <- 0
	    single_stock = coredata(two_stock_data[[n]])
	    for(i in 2:nrows) {
	      two_stock_data[[n]]$logreturns[i] <- log(single_stock[,4][i]) - log(single_stock[,4][i-1])
	    }
	  }
	}

	print("Attempting T Test...")
	control_stock = coredata(two_stock_data[[two_tickers[1]]])
	batch_one <- control_stock[,7]

	test_stock = coredata(two_stock_data[[two_tickers[2]]])
	batch_two <- test_stock[,7]

	twostock_tresults <- t.test(batch_one, batch_two, var.equal=FALSE)
	print(twostock_tresults)

	tslm_logreturns <- lm(batch_one ~ batch_two)
	print(summary(tslm_logreturns))
}

edstock_analysis <- function() {
	for(n in global_tickers) {
		if (n != 'NULL') {
			one_stock_analysis(n, start_date = '2016-09-02', end_date = '2018-09-01')
		}
	}
}

# User Input for type of analysis to run
print("Please choose an option: 1) Single-stock Analysis 2) Two-stock Analysis 3) Pre-configured Educational Analysis")
user.option <- readline()

# Single-Stock Analysis
if (user.option == 1) {
	# User Inputs for Stock Symbol and Start Date
	user.single_ticker <- readline(prompt="Please enter the stock symbol: ")
	user.start_date <- readline(prompt="Please enter the start date you'd like (Format: YYYY-MM-DD): ")

	# Check Start Date is Valid
	tryCatch({
			user.start_date <- as.Date(user.start_date)
		}, error = function(err) {
			stop("Invalid start date: Input must be valid date. Exiting")
		}, finally = {

			# Check Stock Symbol is Valid
			if (class(user.single_ticker) == "character" && class(user.start_date) == "Date") {
				end_date = Sys.Date()
				one_stock_analysis(user.single_ticker, user.start_date, end_date)
			} else {
				stop("Invalid stock symbol: Input must be valid stock symbol. Exiting.")
			}
		}
	)
} else if (user.option == 2) {
	user.two_tickers <- readline(prompt="Please enter two stock symbols, comma-separated: ")
	user.start_date <- readline(prompt="Please enter the start date you'd like (Format: YYYY-MM-DD): ")

	tryCatch({
			user.start_date <- as.Date(user.start_date)
		}, error = function(err) {
			stop("Invalid start date: Input must be valid date. Exiting")
		}, finally = {
			if (class(user.two_tickers) == "character" && class(user.start_date) == "Date") {
				stock_list <- strsplit(user.two_tickers, ",")[[1]]
				stock_list <- gsub(" ", "", stock_list, fixed = TRUE)

				end_date = Sys.Date()
				two_stock_analysis(stock_list, user.start_date, end_date)
			} else {
				stop("Invalid stock symbols: Input must be two valid, comma-separated stock symbols. Exiting.")
			}
		}
	) 
} else if (user.option == 3) {
	edstock_analysis()
} else {
	stop("Invalid selection: Input must be 1, 2 or 3. Exiting.")
}
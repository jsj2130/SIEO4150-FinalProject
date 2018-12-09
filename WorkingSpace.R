##### Student Name/UNI: Jisha Jacob (jsj2130)
##### Start Date: November 2, 2018
##### Due Date: December 10, 2018

require('quantmod')

############################################################
# Source Stock Data for Education Stocks                   #
############################################################

edu_stock_analysis <- function() {
	# Region-specific educational stock symbols
	chinese_tickers = c('TAL','EDU')
	usa_tickers = c('ATGE','BFAM')
	brazil_tickers = c('KROTY','ESTC3.SA')
	india_tickers = c('CLEDUCATE.BO','CAREERP.NS')

	# Global educational stock symbols
	global_tickers = c('TAL','EDU','ATGE','BFAM','KROTY','ESTC3.SA','CLEDUCATE.BO','CAREERP.NS')


	# Create an Environment
	stock_data <- new.env()

	# For each stock in list, read csv file and save as xts object
	getSymbols(global_tickers, src = 'yahoo', from = '2016-09-02', to = '2018-09-01', env = stock_data)

	# Display a histogram for data by symbol
	for(n in names(stock_data)) {
	  if (n != 'NULL' && length(nrow(stock_data[[n]])) != 0) {
	    chartSeries(stock_data[[n]], name = n)
	    Sys.sleep(5)
	  }
	}

	# Display a normal probability plot to test normality
	for(n in names(stock_data)) {
	  if (n != 'NULL' && length(nrow(stock_data[[n]])) != 0) {
	    single_stock = coredata(stock_data[[n]])
	    qqnorm(single_stock[,4])
	    qqline(single_stock[,4])
	    Sys.sleep(5)
	  }
	}
}

############################################################
# One-Stock Analysis                                	   #
#   - Display Histogram for data by symbol                 #
#   - Display normal probability plot to test normality    #
#   - Create confidence intervals for means and variances  #
#   - Display Graph of Log Returns                         #
############################################################

one_stock_analysis <- function(single_ticker, start_date) {
	# Create an Environment
	single_stock_data <- new.env()
	start_date <- as.Date(start_date)

	# For a single stock, get data
	getSymbols(single_ticker, src = 'yahoo', from = start_date, env = single_stock_data)

	# Display Histogram
	print("Displaying Histogram...")
	chartSeries(single_stock_data[[single_ticker]], name = single_ticker)
	Sys.sleep(5)

	# Display Normal Probability Plot
	single_stock = coredata(single_stock_data[[single_ticker]])
	qqnorm(single_stock[,4])
    qqline(single_stock[,4])	

	# Create a Confidence Interval for Means

	# Create a Confidence Interval for Variances

	# Get the Log Returns for the stock
	  if (length(nrow(single_stock_data[[single_ticker]])) != 0) {
	    nrows <- nrow(single_stock_data[[single_ticker]])
	    single_stock_data[[single_ticker]]$logreturns <- 0
	    single_stock = coredata(single_stock_data[[single_ticker]])
	    for(i in 2:nrows) {
	      single_stock_data[[single_ticker]]$logreturns[i] <- log(single_stock[,4][i]) - log(single_stock[,4][i-1])
	    }
	  }
	}
}

############################################################
# Two-Stock Analysis                                       #
#   - Display Histogram for data by symbol                 #
#   - Display normal probability plot to test normality    #
#   - Create confidence intervals for means and variances  #
#   - Display Graph of Log Returns                         #
############################################################

two_stock_analysis <- function(two_tickers, start_date) {
	# Create an Environment
	two_stock_data <- new.env()

	# For a two-stock analysis, get data
	getSymbols(two_tickers, src = 'yahoo', from = start_date, env = two_stock_data)

	# Display Histogram
	for(n in names(two_stock_data)) {
	  if (n != 'NULL' && length(nrow(two_stock_data[[n]])) != 0) {
		print("Displaying Histogram...")
		chartSeries(two_stock_data[[n]], name = n)
		Sys.sleep(5)
	  }
	}

	# Display Normal Probability Plot
	for(n in names(two_stock_data)) {
	  if (n != 'NULL' && length(nrow(two_stock_data[[n]])) != 0) {
		print("Displaying Normal Probability of %s", n)	
		first_stock = coredata(two_stock_data[[n]])
		qqnorm(first_stock[,4])
	    qqline(first_stock[,4])
	    Sys.sleep(5)
	  }
	}

	# Create a Confidence Interval for Means

	# Create a Confidence Interval for Variances

	# Get the Log Returns for each stock
	for(n in names(two_stock_data)) {
	  if (n != 'NULL' && length(nrow(two_stock_data[[n]])) != 0) {
	    nrows <- nrow(single_stock_data[[n]])
	    two_stock_data[[n]]$logreturns <- 0
	    single_stock = coredata(two_stock_data[[n]])
	    for(i in 2:nrows) {
	      two_stock_data[[n]]$logreturns[i] <- log(single_stock[,4][i]) - log(single_stock[,4][i-1])
	    }
	    print(two_stock_data[[n]]$logreturns)
	  }
	}
}

# User Input for type of analysis to run
interactive()==TRUE
print("Please choose an option: 1) Single-stock Analysis 2) Two-stock Analysis 3) Pre-configured Educational Analysis")
user.option <- readline()

# Single-Stock Analysis
if (user.option == 1) {
	# User Inputs for Stock Symbol and Start Date
	user.single_ticker <- readline(prompt="Please enter the stock symbol: ")
	user.start_date <- readline(prompt="Please enter the start date you'd like (Format: YYYY-MM-DD): ")

	# Check Start Date is Valid
	tryCatch({as.Date(user.start_date)
		}, error = function(err) {
			stop("Invalid start date: Input must be valid date. Exiting")
		}, finally = {

			# Check Stock Symbol is Valid
			if (class(user.single_ticker) == "character") {

				# Run Single-Stock Analysis
				one_stock_analysis(user.single_ticker, user.start_date)
			} else {
				stop("Invalid stock symbol: Input must be valid stock symbol. Exiting.")
			}
		}
	)
} else if (user.option == 2) {
	user.two_tickers <- readline(prompt="Please enter two stock symbols, comma-separated: ")
	user.start_date <- readline(prompt="Please enter the start date you'd like (Format: YYYY-MM-DD): ")

	tryCatch({as.Date(user.start_date)
		}, error = function(err) {
			stop("Invalid start date: Input must be valid date. Exiting")
		}, finally = {
			if (class(user.two_tickers) == "character" && class() == "date") {
				one_stock_analysis(user.two_tickers, user.start_date)
			} else {
				stop("Invalid stock symbol: Input must be valid stock symbol. Exiting.")
			}
		}
	) 
} else if (user.option == 3) {
	edu_stock_analysis()
} else {
	stop("Invalid selection: Input must be 1, 2 or 3. Exiting.")
}
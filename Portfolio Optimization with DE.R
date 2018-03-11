
require(DEoptim)
library('quantmod')
library('xts')
library('zoo')
library('TTR')

get_prices<-function(tickers,from,to){
  
  
  # Load these prices into memory
  getSymbols(tickers, from=from, to=to)
  ClosePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
  
  colnames(ClosePrices) <- tickers
  
  return(ClosePrices)
  
}

tickers<-c("QQQ","HYG","UUP","SPY","TLT","XLF","DIA")

px <- NULL
for(ticker in tickers) {
  # Extract the adjusted close price vector
  px <- cbind(px, get_prices(ticker,"2014-07-13","2018-03-09"))
}

returns <- apply(px, 2, function(x) diff(log(x)))

compute_drawdown <- function(x, returns_default = TRUE,
                             geometric = TRUE) {
  # x = Vector of raw pnl or returns
  # If returns_default = FALSE, the geometric
  # argument is ignored and the pnl is used.
  # Output = the maximum drawdown
  if(returns_default) {
    # Cumulative return calculation
    if(geometric) {
      cumulative_return <- cumprod(1 + x)
    } else {
      cumulative_return <- 1 + cumsum(x)
    }
    max_cumulative_return <- cummax(c(1, cumulative_return))[-1]
    drawdown <- -(cumulative_return / max_cumulative_return - 1)
  } else {
    # PnL vector is used
    cumulative_pnl <- c(0, cumsum(x))
    drawdown <- cummax(cumulative_pnl) - cumulative_pnl
    drawdown <- drawdown[-1]
  }
  # Drawdown vector for either pnl or returns
  return(drawdown)
}


obj_max_drawdown <- function(w, r_matrix, small_weight) {
  # w is the weight of every stock
  # r_matrix is the returns matrix of all stocks
  
  # Portfolio return
  portfolio_return <- r_matrix %*% w
  
  # Max drawdown
  drawdown_penalty <- max(compute_drawdown(portfolio_return))
  
  # Create penalty component for sum of weights
  weight_penalty <- 100 * (1 - sum(w)) ^ 2
  
  # Create a penalty component for negative weights
  negative_penalty <- -sum(w[w < 0])
  
  # Create penalty component for small weights
  small_weight_penalty <- 100 * sum(w[w < small_weight])
  
  # Objective function to minimize
  obj <- drawdown_penalty + weight_penalty +
    negative_penalty + small_weight_penalty
  return(obj)
}


small_weight_value <- 0.02

# Specify lower and upper bounds for the weights
lower <- rep(0, ncol(returns))
upper <- rep(1, ncol(returns))

optim_result <- DEoptim(obj_max_drawdown, lower, upper,
                        control = list(NP = 400, itermax = 300, F = 0.25, CR = 0.75),
                        returns, small_weight_value)

weights <- optim_result$optim$bestmem

sum(weights)


weights <- weights / sum(weights)

# Equally weighted portfolio
equal_weights <- rep(1 / 7, 7)
equal_portfolio <- returns %*% equal_weights

equal_portfolio_cumprod <- cumprod(1 + equal_portfolio)

# Optimal max drawdown portfolio
optimized_portfolio <- returns %*% weights
drawdown_portfolio_cumprod <- cumprod(1 + optimized_portfolio)

main_title <- "Equal vs. Optimized Weights"
plot(drawdown_portfolio_cumprod, type = 'l', xaxt = 'n',
     main = main_title, xlab = "", ylab = "cumprod(1 + r)")
lines(equal_portfolio_cumprod, lty = 3)
grid(col = 'black')

# Set x-axis labels
label_location <- seq(1, length(drawdown_portfolio_cumprod),
                      by = 90)
labels <- rownames(returns_matrix)[label_location]
axis(side = 1, at = label_location, labels = labels,
     las = 2, cex.axis= 0.8)

# Equal weighted
max(compute_drawdown(equal_portfolio))


# Optimized for the smallest max drawdown
max(compute_drawdown(optimized_portfolio))

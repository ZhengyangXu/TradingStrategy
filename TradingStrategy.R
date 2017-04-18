## Använda R för att skapa en bra aktiehandelsstrategi
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

symbols <- c('SANION.ST')
start <- as.Date("2014-01-01")
end <- Sys.Date() # Idag
stock <- getSymbols(symbols, src = "yahoo", from = start, to = end, auto.assign = FALSE)
class(stock) # Förutsatt att man laddat Saniona-data (SANION.ST)

### Undersök datat

#### Kika på första raderna

head(stock)
tail(stock)

### Plotta
names(stock)
plot(stock[, "SANION.ST.Close"], main = "Aktiekurs") # Plottar sista avlut för alla rader. Fult men kolumnen heter SANION.ST.Close 
# Borde gå att göra nåt åt
candleChart(stock, up.col = "black", dn.col = "red", theme = "white")


### Beräkna avkastning istället sedan början av perioden

# Get me my beloved pipe operator!
if (!require("magrittr")) {
  install.packages("magrittr")
  library(magrittr)
}

stock_return = apply(stock, 1, function(x) {x / stock[1,]}) %>% 
  t %>% as.xts

head(stock_return)
### Och plotta
candleChart(stock_return, up.col = "black", dn.col = "red", theme = "white")

## Moving averages
### Zoomar in 2016 och senare för att se tydligare
candleChart(stock_return, up.col = "black", dn.col = "red", theme = "white", subset="2016/")

addSMA(n = c(20, 50, 200))

## Taktik
## Köp när korta SMA går över långa och sälj när den går tillbaka under

if (!require("TTR")) {
  install.packages("TTR")
  library(TTR)
}
if (!require("quantstrat")) {
  install.packages("quantstrat", repos="http://R-Forge.R-project.org")
  library(quantstrat)
}


# Måste installera Rtools. https://cran.r-project.org/bin/windows/Rtools/

install.packages('devtools')
library(devtools)
install_github('IlyaKipnis/IKTrading')
library(IKTrading)

if (!require("IKTrading")) {
  if (!require("devtools")) {
    install.packages("devtools")
  }
  library(devtools)
  install_github("IKTrading", username = "IlyaKipnis")
  library(IKTrading)
}
library(quantmod)



stock_sma_10 <- SMA(
  Cl(stock),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 10     # The number of days in the moving average window
)

stock_sma_20 <- SMA(
  Cl(stock),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 20     # The number of days in the moving average window
)

stock_sma_27 <- SMA(
  Cl(stock),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 27     # The number of days in the moving average window
)

stock_sma_30 <- SMA(
  Cl(stock),  # The closing price of AAPL, obtained by quantmod's Cl() function
  n = 30     # The number of days in the moving average window
)

stock_sma_50 <- SMA(
  Cl(stock),
  n = 50
)

stock_sma_200 <- SMA(
  Cl(stock),
  n = 200
)

candleChart(SANION.ST, up.col = "black", dn.col = "red", theme = "white")
zoomChart("2016/")
addTA(stock_sma_10, on = 1, col = "red")  # on = 1 plots the SMA with price
addTA(stock_sma_20, on = 1, col = "pink")  # on = 1 plots the SMA with price
addTA(stock_sma_30, on = 1, col = "yellow")  # on = 1 plots the SMA with price
addTA(stock_sma_50, on = 1, col = "blue")
addTA(stock_sma_200, on = 1, col = "green")


stock_trade <- stock
stock_trade$`10d` <- stock_sma_10
stock_trade$`20d` <- stock_sma_20
stock_trade$`27d` <- stock_sma_27
stock_trade$`50d` <- stock_sma_50

## Ta fram om fast moving average är större än slow moving average. -> bullish
regime_val <- sigComparison("", data = stock_trade, columns = c("10d", "27d"), relationship = "gt") -
              sigComparison("", data = stock_trade, columns = c("10d", "27d"), relationship = "lt")

## Plotta
plot(regime_val["2016"], main = "Regime", ylim = c(-2, 2))
plot(regime_val, main = "Regime", ylim = c(-2, 2))

candleChart(stock, up.col = "black", dn.col = "red", theme = "white", subset = "2016/")
addTA(regime_val, col = "blue", yrange = c(-2, 2))
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(10, 27), on = 1, col = c("red", "blue"))
zoomChart("2016/")


## Kolla antal bullish och bearish
table(as.vector(regime_val))


## Använd ovanstående för att skapa en trading signal

sig <- diff(regime_val) / 2
plot(sig, main = "Signal", ylim = c(-2, 2))
table(sig)
## Buy prices
Cl(stock)[which(sig == 1)]
Cl(stock)[which(sig == -1)]

cbind(Cl(stock)[which(sig == 1)], Cl(stock)[which(sig == -1)])

## Om antal sälj < antal köp -> fejka sälj idag för att nästa uträkning ska funka
sig[dim(sig)[1],] <- -1


## Beräkna vinst om lika många sälj som köp
# as.vector(Cl(stock)[sig == 1])[-1] - Cl(stock)[sig == -1][-table(sig)[["1"]]]


### Utdelningar och splittar kan förstöra. Kör istället på adjustedOHLC

## SANIONA

getSymbols(SANION.ST, src = "yahoo", from = start, to = end, adjust=T)

candleChart(adjustOHLC(SANION.ST), up.col = "black", dn.col = "red", theme = "white")
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(10, 27), on = 1, col = c("red", "blue"))


### Låtsas att vi har en portfölj på 10000 kr

#### Sätt parametrar och välj aktie
rm(list = ls(.blotter), envir = .blotter)  # Clear blotter environment
currency("SEK")  # Currency being used

Sys.setenv(TZ = "UTC")  # Allows quantstrat to use timestamps
SANION.ST_adj <- adjustOHLC(SANION.ST)
stock("SANION.ST_adj", currency = "SEK", multiplier = 1)
initDate <- "2014-01-01"

## Initiera strategi, portfölj och konto
strategy_st <- portfolio_st <- account_st <- "SMAC-10-27"  # Names of objects
rm.strat(portfolio_st)  # Need to remove portfolio from blotter env
rm.strat(strategy_st)   # Ensure no strategy by this name exists either
initPortf(portfolio_st, symbols = "SANION.ST_adj",  # This is a simple portfolio
          # trading SANIONA only
          initDate = initDate, currency = "SEK")
initAcct(account_st, portfolios = portfolio_st,  # Uses only one portfolio
        initDate = initDate, currency = "SEK",
        initEq = 10000)  # Start with a 10000 kr

initOrders(portfolio_st, store = TRUE) # Initialize the order container; will
# contain all orders made by strategy

strategy(strategy_st, store = TRUE)  # store = TRUE tells function to store in
# the .strategy environment

# Now define trading rules
# Indicators are used to construct signals
add.indicator(strategy = strategy_st, name = "SMA",     # SMA is a function
              arguments = list(x = quote(Cl(mktdata)),  # args of SMA
                               n = 10),
              label = "fastMA")

add.indicator(strategy = strategy_st, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 27),
              label = "slowMA")

# Next comes trading signals
add.signal(strategy = strategy_st, name = "sigComparison",  # Remember me?
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "gt"),
           label = "bull")

add.signal(strategy = strategy_st, name = "sigComparison",
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "lt"),
           label = "bear")

# Finally, rules that generate trades
add.rule(strategy = strategy_st, name = "ruleSignal",  # Almost always this one
         arguments = list(sigcol = "bull",  # Signal (see above) that triggers
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          osFUN = osMaxDollar,
                          # The next parameter, which is a parameter passed to
                          # osMaxDollar, will ensure that trades are about 10%
                          # of portfolio equity
                          maxSize = quote(floor(getEndEq(account_st,
                                                         Date = timestamp) * 1)),
                          tradeSize = quote(floor(getEndEq(account_st,
                                                           Date = timestamp) * 1))),
                          type = "enter", path.dep = TRUE, label = "buy")

add.rule(strategy = strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "bear",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
                          type = "exit", path.dep = TRUE, label = "sell")

applyStrategy(strategy_st, portfolios = portfolio_st)

updatePortf(portfolio_st)

dateRange <- time(getPortfolio(portfolio_st)$summary)[-1]
updateAcct(portfolio_st, dateRange)
updateEndEq(account_st)
# Plocka ut statistik
tStats <- tradeStats(Portfolios = portfolio_st, use="trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[, -c(1,2)])))

final_acct <- getAccount(account_st)
plot(final_acct$summary$End.Eq["2014/2017"], main = "Portfolio Equity")



## IMMUNICUM

getSymbols('IMMU.ST', src = "yahoo", from = start, to = end, adjust=T)

candleChart(adjustOHLC(IMMU.ST), up.col = "black", dn.col = "red", theme = "white")
addLines(h = 0, col = "black", on = 3)
addSMA(n = c(10, 27), on = 1, col = c("red", "blue"))


### Låtsas att vi har en portfölj på 10000 kr

#### Sätt parametrar och välj aktie
rm(list = ls(.blotter), envir = .blotter)  # Clear blotter environment
currency("SEK")  # Currency being used

Sys.setenv(TZ = "UTC")  # Allows quantstrat to use timestamps
IMMU.ST_adj <- adjustOHLC(IMMU.ST)
stock("IMMU.ST_adj", currency = "SEK", multiplier = 1)
initDate <- "2014-01-01"

## Initiera strategi, portfölj och konto
strategy_st <- portfolio_st <- account_st <- "SMAC-10-27"  # Names of objects
rm.strat(portfolio_st)  # Need to remove portfolio from blotter env
rm.strat(strategy_st)   # Ensure no strategy by this name exists either
initPortf(portfolio_st, symbols = "IMMU.ST_adj",  # This is a simple portfolio
          # trading SANIONA only
          initDate = initDate, currency = "SEK")
initAcct(account_st, portfolios = portfolio_st,  # Uses only one portfolio
         initDate = initDate, currency = "SEK",
         initEq = 10000)  # Start with a 10000 kr

initOrders(portfolio_st, store = TRUE) # Initialize the order container; will
# contain all orders made by strategy

strategy(strategy_st, store = TRUE)  # store = TRUE tells function to store in
# the .strategy environment

# Now define trading rules
# Indicators are used to construct signals
add.indicator(strategy = strategy_st, name = "SMA",     # SMA is a function
              arguments = list(x = quote(Cl(mktdata)),  # args of SMA
                               n = 10),
              label = "fastMA")

add.indicator(strategy = strategy_st, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 27),
              label = "slowMA")

# Next comes trading signals
add.signal(strategy = strategy_st, name = "sigComparison",  # Remember me?
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "gt"),
           label = "bull")

add.signal(strategy = strategy_st, name = "sigComparison",
           arguments = list(columns = c("fastMA", "slowMA"),
                            relationship = "lt"),
           label = "bear")

# Finally, rules that generate trades
add.rule(strategy = strategy_st, name = "ruleSignal",  # Almost always this one
         arguments = list(sigcol = "bull",  # Signal (see above) that triggers
                          sigval = TRUE,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open",
                          osFUN = osMaxDollar,
                          # The next parameter, which is a parameter passed to
                          # osMaxDollar, will ensure that trades are about 10%
                          # of portfolio equity
                          maxSize = quote(floor(getEndEq(account_st,
                                                         Date = timestamp) * 0.2)),
                          tradeSize = quote(floor(getEndEq(account_st,
                                                           Date = timestamp) * 0.2))),
         type = "enter", path.dep = TRUE, label = "buy")

add.rule(strategy = strategy_st, name = "ruleSignal",
         arguments = list(sigcol = "bear",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "exit", path.dep = TRUE, label = "sell")

applyStrategy(strategy_st, portfolios = portfolio_st)

updatePortf(portfolio_st)

dateRange <- time(getPortfolio(portfolio_st)$summary)[-1]
updateAcct(portfolio_st, dateRange)
updateEndEq(account_st)
# Plocka ut statistik
tStats <- tradeStats(Portfolios = portfolio_st, use="trades",
                     inclZeroDays = FALSE)
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame(t(tStats[, -c(1,2)])))

final_acct <- getAccount(account_st)
plot(final_acct$summary$End.Eq["2014/2017"], main = "Portfolio Equity")


#### Multiple stocks


symbols <- c('SANION.ST', 'CINN.ST', 'IMMU.ST', 'NDA.ST', 'HMED.ST', 'WTX.ST', 'PLED.ST', 'ATORX.ST')
symbols <- c('SANION.ST', 'CINN.ST', 'IMMU.ST', 'HMED.ST', 'PLED.ST')
currency("USD")  # Currency being used

# Get new symbols
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"),
           from=start, to=Sys.Date(), adjust=T)

par(mfrow=c(3,3))
for(symbol in symbols)
{
  getSymbols(symbol, src = "yahoo", from = start, to = end, auto.assign = TRUE)

}

candleChart(SANION.ST , up.col = "black", dn.col = "red", theme = "white", subset="2016/")
addSMA(n = c(20, 50, 200))
candleChart(CINN.ST , up.col = "black", dn.col = "red", theme = "white", subset="2016/")
addSMA(n = c(20, 50, 200))
candleChart(IMMU.ST , up.col = "black", dn.col = "red", theme = "white", subset="2016/")
addSMA(n = c(20, 50, 200))
candleChart(HMED.ST , up.col = "black", dn.col = "red", theme = "white", subset="2016/")
addSMA(n = c(20, 50, 200))
candleChart(PLED.ST , up.col = "black", dn.col = "red", theme = "white", subset="2016/")
addSMA(n = c(20, 50, 200))
zoomChart('2017')

stockSymbols()
for(symbol in symbols)
{
  stock(symbol, currency="USD",multiplier=1)
  x<-get(symbol)
  colnames(x)<-gsub("x",symbol,colnames(x))
  assign(symbol,x)

  }



multi.asset <- "multiAsset"
rm.strat(multi.asset) # remove strategy etc. if this is a re-run

initPortf(multi.asset,symbols=symbols, initDate=start)
initAcct(multi.asset,portfolios=multi.asset, initDate=start,
         initEq=10000)
initOrders(portfolio=multi.asset,initDate=initDate)

applyStrategy(strategy=strategy_st , portfolios=multi.asset)

updatePortf(multi.asset)
updateAcct(multi.asset)
updateEndEq(multi.asset)

checkBlotterUpdate(multi.asset,multi.asset)
a <- getAccount(multi.asset)
p <- getPortfolio(multi.asset)
names(p$symbols)


# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
par(mfrow=c(3,2))
for(symbol in symbols)
{
  chart.Posn(Portfolio=multi.asset,Symbol=symbol,theme=myTheme,
             TA="add_SMA(n=10,col='blue')")
}
par(mfrow=c(1,1))

rets.multi <- PortfReturns(multi.asset)
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi,Return.calculate(a$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "TOTAL"
rets.multi <- rets.multi[,c("TOTAL",symbols)]
round(tail(rets.multi,5),6)

chart.CumReturns(rets.multi, colorset= rich10equal, legend.loc = "topleft",
                 main="SPDR Cumulative Returns")
chart.Boxplot(rets.multi, main = "SPDR Returns", colorset= rich10equal)
(ar.tab <- table.AnnualizedReturns(rets.multi))

max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])

chart.RiskReturnScatter(rets.multi,
                        main = "SPDR Performance", colorset = rich10equal,
                        xlim=c(0,max.risk*1.1),ylim=c(-.8,max.return))

equity <- a$summary$End.Eq
plot(equity,main="Consolidated SPDR Equity Curve")

#function 1 ___________________________________________________________________________

remove(list=ls())
fileName = "http://www.stat.berkeley.edu/~nolan/data/stocks/f.csv"
fileName2 = "http://www.stat.berkeley.edu/~nolan/data/stocks/gm.csv"
dateFormat = "%m/%d/%y"

readData = function(fileName, dateFormat){
  
  stockData = read.csv(fileName, colClasses=c("character","NULL","NULL","NULL","NULL","NULL",NA))
  stockData$Date = as.Date(stockData$Date, format = dateFormat)
  stockData = stockData[ order(stockData$Date),   ]
  return(stockData)
}

example = readData(fileName, dateFormat)         
example2 = readData(fileName2, dateFormat)         


#function 2 ___________________________________________________________________________

combine2stocks = function(stockA, stockB){  #stockA= readData(something)
  
  dateconv = as.Date(intersect(stockA$Date, stockB$Date), origin = "1970-01-01")
  intersectA = stockA$Date %in% stockB$Date
  intersectB = stockB$Date %in% stockB$Date
  adj.close.a = stockA$Adj.Close[intersectA]
  adj.close.b = stockB$Adj.Close[intersectB]
  
  combinedData = data.frame(Date = dateconv, Adj.Close.A = adj.close.a, Adj.Close.B = adj.close.b,
                            ratio = adj.close.a / adj.close.b )
  return(combinedData)

}
example3=combine2stocks( readData(fileName, dateFormat), readData(fileName2, dateFormat))  #-------->testing

#function 3 ___________________________________________________________________________
 
plotRatio = function(ratio, k=1, date= seq(along=ratio)){
  
  plot(ratio ~ date, xlab = "Date", ylab = "ratio", col = "grey", type = "l")
  abline(h = mean(ratio), col = "green", lty = 2, lwd = 1)
 
  abline(h = mean(ratio) + sd(ratio)*k, col = "red", lty = 2, lwd = 1)
  abline(h = mean(ratio) + sd(ratio)*(-k), col = "red", lty = 2, lwd = 1)

}
plotRatio(example3$ratio,1,example3$Date) #---------------------------->  testing

#function 4 ___________________________________________________________________________

showPosition = function(pos, ratio, col = c("green", "red"), radius = 100){

  symbols( as.Date(pos[1],"%Y-%m-%d"), ratio[1], circles = radius, fg = col[1], inches = 0.1, add=TRUE)

  symbols( as.Date(pos[2],"%Y-%m-%d"), ratio[2], circles = radius, fg = col[2], inches = 0.1, add=TRUE)
}

  posPosition = as.Date(example3$Date[ c(49,87)])
  ratioPosition =example3$ratio[c(49,87)]
  showPosition(posPosition, ratioPosition)

#function 5 ___________________________________________________________________________

findNextPosition = function(ratio, startDay=1, k=1, m = mean(ratio), s = sd(ratio)){
  
  if(is.na(startDay) | is.null(startDay)){
    return(integer())
  }
  
  if( length(which( ratio[startDay:length(ratio)]>=m+k*s | ratio[startDay:length(ratio)]<=m-k*s))!=0 ){
    open=which( ratio[startDay:length(ratio)]>=m+k*s | ratio[startDay:length(ratio)]<=m-k*s )[1]+startDay-1
    
    if(is.null(open)){return(integer())}
    
    
    if( ratio[open]>=m+k*s){
      close=which( ratio[open:length(ratio)] <= m )[1]
      if( is.na(close) | is.null(close )){
        pos=c(open,length(ratio))
        return(pos)
      }
      close=close+open-1
      pos=c(open,close)
      return(pos)
    }
    close=which( ratio[open:length(ratio)] >= m )[1]
    if( is.na(close) | is.null(close )){
      pos=c(open,length(ratio))
      return(pos)
    }
    close=close+open-1
    pos=c(open,close)
    return(pos)  
  }
  return(integer())
}

findNextPosition(example3$ratio, startDay=1, 1)  

#function 6 ___________________________________________________________________________

getPositions = function(ratio, k = 1, m = mean(ratio), s = sd(ratio)){
  
  open = vector(mode = "integer")
  close = vector(mode = "integer")
  i = 1
  j = 1
  while( j < length(ratio)){
    
    if( !(is.na(findNextPosition(ratio, startDay=j, k, m, s)[1])) ) {
    open[i] = findNextPosition(ratio, startDay=j, k, m, s)[1]
    close[i] = findNextPosition(ratio, startDay=j, k, m, s)[2]
    }
    
    if( !(is.na(findNextPosition(ratio, startDay=j, k, m, s)[2])) ) {
      j = findNextPosition(ratio, startDay=j, k)[2]
    }
   
    if( is.na(findNextPosition(ratio, startDay=j, k, m, s)[2]) ) {
      j = length(ratio)
    }
    i= i + 1
  }
    
 return(list(open, close))
}

A = getPositions(example3$ratio, 1)

#function 7 __________________________________________________________________

positionProfit = function(pos, stockPriceA, stockPriceB) {
  
  mean.ratio = mean( stockPriceA/stockPriceB )
  if(stockPriceA[pos[1]]/stockPriceB[pos[1]] >= mean.ratio ){
    
    A = 1 - (1/stockPriceA[pos[1]]) * (stockPriceA[pos[2]])
    B = -1+ (1/stockPriceB[pos[1]]) * (stockPriceB[pos[2]])
    total = A + B
  }
  
  if(stockPriceA[pos[1]]/stockPriceB[pos[1]] <= mean.ratio ){
    
    A = -1+ (1/stockPriceA[pos[1]]) * (stockPriceA[pos[2]])
    B = 1 - (1/stockPriceB[pos[1]]) * (stockPriceB[pos[2]])
    total = A + B
  }
  return(total)
}

positionProfit(c(1,1036 ), example3$Adj.Close.A, example3$Adj.Close.B)

#function 8 __________________________________________________________________

getProfit.K = function(x, y, k, m = mean(x/y), s = sd(x/y)){
  
 openclose.list = getPositions(x/y, k, m, s)
 i = 1
 profit = 0
 while( i <= length(openclose.list[[1]]) ) {
   profit = positionProfit( c(openclose.list[[1]][[i]], openclose.list[[2]][[i]]), x, y ) + profit
   i = i+1
 }
 return(profit)
}

getProfit.K(example3$Adj.Close.A, example3$Adj.Close.B, 0.1)

# function 9 _________________________________________________________________________
getBest.K = function(x, y, k.min = 0.1, k.max, numK, m=mean(x/y), s=sd(x/y)){
  
  i = k.min
  a = 0
  while( i < k.max){
    if( i < 0 ){
      a = -10000
    }
    
    if( getProfit.K(x, y, i) > a){
      a = getProfit.K(x, y, i)
      b = i
    }
    
  i = i + ((abs(k.max)-abs(k.min)) / (numK - 1))
  }
  return(b)
}

getBest.K(example3$Adj.Close.A, example3$Adj.Close.B, 0.1, 100, 100)

# function 10 ________________________________________________________________________

stockSim = function(n = 4000, rho = 0.99, psi = 0, sigma = rep(1, 2),
                    beta0 = rep(100, 2), beta1 = rep(0, 2),
                    epsilon = matrix(rnorm(2*n, sd = sigma),
                                     nrow = n, byrow = TRUE)){
  
  x1 = vector(mode = "numeric", length = n)
  x2 = vector(mode = "numeric", length = n)
  x1[1] = epsilon[1,1]
  x2[1] = epsilon[1,2]
  for(i in 2:n){
    
    x1[i] = rho*x1[i-1] + psi*(1-rho)*x2[i-1] + epsilon[i,1]
    x2[i] = rho*x2[i-1] + psi*(1-rho)*x1[i-1] + epsilon[i,2]
  }
  y1 = vector(mode = "numeric", length = n)
  y2 = vector(mode = "numeric", length = n)
  
  for(i in 1:n){
    
    y1[i] = beta0[1] + beta1[1]*i+x1[i]
    y2[i] = beta0[2] + beta1[2]*i+x2[i]  
  }
  stockSim = data.frame(A = y1, B = y2)
  return(stockSim)
}
stock = stockSim()
stock2= stockSim()
stock3= stockSim()
stock4= stockSim()
#1
plotRatio(stock$A/ stock$B, 1, 1:4000)
getPositions(stock$A/ stock$B, 1)

getProfit.K(stock$A, stock$B, getBest.K(stock$A, stock$B, 0.1, 100, 100))
getBest.K(stock$A, stock$B, 0.1, 100, 100)

#2
plotRatio(stock2$A/ stock2$B, 1, 1:4000)
getPositions(stock2$A/ stock2$B, 1)

getProfit.K(stock2$A, stock2$B, 1)
getBest.K(stock2$A, stock2$B, 1, 2, 4)

#3
plotRatio(stock3$A/ stock3$B, 1, 1:4000)
getPositions(stock3$A/ stock3$B, 1)

getProfit.K(stock3$A, stock3$B, 1)
getBest.K(stock3$A, stock3$B, 1, 2, 4)

#4
plotRatio(stock4$A/ stock4$B, 1, 1:4000)
getPositions(stock4$A/ stock4$B, 1)

getProfit.K(stock4$A, stock4$B, 1)
getBest.K(stock4$A, stock4$B, 1, 2, 4)

  
# function 11 ___________________________________________________________________

runSim = function(rho, psi, beta0 = c(100, 100), beta1 = c(0, 0),
                  sigma = c(1, 1), n = 100){
  
  stock = stockSim(n, rho, psi, sigma,
                   beta0, beta1)


  train.period = c( floor((n/2) - ((n/2) -1)), floor(n/2), floor((n/2)+1) , floor(n) )
  train.df = data.frame(Price.A = stock$A, Price.B = stock$B, ratio = stock$A/stock$B)
  
  stock.train = stock[train.period[1] : train.period[2], ]
  stock.test  = stock[train.period[3] : train.period[4], ]
  
  up = (max(stock.train$A/stock.train$B) - mean(stock.train$A/stock.train$B) ) / sd(stock.train$A/stock.train$B)
  down = (mean(stock.train$A/stock.train$B) - min(stock.train$A/stock.train$B) )/ sd(stock.train$A/stock.train$B)
  k.max = max(up, down)
  
  up2 = (max(stock.test$A/stock.test$B) - mean(stock.test$A/stock.test$B) ) / sd(stock.test$A/stock.test$B)
  down2 = (mean(stock.test$A/stock.test$B) - min(stock.test$A/stock.test$B) )/ sd(stock.test$A/stock.test$B)
  k.max2 = max(up2, down2)
 
  best.K1 = getBest.K(stock.train$A, stock.train$B, 0.1, k.max, 100)
  best.K2 = getBest.K(stock.test$A, stock.test$B, 0.1, k.max2, 100)
  
  profit1 = getProfit.K(stock.train$A, stock.train$B, best.K1)
  profit2 = getProfit.K(stock.test$A, stock.test$B, best.K2)
  
  final = max(profit1, profit2)
  return(final)
}
runSim(0.99, 0.1)

#function 12___________________________________________________________________________

simProfitDist =
  function(..., B = 100)
    sapply(1:B,  function(i, ...) runSim(...), ...)
  
simProfitDist(0.99, 0)


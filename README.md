
<html>


</head>

<body>

<h1>Pairs Trading Simulation Study</h1>

<h2>Introduction</h2>

<p>

Problem of this project was to see the accuracy of the pair trading. How it brings the profit by using the method of pair trading. By using the pair trading method, using Ford and GM Adjusted Closed Price ratio, total profit at the end of the data plot over 4 years showed that this method worked on these two different stocks. Profit was positive for these two stocks, and the method seemed to make sense. Trying the result of profit using different number of k, which decided whether to sell or buy the stocks at some point, determined the which # of k had the most profit.
</p>

<h2>Background</h2>

<p>

Pair trading is basically buying and selling using two different kind of stocks. As the ratio of these two stock hits # of mean of ratio + K * SD, sell the numerator stock (which will be at higher price than when it was bought) and buy the denominator stock ( which will be at lower price). Buy and Sell the stocks oppositly when it passes the mean of the ratio, since it shows the price of the stock we bought(own) had rose and the price of the stock we sold had decreased. As these steps continue on, profit would increase.
</p>

<h2>Empirical Study</h2>

<p>
  On Ford vs GM, there are 18 openings and 18 closings when k = 1 in the plot. Each pair of open and close will bring profit. As more of these pairs add up to the total profit, more profits will be there as time goes by. These two are real stocks from 2011 to 2014, but by using this pair trading method, profit has been made. At k = 1, 1.34 was made.
  
  By using best.K function, we were able to calculate the best K, which brought the most profit over these time interval. Using best K, which was 0.1 made 2.04, which is obviously higher than 1.34. The reason k = 0.1 made more is because there are more active trading opening and closing positions as k goes lower, which leads up to more profit.
</p>

```{r, echo=FALSE}
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
  
  plot(ratio ~ date, xlab = "Date", ylab = "ratio", col = "grey", type = "l", main = "Ford vs GM")
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
A = getPositions(example3$ratio, 1)
for(i in 1:length(A[[1]])){
showPosition(as.Date(example3$Date[ c(A[[1]][[i]], A[[2]][[i]])] ), example3$ratio[c( A[[1]][[i]], A[[2]][[i]] )] )
}
```

<h2>Simulation Study</h2>

<p>

Using simulations with best.K built in and having more # of data in Date gave a larger total profit. The real stock data only had 1036 dates, compared to the simulation which had 4000. If there are more dates, more chance to have more profits since profits add up as it goes through the date. Not only it depends on the date, but also many other things, such as #of opening and closing positions, which Best.K function controls. Compared to the real stock plot, the total profit is 3.26 with best.K, which the real stock value had 2.04 with best.K. Parameter: rho = 0.99, psi = 0, sigma = rep(1, 2),  beta0 = rep(100, 2), beta1 = rep(0, 2), epsilon = matrix(rnorm(2*n, sd = sigma),nrow = n, byrow = TRUE)
</p>

```{r, echo=FALSE}
plotRatio = function(ratio, k=1, date= seq(along=ratio)){
  
  plot(ratio ~ date, xlab = "Date", ylab = "ratio", col = "grey", type = "l", main = "Simulation")
  abline(h = mean(ratio), col = "green", lty = 2, lwd = 1)
 
  abline(h = mean(ratio) + sd(ratio)*k, col = "red", lty = 2, lwd = 1)
  abline(h = mean(ratio) + sd(ratio)*(-k), col = "red", lty = 2, lwd = 1)

}

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
plotRatio(stock$A/ stock$B, 1, 1:4000)
```
<h2>Discussion</h2>

<p>
Few limitation would be predicting a future. The plot may give some hints where it had huge decrease or increase on certain dates, but it may not happen again or may happen again. For example Great Depression, no one predicted it.
</p>

<h2>Conclusions</h2>

<p>
As a conclusion, simulation and the real stock's profit shows the more time we have with Best.k, it will give higher profit. 
</p>

</body>
</html>



## for info adv
ret_turn = 0

## info advantage
prob_infadv = sample(c(1, 0), size = 1, replace = TRUE, prob = c(0.1, 0.9))
if (prob_infadv == 1) {
  po_ne = sample(c(1,-1), replace = TRUE, prob = c(0.5,0.5))
  ins_chg = round(rnorm(num_ins_player, mean = current_share_price/10, sd = current_share_price/50), digits = 2) * po_ne
  Playerdata$valuation[1:num_ins_player] = Playerdata$valuation[1:num_ins_player] + ins_chg
  ret_turn = c(ret_turn, i + 30)
}
if (i %in% ret_turn) {
  ret_chg = round(rnorm(num_ret_player, mean = current_share_price/10, sd = current_share_price/50), digits = 2) * po_ne
  Playerdata$valuation[1:num_ret_player] = Playerdata$valuation[1:num_ret_player] + ret_chg
}

############Actual Trading#############################

for (i in 1:tot_turn) {
  #### new orders
  ord_pla_mkt_los = Ordering(Playerdata, num_ins_player, num_ret_player, hist_price, fv, current_share_price)
  new_order = ord_pla_mkt_los[[1]]
  Playerdata = ord_pla_mkt_los[[2]]
  market_stm[i] = ord_pla_mkt_los[[3]]
  loser = ord_pla_mkt_los[[4]]
  
  #### creating order book
  order_book = check(order_book, new_order)
  
  #### trading
  output = trade(order_book, Playerdata, current_share_price)
  Playerdata = output[[1]]
  order_book = output[[2]]
  
  #### fv growth
  #fv = (1 + 0.0008) * fv
  
  
  #### losers out
  #if (all(is.na(loser)) == F) {
  #  loser_record = rbind(loser_record, Playerdata[Playerdata$player %in% loser,])
  #  Playerdata[Playerdata$player %in% loser,]$`total wealth` = 0
  #  Playerdata[Playerdata$player %in% loser,]$cash = 0
  #  Playerdata[Playerdata$player %in% loser,]$`trading frequency(prob)` = 0
  #}
  
  ## short squeeze (player)
  #if ((hist_price[i]/hist_price[1]) > 1.3 & nrow(Playerdata) == num_player) {
  #  sq_player = c(num_player+1, 10^11, -875000, 10^11-1750000*current_share_price, 0, 1, 1)
  #  Playerdata = rbind(Playerdata, sq_player)
  #}
  
  
  ## increasing trading frequency
  #Playerdata$ori_freq = Playerdata$ori_freq + 0.0005
  
  ## adjust trading freq. accord. mkt sent.
  Playerdata$`trading frequency(prob)` = Playerdata$ori_freq * (1 + 0.5*market_stm[i])
  
  ## short squeeze (player) freq
  #if (nrow(Playerdata) > (num_ins_player+num_ret_player)) {
  #  Playerdata[Playerdata$player == (num_player+1),]$`trading frequency(prob)` = 1
  #}
  
  
  
  ## cap for trading freq.
  Playerdata$`trading frequency(prob)` = pmin(Playerdata$`trading frequency(prob)`, 1)
  Playerdata$`trading frequency(prob)` = pmax(Playerdata$`trading frequency(prob)`, 0)
  
  #### records
  current_share_price = output[[3]]
  vol[i] = output[[4]]
  exec_hist[[i]] = output[[5]]
  index[i+1] = ineq(Playerdata, loser_record)
  hist_price[i+1] = current_share_price
  
  ## info advantage (separate)
  #prob_infadv = sample(c(1, 0), size = 1, replace = TRUE, prob = c(0.1, 0.9))
  #if (prob_infadv == 1) {
  #  #po_ne = sample(c(1,-1), replace = TRUE, prob = c(0.5,0.5))
  #  ins_chg = round(rnorm(num_ins_player, mean = current_share_price/3, sd = current_share_price/50), digits = 2) #* po_ne
  #  Playerdata$valuation[1:num_ins_player] = Playerdata$valuation[1:num_ins_player] + ins_chg
  #  ret_turn = c(ret_turn, i + 10)
  #}
  
  #if (i %in% ret_turn) {
  #  ret_chg = round(rnorm(num_ret_player, mean = current_share_price/3, sd = current_share_price/50), digits = 2) #* po_ne
  #  Playerdata$valuation[1:num_ret_player] = Playerdata$valuation[1:num_ret_player] + ret_chg
  #}
  
  
  ## debug
  #if (!all(Playerdata$stock >= 0)) {
  #  print(i)
  #  #all(Playerdata$stock >= 0)
  #  #all(Playerdata$cash >= 0)
  #  
  #  break
  #}
  #if (!all(Playerdata$cash >= 0)) {
  #  print(i)
  #  
  #  debugord = ord_pla_mkt_los[[5]]
  #  break
  #}
}

plot(index, type = "l")

plot(hist_price / ori_share_price, type = "l")

plot(vol, type = "l")
plot(market_stm, type = "l")

############Actual Trading (GME)#############################

for (i in 1:tot_turn) {
  #### new orders
  ord_pla_mkt_los = Ordering(Playerdata, num_ins_player, num_ret_player, hist_price, fv, current_share_price)
  new_order = ord_pla_mkt_los[[1]]
  Playerdata = ord_pla_mkt_los[[2]]
  market_stm[i] = ord_pla_mkt_los[[3]]
  loser = ord_pla_mkt_los[[4]]
  
  #### creating order book
  order_book = check(order_book, new_order)
  
  #### trading
  output = trade(order_book, Playerdata, current_share_price)
  Playerdata = output[[1]]
  order_book = output[[2]]
  
  #### fv growth
  #fv = (1 + 0.0008) * fv
  
  
  #### losers out
  #if (all(is.na(loser)) == F) {
  #  loser_record = rbind(loser_record, Playerdata[Playerdata$player %in% loser,])
  #  Playerdata[Playerdata$player %in% loser,]$`total wealth` = 0
  #  Playerdata[Playerdata$player %in% loser,]$cash = 0
  #  Playerdata[Playerdata$player %in% loser,]$`trading frequency(prob)` = 0
  #}
  
  ## short squeeze (player)
  if ((hist_price[i]/hist_price[1]) > 1.3 & nrow(Playerdata) == num_player) {
    sq_player = c(num_player+1, 10^11, -875000, 10^11-1750000*current_share_price, 0, 1, 1)
    Playerdata = rbind(Playerdata, sq_player)
  }
  
  
  ## increasing trading frequency
  #Playerdata$ori_freq = Playerdata$ori_freq + 0.0005
  
  ## adjust trading freq. accord. mkt sent.
  Playerdata$`trading frequency(prob)` = Playerdata$ori_freq * (1 + 0.5*market_stm[i])
  
  ## short squeeze (player) freq
  if (nrow(Playerdata) > (num_ins_player+num_ret_player)) {
    Playerdata[Playerdata$player == (num_player+1),]$`trading frequency(prob)` = 1
  }
  
  
  
  ## cap for trading freq.
  Playerdata$`trading frequency(prob)` = pmin(Playerdata$`trading frequency(prob)`, 1)
  Playerdata$`trading frequency(prob)` = pmax(Playerdata$`trading frequency(prob)`, 0)
  
  #### records
  current_share_price = output[[3]]
  vol[i] = output[[4]]
  exec_hist[[i]] = output[[5]]
  index[i+1] = ineq(Playerdata, loser_record)
  hist_price[i+1] = current_share_price
  
  ## info advantage (separate)
  #prob_infadv = sample(c(1, 0), size = 1, replace = TRUE, prob = c(0.1, 0.9))
  #if (prob_infadv == 1) {
  #  #po_ne = sample(c(1,-1), replace = TRUE, prob = c(0.5,0.5))
  #  ins_chg = round(rnorm(num_ins_player, mean = current_share_price/3, sd = current_share_price/50), digits = 2) #* po_ne
  #  Playerdata$valuation[1:num_ins_player] = Playerdata$valuation[1:num_ins_player] + ins_chg
  #  ret_turn = c(ret_turn, i + 10)
  #}
  
  #if (i %in% ret_turn) {
  #  ret_chg = round(rnorm(num_ret_player, mean = current_share_price/3, sd = current_share_price/50), digits = 2) #* po_ne
  #  Playerdata$valuation[1:num_ret_player] = Playerdata$valuation[1:num_ret_player] + ret_chg
  #}
  
  
  ## debug
  #if (!all(Playerdata$stock >= 0)) {
  #  print(i)
  #  #all(Playerdata$stock >= 0)
  #  #all(Playerdata$cash >= 0)
  #  
  #  break
  #}
  #if (!all(Playerdata$cash >= 0)) {
  #  print(i)
  #  
  #  debugord = ord_pla_mkt_los[[5]]
  #  break
  #}
}



plot(index, type = "l")

plot(hist_price / ori_share_price, type = "l")
min(hist_price)
Playerdata = Ori_playerdata
order_book = NA
current_share_price = ori_share_price

plot(vol, type = "l")
plot(market_stm, type = "l")

sd(hist_price) / mean(hist_price)
mean(diff(log(hist_price)))
sd(diff(log(hist_price)))


install.packages("writexl")
library("writexl")
GME_simu = data.frame(price = hist_price/ori_share_price, volume = c(0,vol))

write_xlsx(GME_simu,"C:\\Users\\xzhzh\\Desktop\\Master Thesis\\GME_simu.xlsx")

a = rep(0, 20)


for (k in 1:20) {
  for (i in 1:tot_turn) {
    #### new orders
    ord_pla_mkt_los = Ordering(Playerdata, num_ins_player, num_ret_player, hist_price, fv)
    new_order = ord_pla_mkt_los[[1]]
    Playerdata = ord_pla_mkt_los[[2]]
    market_stm[i] = ord_pla_mkt_los[[3]]
    loser = ord_pla_mkt_los[[4]]
    
    #### creating order book
    order_book = check(order_book, new_order)
    
    #### trading
    output = trade(order_book, Playerdata, current_share_price)
    Playerdata = output[[1]]
    order_book = output[[2]]
    
    #### fv growth
    fv = (1 + 0.0008) * fv
    
    
    #### losers out
    #if (all(is.na(loser)) == F) {
    #  loser_record = rbind(loser_record, Playerdata[Playerdata$player %in% loser,])
    #  Playerdata[Playerdata$player %in% loser,]$`total wealth` = 0
    #  Playerdata[Playerdata$player %in% loser,]$cash = 0
    #  Playerdata[Playerdata$player %in% loser,]$`trading frequency(prob)` = 0
    #}
    
    
    
    ## increasing trading frequency
    Playerdata$ori_freq = Playerdata$ori_freq + 0.0005
    
    ## adjust trading freq. accord. mkt sent.
    Playerdata$`trading frequency(prob)` = Playerdata$ori_freq * (1 + 0.5*market_stm[i])
    
    ## cap for trading freq.
    Playerdata$`trading frequency(prob)` = pmin(Playerdata$`trading frequency(prob)`, 1)
    Playerdata$`trading frequency(prob)` = pmax(Playerdata$`trading frequency(prob)`, 0)
    
    #### records
    current_share_price = output[[3]]
    vol[i] = output[[4]]
    exec_hist[[i]] = output[[5]]
    index[i+1] = ineq(Playerdata, loser_record)
    hist_price[i+1] = current_share_price
    
  }
  
  ####wealth change
  a[k] = index[tot_turn+1]/index[1]-1
  
  ####restore
  Playerdata = Ori_playerdata
  order_book = NA
  order_book = NA
  current_share_price = ori_share_price
}


a

sum(a)

#### Simulation (return the index)
simu <- function(Playerdata, num_ins_player, num_ret_player, order_book, current_share_price, tot_turn, hist_price) {
  beg = ineq(Playerdata)
  for (i in 1:tot_turn) {
    #### new orders
    ord_pla = Ordering(Playerdata, num_ins_player, num_ret_player, hist_price)
    new_order = ord_pla[[1]]
    Playerdata = ord_pla[[2]]
    
    #### creating order book
    order_book = check(order_book, new_order)
    
    #### trading
    output = trade(order_book, Playerdata, current_share_price)
    Playerdata = output[[1]]
    order_book = output[[2]]
    
    #### records
    current_share_price = output[[3]]
  }
  end = ineq(Playerdata)
  chg = end - beg
  return(chg)
}

replicate(n = 20, simu(Playerdata, num_ins_player, num_ret_player, order_book, current_share_price, tot_turn))


############restore#####################################
Playerdata = Ori_playerdata
order_book = NA
########################################################

a = diff(hist_price, lag = 1)
plot(a, type = "l")


library("forecast")
acf(a)
pacf(a)
rtarima = auto.arima(a, allowmean = T, stepwise = F, approximation = T, ic = "bic")
summary(rtarima)

res = rtarima$residuals
acf(res)
pacf(res)

Box.test(res,lag = 5,type = "Ljung")
Box.test(res,lag = 10,type = "Ljung")
Box.test(res,lag = 20,type = "Ljung")

vo = res^2
plot(vo)
acf(vo)
pacf(vo)


output = trade(order_book, Playerdata, current_share_price)
output[[1]]
output[[2]]
a = output[[2]]


sum(Playerdata$stock)

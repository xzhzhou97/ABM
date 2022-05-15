############################
#### Just Source-run it ####
############################

############reading frequency##################
re_freq <- function(freq_data) {
  result = sample(c(freq_data[1],NA), size = 1, replace = TRUE, prob = c(freq_data[2], 1 - freq_data[2]))
  return(result)
}
###############################################


############creating order function############
Ordering <- function(Playerdata, num_ins_player, num_ret_player, hist_price, fv, current_share_price) {
  
  #### selecting trading agents
  #### apply in case of different trading frequencies
  select_data = cbind(Playerdata$player, Playerdata$`trading frequency(prob)`)
  ord_agent = apply(select_data, 1, re_freq)
  ord_agent = as.numeric(na.omit(ord_agent))
  
  #### updating the valuations for selected agents
  
  num_trader = as.numeric(length(ord_agent))

  ## mkt sentiment
  mkt_stm = 0
  
  
  ## innovations

  inno = rnorm(num_trader, mean = 0, sd = current_share_price/10)
  #inno[c(1,2)] = rnorm(2, mean = 0, sd = current_share_price/100)
  #inno = 0
  
  ## update
  Playerdata[Playerdata$player %in% ord_agent,]$valuation = Playerdata[Playerdata$player %in% ord_agent,]$valuation + inno
  
  ## non-negative
  Playerdata[Playerdata$player %in% ord_agent,]$valuation = pmax(Playerdata[Playerdata$player %in% ord_agent,]$valuation, 0)
  
  
  #### extract rows from data frame
  Ordering_data = Playerdata[Playerdata$player %in% ord_agent,]
  
  #### adding the row of needed stock
  exp_stock = rep(0,num_trader)
  
  Ordering_data$exp_stock = exp_stock
  
  #### calculating needed proportion
  #### (keep x of wealth as stock, x = e^((V-P)/V-1))
  prop = exp((Ordering_data[Ordering_data$valuation != 0,]$valuation - current_share_price)/Ordering_data[Ordering_data$valuation != 0,]$valuation - 1)
  Ordering_data[Ordering_data$valuation != 0,]$exp_stock = prop * Ordering_data[Ordering_data$valuation != 0,]$`total wealth`/ current_share_price
  
  
  ## not exceeding cash order
  max_share = Ordering_data[Ordering_data$valuation != 0,]$cash / Ordering_data[Ordering_data$valuation != 0,]$valuation + Ordering_data[Ordering_data$valuation != 0,]$stock
  
  Ordering_data[Ordering_data$valuation != 0,]$exp_stock = pmin(Ordering_data[Ordering_data$valuation != 0,]$exp_stock, max_share)
  
  
  if (as.numeric(length(Ordering_data[Ordering_data$valuation == 0,]$exp_stock) > 0)) {
    Ordering_data[Ordering_data$valuation == 0,]$exp_stock = 0
  }
  
  Ordering_data$exp_stock = floor(Ordering_data$exp_stock)
  
  loser_list = NA
  
  #### Actual Order
  volume = Ordering_data$exp_stock - Ordering_data$stock
  
  Ordering_data$volume = volume
  
  Orders = Ordering_data[,c("player","valuation","volume")]
  
  
  
  #### return
  result = list(Orders, Playerdata, mkt_stm, loser_list)
  return(result)
}
################################################



############check and combine###################
#### check for existing order placed by duplicate agents
check <- function(order_book, new_order) {
  if (is.null(nrow(order_book)) == F) {
    #### checking process
    ordercheck = c(order_book$player, new_order$player)
    dup = ordercheck[duplicated(ordercheck)]
    
    #### remove duplicated players
    order_book = order_book[!order_book$player %in% dup,]
    
    #### combining
    order_book = rbind(order_book, new_order)
  }
  else {
    #### combining
    order_book = new_order
  }
  
  ## sort
  order_book = order_book[order(order_book$player),]
  
  ####return
  return(order_book)
}

################################################

############Trading function#######################
trade <- function(order_book, Playerdata, current_share_price) {
  
  #### buy and sell
  sell = subset(order_book, volume < 0)
  buy = subset(order_book, volume > 0)
  
  #### the execution record
  exec = NA
  
  #### the volume record
  volume = 0
  
  #### sorting by price
  sell = sell[order(sell$valuation),]
  buy = buy[order(buy$valuation, decreasing = T),]
  
  
  #### matching
  if (is.na(buy$volume[1])==F & is.na(sell$volume[1])==F) {
    while (sell$valuation[1]<=buy$valuation[1]) {
      if (abs(sell$volume[1]) > buy$volume[1]) {
        sell$volume[1] = sell$volume[1]+buy$volume[1]
        
        #### to record partial sell
        temp = sell[1,]
        temp$volume[1] = -buy$volume[1]
        
        #### update execution (always in order of buy and sell)
        exec = rbind(exec, buy[1,], temp)
        
        #### update volume
        volume = volume + buy$volume[1]
        
        #### update the share price
        current_share_price = mean(c(sell$valuation[1],buy$valuation[1]))
        
        #### update buy
        buy = buy[-1,]
      }
      else if(abs(sell$volume[1]) < buy$volume[1]) {
        buy$volume[1] = sell$volume[1]+buy$volume[1]
        
        #### to record partial buy
        temp = buy[1,]
        temp$volume[1] = -sell$volume[1]
        
        #### update execution
        exec = rbind(exec, temp, sell[1,])
        
        #### update volume
        volume = volume - sell$volume[1]
        
        #### update the share price
        current_share_price = mean(c(sell$valuation[1],buy$valuation[1]))
        
        #### update sell
        sell = sell[-1,]
      }
      else {
        #### update execution
        exec = rbind(exec, sell[1,], buy[1,])
        
        #### update volume
        volume = volume + buy$volume[1]
        
        #### update the share price
        current_share_price = mean(c(sell$valuation[1],buy$valuation[1]))
        
        #### update buy and sell
        buy = buy[-1,]
        sell = sell[-1,]
      }
      if (is.na(buy$volume[1]) | is.na(sell$volume[1])) {
        break
      }
    }
  }
  
  if (is.null(nrow(exec)) == F) {
    #### remove the NA for exec
    exec = exec[!is.na(exec$player),]
    
    #### updating the order book
    order_book = rbind(buy, sell)
    order_book = order_book[order(order_book$player),]
    
    #### update the player holdings
    for (i in 1:nrow(exec)) {
      Playerdata[Playerdata$player == exec$player[i],]$cash = Playerdata[Playerdata$player == exec$player[i],]$cash - exec$volume[i]*current_share_price
      Playerdata[Playerdata$player == exec$player[i],]$stock = Playerdata[Playerdata$player == exec$player[i],]$stock + exec$volume[i]
    }
    
    #### re-calculating the total wealth
    Playerdata$`total wealth` = Playerdata$cash + Playerdata$stock*current_share_price
    
    #### round to two digits
    #Playerdata = round(Playerdata, digits = 2)
  }
  
  ####return
  record = list(Playerdata, order_book, current_share_price, volume, exec)
  
  return(record)
}

###################################################

#### Measurement of inequality (% of wealth) ######

ineq <- function(Playerdata, loser_record) {
  #if (is.null(nrow(loser_record)) == T) {
  ineq_mea = sum(Playerdata$`total wealth`[1:num_ins_player])/sum(Playerdata$`total wealth`)*100
  #}
  #else {
  #  ineq_mea = sum(Playerdata[Playerdata$player %in% c(1:num_ins_player),]$'total wealth', loser_record[loser_record$player %in% c(1:num_ins_player),]$'total wealth')/(sum(Playerdata$`total wealth`) + sum(loser_record$`total wealth`))*100
  #}
  
  ####return
  return(ineq_mea)
}

###################################################



############Settings######################
#### current price
ori_share_price = (exp(-1)*10000)/(1-exp(-1))
current_share_price = ori_share_price

## fundamental value

fv = 20

#### Number of total traders
num_ins_player = 50
num_ret_player = 5000
num_player = num_ins_player + num_ret_player

#### Creating Players' data matrix
#### player, cash, stock, total wealth, valuation, trading frequency (prob)
cash = 1000000000

ins_player = c(NA, cash, 100000, NA, NA, NA)
ret_player = round(ins_player/1000, digits = 2)

Playermatrix = matrix(data = NA, nrow = num_player, ncol = 6)
colnames(Playermatrix) = c("player", "cash", "stock", "total wealth", "valuation", "trading frequency(prob)")

#### heads ins, tails ret
for (i in 1:num_ins_player) {
  Playermatrix[i,] = ins_player
}

for (i in (num_ins_player+1):num_player) {
  Playermatrix[i,] = ret_player
}


#### numbering the agents
for (i in 1:num_player) {
  Playermatrix[i,1] = i
}

#### frequency
#for (i in 1:num_ins_player) {
#  Playermatrix[i,6] = 0.2
#}

#library("truncnorm")

#for (i in 1:num_ret_player) {
#  Playermatrix[num_ins_player + i,6] = 0.3
#}

#Playermatrix[,6] = c(rnorm(num_ins_player, mean = 0.3, sd = 0.05), 
#                     rtruncnorm(num_ret_player/2, mean=0.2, sd=0.05), 
#                     rtruncnorm(num_ret_player/2, mean=0.7, sd=0.1))

## non-negative and no larger than 1
#Playermatrix[,6] = pmax(Playermatrix[,6], 0)
#Playermatrix[,6] = pmin(Playermatrix[,6], 1)

Playermatrix[,6] = 1

#### creating valuation
c_valuation = ori_share_price
####rnorm(num_player, mean = ori_share_price, sd = ori_share_price/10)

Playermatrix[,5] = c_valuation

#### total turns
tot_turn = 120

#### Create data frame
Playerdata = as.data.frame(Playermatrix)

## ori trading freq. (for mkt sent. influence)
ori_freq = Playerdata$`trading frequency(prob)`
Playerdata$ori_freq = ori_freq



#### fill in the total wealth
Playerdata$`total wealth` = Playerdata$cash + Playerdata$stock*current_share_price

#### weights for trend
#t_w = c(rnorm(num_ins_player, mean = 0.8, sd = 0.01), rnorm(num_ret_player, mean = 0.8, sd = 0.15))

#Playerdata$trend_weight = t_w

#### weights for fundamental
#f_w = c(rnorm(num_ins_player, mean = 0.8, sd = 0.01), rnorm(num_ret_player, mean = 0.8, sd = 0.15))
#
#Playerdata$funda_weight = f_w

#### save a copy of the starting agents settings
Ori_playerdata = Playerdata

#### Creating the order book for the first time
order_book = NA

#### loser record
loser_record = 0

#### Creating inequality record 
index = rep(NA, tot_turn+1)
index[1] = ineq(Playerdata, loser_record)

#### Creating the price record
hist_price = rep(NA, tot_turn+1)
hist_price[1] = ori_share_price

#### Creating volume record
vol = rep(0, tot_turn)

#### create execution record
exec_hist = vector(mode = "list", length = tot_turn)

#### create mkt stm record
market_stm = rep(0, tot_turn)



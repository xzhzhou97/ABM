chg_index = as.data.frame(matrix(data = 0, nrow = 20, ncol = 3))

for (k in 1:3) {
  
  for (j in 1:20) {
    
    #### Number of total traders
    num_ins_player = 5
    num_ret_player = 3000-500*k
    num_player = num_ins_player + num_ret_player
    
    #### Creating Players' data matrix
    #### player, cash, stock, total wealth, valuation, trading frequency (prob)
    cash = 500000000
    
    ins_player = c(NA, cash, 100000, NA, NA, NA)
    ret_player = round(ins_player/500, digits = 2)
    
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
    Playermatrix[,6] = c(rnorm(num_ins_player, mean = 0.3, sd = 0.05), 
                         rtruncnorm(num_ret_player/2, mean=0.2, sd=0.05), 
                         rtruncnorm(num_ret_player/2, mean=0.5, sd=0.1))
    
    ## non-negative and no larger than 1
    Playermatrix[,6] = pmax(Playermatrix[,6], 0)
    Playermatrix[,6] = pmin(Playermatrix[,6], 1)
    
    #### creating valuation
    c_valuation = ori_share_price
    
    Playermatrix[,5] = c_valuation
    
    #### Create data frame
    Playerdata = as.data.frame(Playermatrix)
    
    ## ori trading freq. (for mkt sent. influence)
    ori_freq = Playerdata$`trading frequency(prob)`
    Playerdata$ori_freq = ori_freq
    
    #### Randomize the proportion(wealth)
    cash_r = rnorm(num_player, mean = 1, sd = 0.05)
    Playerdata$cash = round(Playerdata$cash * cash_r, digits = 2)
    
    stock_r = rnorm(num_player, mean = 1, sd = 0.05)
    Playerdata$stock = round(Playerdata$stock * stock_r)
    
    #### fill in the total wealth
    Playerdata$`total wealth` = Playerdata$cash + Playerdata$stock*current_share_price
    
    
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
    
    chg_index[j,k] = index[tot_turn+1] - index[1]
    
    ####restore
    Playerdata = Ori_playerdata
    order_book = NA
    current_share_price = ori_share_price
    
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
    
  }
  
  
}

library("writexl")

write_xlsx(chg_index,"C:\\Users\\xzhzh\\Desktop\\Master Thesis\\indexchg_ins_port.xlsx")







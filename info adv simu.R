chg_index = as.data.frame(matrix(data = 0, nrow = 20, ncol = 3))

### no info adv
for (j in 1:20) {
  
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
  
  chg_index[j,1] = index[tot_turn+1] - index[1]
  
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


##########info adv

## for info adv
ret_turn = 0

## info advantage
#prob_infadv = sample(c(1, 0), size = 1, replace = TRUE, prob = c(0.1, 0.9))
#if (prob_infadv == 1) {
#  po_ne = sample(c(1,-1), replace = TRUE, prob = c(0.5,0.5))
#  ins_chg = round(rnorm(num_ins_player, mean = current_share_price/10, sd = current_share_price/50), digits = 2) * po_ne
#  Playerdata$valuation[1:num_ins_player] = Playerdata$valuation[1:num_ins_player] + ins_chg
#  ret_turn = c(ret_turn, i + 30)
#}
#if (i %in% ret_turn) {
#  ret_chg = round(rnorm(num_ret_player, mean = current_share_price/10, sd = current_share_price/50), digits = 2) * po_ne
#  Playerdata$valuation[1:num_ret_player] = Playerdata$valuation[1:num_ret_player] + ret_chg
#}



for (k in 2:3) {
  
  for (j in 1:20) {
    
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
      
      ## info advantage (separate)
      prob_infadv = sample(c(1, 0), size = 1, replace = TRUE, prob = c(0.05, 0.95))
      if (prob_infadv == 1) {
        #po_ne = sample(c(1,-1), replace = TRUE, prob = c(0.5,0.5))
        ins_chg = rnorm(num_ins_player, mean = current_share_price*0.15, sd = current_share_price/50) #* po_ne
        Playerdata$valuation[1:num_ins_player] = Playerdata$valuation[1:num_ins_player] + ins_chg
        ret_turn = c(ret_turn, i + 2*k*5 - 10)
      }
      
      if (i %in% ret_turn) {
        ret_chg = rnorm(num_ret_player, mean = current_share_price*0.15, sd = current_share_price/50) #* po_ne
        Playerdata$valuation[1:num_ret_player] = Playerdata$valuation[1:num_ret_player] + ret_chg
      }
      
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

write_xlsx(chg_index,"C:\\Users\\xzhzh\\Desktop\\Master Thesis\\index_chg_info_adv.xlsx")

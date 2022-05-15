chg_index = as.data.frame(matrix(data = 0, nrow = 20, ncol = 3))

for (k in 1:3) {
  
  
  
  ############creating order function############
  Ordering <- function(Playerdata, num_ins_player, num_ret_player, hist_price, fv, current_share_price) {
    
    #### selecting trading agents
    #### apply in case of different trading frequencies
    select_data = cbind(Playerdata$player, Playerdata$`trading frequency(prob)`)
    ord_agent = apply(select_data, 1, re_freq)
    ord_agent = as.numeric(na.omit(ord_agent))
    
    #### updating the valuations for selected agents
    
    num_trader = as.numeric(length(ord_agent))
    
    hist_p = as.numeric(na.omit(hist_price))
    
    
    ## mkt sentiment
    mkt_stm = 0
    
    if (as.numeric(length(hist_p)>=21)) {
      hist_rtn20 = diff(log(rev(tail(hist_p, n = 21))))
      
      mkt_stm = mean(hist_rtn20) * 0.4 - sd(hist_rtn20) * 0.25 + 0.02
    }
    
    
    
    ## fundamental value
    
    
    Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ins_player) & Playerdata$valuation < fv,]$valuation = 
      Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ins_player) & Playerdata$valuation < fv,]$valuation +
      (fv - Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ins_player) & Playerdata$valuation < fv,]$valuation) * 0.8
    
    Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ret_player) & Playerdata$valuation < fv,]$valuation = 
      Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ret_player) & Playerdata$valuation < fv,]$valuation +
      (fv - Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ret_player) & Playerdata$valuation < fv,]$valuation) * 0.6
    
    
    
    
    ## innovations (including mkt effect) ((n+2) length)
    if (as.numeric(length(hist_p)>=21)) {
      rtn_sd = current_share_price * sd(hist_rtn20)
      inno = rnorm(num_trader, mean = 0, sd = rtn_sd*0.6)
    }
    else {
      inno = rnorm(num_trader, mean = 0, sd = current_share_price/50)
    }
    
    
    
    
    ## MKT sent.
    Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ins_player),]$valuation = Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ins_player),]$valuation * (1 + 0.6*mkt_stm)
    Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ret_player),]$valuation = Playerdata[Playerdata$player %in% ord_agent & Playerdata$player %in% c(1:num_ret_player),]$valuation * (1 + (1.3-0.3*k)*mkt_stm)
    
    
    ## update
    Playerdata[Playerdata$player %in% ord_agent,]$valuation = Playerdata[Playerdata$player %in% ord_agent,]$valuation + inno
    
    
    ## non-negative
    Playerdata[Playerdata$player %in% ord_agent,]$valuation = pmax(Playerdata[Playerdata$player %in% ord_agent,]$valuation, 0)
    
    
    ## timevalue(?)
    #Playerdata$valuation = Playerdata$valuation + 0.25
    
    ## let the ins player chg be non-negative
    ##if (Playerdata$valuation[1] <= 36.69) {
    ##  Playerdata$valuation[1] = 36.69
    ##}
    
    ## let the ins player stay the same
    ##Playerdata$valuation[1] = 36.69
    
    
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
    
    #########debug
    debugdata = list(Ordering_data, Orders)
    
    
    
    #### return
    result = list(Orders, Playerdata, mkt_stm, loser_list, debugdata)
    return(result)
  }
  ################################################
  
  for (j in 1:20) {
    ## keep all other factor constant but ins_traders' sensitivity to MKT sent.
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
      
      ## short squeeze (player)
      if ((hist_price[i]/hist_price[1]) > 1.3 & nrow(Playerdata) == num_player) {
        sq_player = c(num_player+1, 10^11, -875000, 10^11-1750000*current_share_price, 0, 1, 1)
        Playerdata = rbind(Playerdata, sq_player)
      }
      
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


install.packages("writexl")
library("writexl")

write_xlsx(chg_index,"C:\\Users\\xzhzh\\Desktop\\Master Thesis\\sensitivity_index_chg.xlsx")




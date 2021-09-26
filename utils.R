

binance_login <- function(credentials_file = "credentials.json") {
    cred <- jsonlite::read_json(credentials_file, simplifyVector = T)
    binance_credentials(key = cred$binance$binance_key,
                        secret = cred$binance$binance_secret)
    return(TRUE)
}


binance_sync <- function(...) {
    message("Syncing binance trades...")
    binance_login(...)
    bwallet <- binance_balances(threshold = 0)
    bwallet <- bwallet %>% 
        filter(!(asset %in% c("USD", "USDT"))) %>% 
        mutate(coin = paste(asset, "USD", sep = "")) %>% 
        mutate(yfin_coin = paste(asset, "-USD", sep = ""))
    
    bt <- binance_mytrades(symbol = bwallet$coin)
    bt2 <- bt %>% 
        mutate_at(vars(price:commission), as.numeric) %>% 
        group_by(order_id) %>% 
        summarise(symbol = unique(symbol), price = mean(price), qty = sum(qty), 
                  quote_qty = sum(quote_qty), commission = sum(commission), commission_asset = unique(commission_asset),
                  time = min(time), is_buyer = unique(is_buyer)) %>% 
        mutate(side = ifelse(is_buyer, 1, -1) * qty)
    
    bt2 <- bt2 %>% 
        arrange(time) %>% 
        left_join(select(bwallet, coin, yfin_coin), by = c("symbol" = "coin")) 
    
    saveRDS(bt2, file = "binance_trades.RDS")
    saveRDS(bwallet, file = "binance_wallet.RDS")
    message("Sync complete..")
}

create_blotter_portfolio <- function(p_name, trades, start_date = '2021-02-01') {
    currency("USD")
    coins <- unique(trades$yfin_coin)
    # coins <- c("BTC-USD", "ETH-USD", "ADA-USD", "XLM-USD", "BNB-USD", "LINK-USD", 
               # "ALGO-USD", "DOGE-USD", "VET-USD", "EOS-USD", "MATIC-USD", "VTHO-USD", 
               # "MANA-USD")
    for (coin in coins)
        stock(coin, currency = "USD", multiplier = 1)
    getSymbols(coins, from=start_date)
    
    try(rm(list=paste("portfolio.", p_name, sep = ""), pos=.blotter))
    initPortf(name = p_name, symbols = coins, initDate = start_date)
    
    trades <- trades %>% 
        dplyr::mutate(addT = pmap_lgl(list(.$yfin_coin, .$time, .$side, .$price), 
                                      function(s, d, a, p) {
                                          addTxn("binance", Symbol = s, TxnDate = d, TxnQty = a,
                                                 TxnPrice = p)
                                          return(T)
                                      }))
}


if (F) {
    updatePortf('binance')
    pt <- perTradeStats('binance', coins)
    pt <- pt %>% select(Symbol, End.Pos, Num.Txns, Max.Notional.Cost, Net.Trading.PL, MAE, Pct.MAE, MFE, Pct.MFE)
    
    p <- getPortfolio('binance')
    t2 <- lapply(coins, function(x) {
        tail(p$symbols[[x]]$txn, 1)
    })
    names(t2) <- coins
    t3 <- bind_rows(t2, .id = "Symbol")
    
    t3 <- t3 %>% select(Symbol, Pos.Avg.Cost)
    
    pt %>% 
        left_join(t3) %>% View
}

if(F) {
    th <- readxl::read_xlsx("~/Projects/misc/Trade_History)new.xlsx")
    th <- th %>% transmute("order_id" = 11000, symbol = Market, price = as.numeric(Price), 
                  qty = as.numeric(Amount), quote_qty = as.numeric(Total), 
                  commission = as.numeric(Fee),
                  commission_asset = `Fee Coin`, time = as.Date(`Date(UTC)`),
                  is_buyer = Type == "BUY", side = ifelse(is_buyer, yes = 1, no =  -1),
                  side = side * qty)
    
    th <- th %>% 
        left_join(select(bwallet, coin, yfin_coin), by = c("symbol" = "coin"))
    
    trades <- trades %>% 
        bind_rows(th)
}
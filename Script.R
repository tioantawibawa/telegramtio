#devtools::install_github("ebeneditos/telegram.bot")
library(telegram.bot)
library(forecast)
library(tidyverse)
library(rvest)
library(dplyr)
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots
library(ps)
library(xts)
library(stockPortfolio)

setwd("D:/R project/Telegram bot")

bot <- Bot(token = '634542972:AAEm9EiwIdCEyd4GCAWuCH63bt-TcKYeoWI')

updater <- Updater(token='634542972:AAEm9EiwIdCEyd4GCAWuCH63bt-TcKYeoWI')
dispatcher <- updater$dispatcher
  
  #1
  start <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id, text = "Selamat datang, dalam bot Uji Coba ATM cash management, Silahkan ketikan /cash<spasi>(Terminal ID),(jangka waktu) untuk mengetahui prediksi Cash withdrawal.")
  }
start_handler <- CommandHandler('start', start)
dispatcher$add_handler(start_handler)
#3
help1 <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, text = "Ini baru memfasilitasi prediksi ATM cash management")
  #    bot$sendMessage(chat_id = update$message$chat_id, text = paste("----------------\nNo id pengajuan : ","1","; Predicted score : ","2", "\n----------------\n"))
  #  bot$sendMessage(chat_id = update$message$chat_id, text = readline("masukan nama kolom yang berisi variabel tahun : "))
}

help1_handler <- CommandHandler('help', help1)
dispatcher$add_handler(help1_handler)
#3.1
lq45 <- function(bot, update){
  # Web-scrape S&P500 stock list
  sp_500 <- read_html("https://id.wikipedia.org/wiki/Indeks_LQ45") %>%
    html_node("table.wikitable") %>%
    html_table() %>%
    select(`Singkatan`,`Perusahaan`) %>%
    as_tibble()
  # Format names
  names(sp_500) <- sp_500 %>%
    names() %>%
    str_to_lower() %>%
    make.names()
  
  sp_500$singkatan <- paste0(sp_500$singkatan, '.JK')
  
  
  
  # Creating Functions to Map ----------------------------------------------------
  
  get_stock_prices <- function(ticker, return_format = "tibble", ...) {
    # Get stock prices
    stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
    # Rename
    names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
      stock_prices <- stock_prices_xts %>%
        as_tibble() %>%
        rownames_to_column(var = "Date") %>%
        mutate(Date = ymd(Date))
    } else {
      stock_prices <- stock_prices_xts
    }
    stock_prices
  }
  
  get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
    # Convert tibble to xts
    if (!is.xts(x)) {
      x <- xts(x[,-1], order.by = x$Date)
    }
    # Get stock prices
    log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
    # Rename
    names(log_returns_xts) <- "Log.Returns"
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
      log_returns <- log_returns_xts %>%
        as_tibble() %>%
        rownames_to_column(var = "Date") %>%
        mutate(Date = ymd(Date))
    } else {
      log_returns <- log_returns_xts
    }
    log_returns
  }
  
  
  # Mapping the Functions --------------------------------------------------------
  from <- "2017-01-01"
  sp_500 <- sp_500 %>%
    mutate(
      stock.prices = map(singkatan,
                         function(.x) get_stock_prices(.x,
                                                       return_format = "tibble",
                                                       from = from,
                                                       to   = today())
      ),
      log.returns  = map(stock.prices,
                         function(.x) get_log_returns(.x, return_format = "tibble")),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )
  
  
  # Visualizing the Results with Plotly ------------------------------------------
  pl <-  plot_ly(data   = sp_500,
                 type   = "scatter",
                 mode   = "markers",
                 x      = ~ sd.log.returns,
                 y      = ~ mean.log.returns,
                 color  = ~ n.trade.days,
                 colors = "Blues",
                 size   = ~ n.trade.days,
                 text   = ~ str_c("<em>", perusahaan, "</em><br>",
                                  "Ticker: ", singkatan),
                 marker = list(opacity = 0.8,
                               symbol = 'circle',
                               sizemode = 'diameter',
                               sizeref = 4.0,
                               line = list(width = 2, color = '#FFFFFF'))
  ) %>%
    layout(title   = 'Return vs Risiko',
           xaxis   = list(title = 'Risk: StDev Log Returns',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
           yaxis   = list(title = 'Return: Mean Log Returns',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
           margin = list(l = 100,
                         t = 100,
                         b = 100),
           font   = list(color = '#FFFFFF'),
           paper_bgcolor = 'rgb(0, 0, 0)',
           plot_bgcolor = 'rgb(0, 0, 0)')
  pl2 <- add_annotations(p = pl, data = sp_500,
                         x = sp_500$sd.log.returns,
                         y = sp_500$mean.log.returns,
                         text = str_c("<em>", sp_500$singkatan, "</em><br>",
                                      sp_500$perusahaan),
                         xref = "x",
                         yref = "y",
                         showarrow = TRUE,
                         arrowhead = 4,
                         arrowsize = .5,
                         ax = 20,
                         ay = -40)
  export(pl2, file = "D:/R project/Telegram bot/saham.jpeg")

  bot$sendPhoto(chat_id = update$message$chat_id,photo ='D:/R project/Telegram bot/saham.jpeg', caption = 'Grafik Prediksi Return dan Reward Saham LQ45')
  #    bot$sendMessage(chat_id = update$message$chat_id, text = paste("----------------\nNo id pengajuan : ","1","; Predicted score : ","2", "\n----------------\n"))
  #  bot$sendMessage(chat_id = update$message$chat_id, text = readline("masukan nama kolom yang berisi variabel tahun : "))
}

lq45_handler <- CommandHandler('lq45', lq45)
dispatcher$add_handler(lq45_handler)

#2
echo <- function(bot, update){
  awal <- toupper(paste(update$message$text, collapse = ' '))
  #awal <- "tio"
  basabasimatch <- c("HI", "HI,","HALO","HALLO","HELO","HELLO","CUY","BRO","BOS","GAN")
  kabarmatch <- c("APA KABAR","KABAR","KEADAAN","SEHAT","KEADAANMU")
  namamatch <- c("NAMAMU", "KAMU SIAPA", "KENALAN", "LO SIAPA", "KW SIAPA", "ANDA SIAPA","IKI SOPO","KOE SOPO","KOE SIAPA","SIAPA NAMA")
  kasparmatch <- c("KENAL PAK KASPAR", "KENAL KASPAR", "TAU KASPAR","TAU PAK KASPAR","KALAU PAK KASPAR")
  dodymatch <- c("KENAL PAK DODDY","KENAL PAK DODI","TAU PAK DODDY","TAU PAK DODI","KALAU PAK DODI","KALAU PAK DODDY","KLO PAK DODI","KALAU PAK DODDY")
  elvismatch <- c("KENAL PAK ELVIS", "KENAL ELVIS", "TAU ELVIS","TAU PAK ELVIS","KALAU PAK ELVIS","KALAU ELVIS")
  sapaanmatch1 <- c("GW","GUE")
  sapaanmatch2 <- c("LO","LU","LOE","LW")
  sapaanmatch3 <- c("KULA","SLIRA","SAMPEYAN","PANJENEGAN","KOE")
  sapaan1 <- grepl(paste(sapaanmatch1,collapse="|"),awal)
  sapaan2 <- grepl(paste(sapaanmatch2,collapse="|"),awal)
  sapaan3 <- grepl(paste(sapaanmatch3,collapse="|"),awal)
  if (sapaan1 == TRUE) {
    sapaan <- "gue"  
  } else if (sapaan2 == TRUE) {
    sapaan <- "gue"
  } else if (sapaan3 == TRUE){
    sapaan <- "kula"
  } else {
    sapaan <- "aku"
  }
  basabasi <- grepl(paste(basabasimatch,collapse="|"),awal)
  kabar <- grepl(paste(kabarmatch,collapse="|"),awal)
  nama <- grepl(paste(namamatch,collapse="|"),awal)
  kaspar <- grepl(paste(kasparmatch,collapse="|"),awal)
  doddy <- grepl(paste(dodymatch,collapse="|"),awal)
  elvis <- grepl(paste(elvismatch,collapse="|"),awal)
  if (kabar == TRUE) {
    final_name <- "Alhamdulillah, Sehat"  
  } else if (nama == TRUE) {
    final_name <- paste(sapaan, "tio")
  } else if (kaspar == TRUE) {
    final_name <- paste("kenal lah, kadiv ",sapaan,"itu")
  } else if (elvis == TRUE) {
    final_name <- "elvis yang botak itu kan ?"
  } else if (doddy == TRUE) {
    final_name <- paste("tauuu, tp ",sapaan,"takut di jump strike sama om dod")
  } else if (basabasi == TRUE) {
    final_name <- paste("Ngopoo ?")
  } else {
    url <- URLencode(paste0('https://www.google.com/search?q=', awal))
    tio <- read_html(url) %>% 
      html_nodes(".r") %>% 
      #    html_attr("href") %>%
      html_text()
    final_name <- gsub("[^0-9A-Za-z///' ]","" , tio[1] ,ignore.case = TRUE)
    final_name <- gsub("LinkedIn","" , final_name ,ignore.case = TRUE)
    final_name <- gsub("Youtube","" , final_name ,ignore.case = TRUE)
    final_name <- gsub("Facebook","" , final_name ,ignore.case = TRUE)  
    final_name <- gsub("Instagram","" , final_name ,ignore.case = TRUE)
    final_name <- gsub("Twitter","" , final_name ,ignore.case = TRUE)
    final_name <- gsub("Kaskus","" , final_name ,ignore.case = TRUE)
  }
  bot$sendMessage(chat_id = update$message$chat_id, text = final_name)
}
echo_handler <- MessageHandler(echo, Filters$text)
dispatcher$add_handler(echo_handler)
#4
cash <- function(bot, update, args){
  text_awal <- toupper(paste(args, collapse = ' '))
  text_awal <- as.data.frame(unlist(strsplit(text_awal, ",")))
  text_caps <- as.vector(text_awal[1,])
  if (is.na(text_awal[2,])==TRUE)
  {
    periode <- 7
  } else
  {
    periode <- as.numeric(as.character(text_awal[2,]))
  }
  nama <- paste0("D:/R project/Telegram bot/",text_caps,".csv")
  atm <- read.csv(nama)
  atm <- atm[,-1]
  library(lubridate)
  atm$Date <- ymd(atm$Date)
  colnames(atm)[2] <- "Withdrawals"
  library(xts)
  atm_ts <- xts(atm$Withdrawals, frequency = 7,order.by = ymd(atm$Date))
  set.seed(1)
  fit <- nnetar(atm_ts, lambda = 0.5)
  fcast <- forecast(fit,h=periode, level = c(90, 95))
  plot(fcast, xlab="Time", ylab="Nominal")
  lines(fcast$fitted, col="red")
  lines(fcast$mean, col="green", lty=2)
  dev.copy(png,'myplot.png')
  dev.off()
#============  
  cek <- as.data.frame(fcast)
  total <- round(sum(cek[,1]),0)
for (i in 1:10) {
  set.seed(1)
    fcast2 <- forecast(fit,h=i, level = c(90, 95))
    cek2 <- as.data.frame(fcast2)
    total2 <- round(sum(cek2[,1]),0)
    print(as.vector(i))
    if(total2 > 800000000) break
  }
  text_caps2 <- paste("----------------\nTotal Prediksi Jumlah Penarikan dalam",periode,"hari kedepan : Rp.",total, "\n----------------\n","\n Jika dilakukan pengisian sekarang, Kas ATM ini akan habis dalam",i-1,"hari ke depan")

    bot$sendPhoto(chat_id = update$message$chat_id,photo ='D:/R project/Telegram bot/myplot.png', caption = text_caps2)
}

cash_handler <- CommandHandler('cash', cash, pass_args = TRUE)
dispatcher$add_handler(cash_handler)
#==================   
#4.1
saham <- function(bot, update, args){
  text_caps <- toupper(paste(args, collapse = ' '))
  mySymbol = text_caps
 getSymbols(mySymbol, from="2018-01-01", to=Sys.Date())
  chartSeries(Cl(get(mySymbol)),name=mySymbol,TA='addBBands();
              addBBands(draw="p");
              addVo();
              addMACD()',
              theme="white")
  
  dev.copy(jpeg,'D:/R project/Telegram bot/new.jpeg')
  dev.off()          
  
  bot$sendPhoto(chat_id = update$message$chat_id, photo = 'D:/R project/Telegram bot/new.jpeg',caption = paste("informasi saham :",text_caps))
}

saham_handler <- CommandHandler('saham', saham, pass_args = TRUE)
dispatcher$add_handler(saham_handler)
#===============
#4.2
rw <- function(bot, update, args){
  text_caps <- toupper(paste(args, collapse = ' '))
  text_caps <- as.data.frame(unlist(strsplit(text_caps, ",")))
  simbol <- as.vector(text_caps[1,])
  if (is.na(text_caps[2,])==TRUE)
  {
    periode <- 7
  } else
  {
    periode <- as.numeric(as.character(text_caps[2,]))
  }
  mySymbol = substr(simbol,start = 1, stop = 7)
  getSymbols(mySymbol, from="2018-01-01", to=Sys.Date())
  logreturn <- get(mySymbol) %>%
    Ad() %>%
    dailyReturn(type = "log")
  names(logreturn) <- "logreturn" 
  mean_log_returns <- mean(logreturn, na.rm = TRUE)
  sd_log_returns <- sd(logreturn, na.rm = TRUE)
  N     <- periode  # Number of Stock Price Simulations
  M     <- 100  # Number of Monte Carlo Simulations   
  mu    <- mean_log_returns
  sigma <- sd_log_returns
  day <- 1:N
  price_init_table <- as.data.frame(get(mySymbol))
  price_init <- price_init_table[nrow(price_init_table),6]
  # Simulate prices
  set.seed(123)
  monte_carlo_mat <- matrix(nrow = N, ncol = M)
  for (j in 1:M) {
    monte_carlo_mat[[1, j]] <- price_init
    for(i in 2:N) {
      monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
    }
  }
  # Format and organize data frame
  price_sim <- cbind(day, monte_carlo_mat) %>%
    as_tibble() 
  nm <- str_c("Sim.", seq(1, M))
  nm <- c("Day", nm)
  names(price_sim) <- nm
  price_sim <- price_sim %>%
    gather(key = "Simulation", value = "Stock.Price", -(Day))
  # Visualize simulation
  
  gg <- ggplot(data = price_sim,aes(x = Day, y = Stock.Price, Group = Simulation))+
    geom_line(alpha = 0.6,color="blue") +
    ggtitle(str_c("MA: ", M, 
                  " Monte Carlo Simulations for Prices Over ", N, 
                  " Trading Days")) 
  path1 <- paste0("D:/R project/Telegram bot/",substr(mySymbol,1,4),".png")
  ggsave(path1,plot = gg)  
  end_stock_prices <- price_sim %>% 
    filter(Day == max(Day))
  probs <- c(.005, .025, .25, .5, .75, .975, .995)
  dist_end_stock_prices <- round(quantile(end_stock_prices$Stock.Price, probs = probs),0)
  bawah <- dist_end_stock_prices[1]
  atas <- dist_end_stock_prices[7]

  bot$sendPhoto(chat_id = update$message$chat_id, photo = path1,caption = paste("Prediksi saham :",simbol,"given from the result of my simulation, stock may reach the price of Rp.",atas," in",periode,"days or crash to a Rp.",bawah,"low."))
}

rw_handler <- CommandHandler('prediksi', rw, pass_args = TRUE)
dispatcher$add_handler(rw_handler)

#uji coba
coba <- function(bot, update, args){
  text_caps <- toupper(paste(args, collapse = ' '))
  bot$sendMessage(chat_id = update$message$chat_id,text =  text_caps)
}

coba_handler <- CommandHandler('coba', coba, pass_args = TRUE)
dispatcher$add_handler(coba_handler)
#5
unknown <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Ngapuntene, command menika mboten dimangertosi")
}

unknown_handler <- MessageHandler(unknown, Filters$command)
dispatcher$add_handler(unknown_handler)

updater$start_polling()

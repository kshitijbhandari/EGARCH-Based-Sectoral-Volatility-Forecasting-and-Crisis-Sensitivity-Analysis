zip_file <- "C:\\lehigh_academics\\Homework\\time series forecasting\\project\\archive (1).zip"
# List files inside zip
files <- unzip(zip_file, list = TRUE)$Name

# Read all CSVs into a named list
data_list <- lapply(files, function(f) {
  read.csv(unz(zip_file, f))
})

# Name each list element by file name
names(data_list) <- gsub(".csv", "", files)

# Example: access Nifty IT data
sector_IT = data_list$"NIFTY IT"
head(sector_IT)

sector_bank = data_list$"NIFTY BANK"
head(sector_bank)

sector_pharma = data_list$"NIFTY PHARMA"
head(sector_pharma)

sector_fmcg = data_list$"NIFTY FMCG"
head(sector_fmcg)

sector_metal = data_list$"NIFTY METAL"
head(sector_fmcg)

india_vix = data_list$"INDIAVIX"
head(india_vix)
  

sector_list <- list(
  IT     = sector_IT,
  BANK   = sector_bank,
  PHARMA = sector_pharma,
  FMCG   = sector_fmcg,
  METAL  = sector_metal,
  VIX    = india_vix
)


par(mfrow = c(1,1)) 

for(name in names(sector_list)) {
  
  df <- sector_list[[name]]
  
  df$Date <- as.Date(df$Date)
  
  plot(df$Date, df$Close, type = "l",
       main = paste("Time Series -", name),
       xlab = "Year", ylab = "Index Level")
}


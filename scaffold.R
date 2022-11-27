dir = c("R","data", "docs", "figures", "data/raw", "data/cache")

lapply(dir, function(x) if(!dir.exists(x)) dir.create(x))

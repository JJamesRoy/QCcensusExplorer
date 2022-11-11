R = "./R"
raw = "./data/raw"
data = "./data"
docs = "./docs"
figures = "./figures"
dir = c(R,data, docs, figures, raw)

lapply(dir, function(x) if(!dir.exists(x)) dir.create(x))

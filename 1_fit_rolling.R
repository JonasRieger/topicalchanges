library(rollinglda)
library(data.table)
library(readxl)
library(tm)

news_cnn = as.data.table(read_excel(file.path("data", "news_cnn.xlsx")))
news_cnn[, id := paste0("news", ...1)]

tmp = as.POSIXct(strptime(news_cnn$date, "%Y-%m-%d %H:%M"), tz = "GMT")
news_cnn[, posix := tmp]
news_cnn[, date_new := as.Date(posix)]
news_cnn[, date_old := date]
news_cnn[, date := date_new]
news_cnn[, date_new := NULL]
setkeyv(news_cnn, "posix")
saveRDS(news_cnn, file.path("data", "news_cnn.rds"))

lemma = fread(file.path("data", "lemmatization-en.txt"))
colnames(lemma) = c("base", "variant")
lemma[, base := tolower(base)]
lemma[, variant := tolower(variant)]
lemma = lemma[- (variant == "data"), ]

texts = trimws(stripWhitespace(
  removeWords(
    removeNumbers(
      removePunctuation(
        removePunctuation(
          removeWords(
            tolower(news_cnn$text), stopwords())), ucp = TRUE)), stopwords())))

names(texts) = news_cnn$id
texts = sapply(texts, function(x) strsplit(x, "\\s")[1])
texts = sapply(texts, function(x) x[nchar(x) > 1])

texts = lapply(texts, function(x){
  tmp = x %in% lemma$variant
  if(any(tmp)) x[tmp] = lemma$base[match(x[tmp], lemma$variant)]
  x
})
texts = lapply(texts, function(x) x[!x %in% stopwords()])
texts = lapply(texts, function(x){
  removeNumbers(x)
  x[nchar(x) > 1]
})

dates = news_cnn$date

set.seed(20220112)

dir.create("roll")
for(k in 8:20){
  roll = RollingLDA(texts, dates,
                    chunks = "week",
                    memory = "week",
                    memory.fallback = 10,
                    init = "2020-01-31",
                    K = k)
  saveRDS(roll, file.path("roll", paste0(k, ".rds")))
}

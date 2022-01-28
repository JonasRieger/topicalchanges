library(rollinglda)
library(data.table)
library(readxl)
library(tm)

news_guardian = as.data.table(read_excel(file.path("data", "news_guardian.xlsx")))
news_guardian[, id := paste0("news", ...1)]

tmp = as.POSIXct(strptime(news_guardian$date, "%Y-%m-%d %H:%M"), tz = "GMT")
news_guardian[, posix := tmp]
news_guardian[, date_new := as.Date(posix)]
news_guardian[, date_old := date]
news_guardian[, date := date_new]
news_guardian[, date_new := NULL]
setkeyv(news_guardian, "posix")
saveRDS(news_guardian, file.path("data", "news_guardian.rds"))

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
            tolower(news_guardian$text), stopwords())), ucp = TRUE)), stopwords())))

names(texts) = news_guardian$id
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

dates = news_guardian$date

set.seed(20220112)

k = 12
roll = RollingLDA(texts, dates,
                  chunks = "week",
                  memory = "week",
                  memory.fallback = 10,
                  init = "2020-01-31",
                  K = k)
saveRDS(roll, file.path("roll", paste0(k, "_guardian.rds")))

library(rollinglda)
library(data.table)
library(readxl)
library(tm)
library(tosca)
library(ggplot2)
library(GGally)

dir.create("publico")

news_publico = as.data.table(read_excel(file.path("data", "news_publico.xlsx")))
news_publico[, id := paste0("news", ...1)]

tmp = as.POSIXct(strptime(news_publico$date, "%Y-%m-%d %H:%M"), tz = "GMT")
news_publico[, posix := tmp]
news_publico[, date_new := as.Date(posix)]
news_publico[, date_old := date]
news_publico[, date := date_new]
news_publico[, date_new := NULL]
setkeyv(news_publico, "posix")
saveRDS(news_publico, file.path("data", "news_publico.rds"))

lemma = fread(file.path("data", "lemmatization-pt.txt"))
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
            tolower(news_publico$text), stopwords())), ucp = TRUE)), stopwords())))

names(texts) = news_publico$id
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

dates = news_publico$date

set.seed(20220112)

k = 12
roll = RollingLDA(texts, dates,
                  chunks = "week",
                  memory = "week",
                  memory.fallback = 10,
                  init = "2020-01-31",
                  K = k)
saveRDS(roll, file.path("roll", paste0(k, "_publico.rds")))

## sim, quantile
K = getK(getLDA(roll))
eta = getEta(getLDA(roll))
docs = getDocs(roll)
vocab = getVocab(roll)
assignments = getAssignments(getLDA(roll))
topics_chunks = lapply(roll$chunks$chunk.id, function(x){
  limits = roll$chunks[chunk.id == x]
  tmp = table(factor(unlist(assignments[roll$dates >= limits$start.date & roll$dates <= limits$end.date])+1, levels = 1:K), 
              factor(unlist(lapply(docs[roll$dates >= limits$start.date & roll$dates <= limits$end.date], function(y) y[1,]))+1, levels = seq_len(length(vocab))))
  tmp = matrix(as.integer(tmp), nrow = K)
  colnames(tmp) = vocab
  tmp
})
phi_chunks = lapply(topics_chunks, function(topics)
  (topics + eta)/(rowSums(topics) + ncol(topics) * eta))

topwords = apply(topWords(getTopics(getLDA(roll)), 5), 2, paste, collapse = "_")
topwords_chunks = lapply(topics_chunks, topWords, numWords = 50)
topwords_chunks = lapply(1:K, function(k) sapply(seq_along(topwords_chunks), function(t) topwords_chunks[[t]][,k]))
dir.create(file.path("publico", "topWordsPerChunk"))
for(i in 1:K){
  out = topwords_chunks[[i]]
  colnames(out) = as.character(roll$chunks$end.date)
  write.csv(out,
            file = file.path("publico", "topWordsPerChunk", paste0(i, "_", topwords[i], ".csv")),
            fileEncoding = "UTF-8")
}

topics = lapply(1:K, function(k){
  tmp = sapply(topics_chunks, function(x) x[k,])
  colnames(tmp) = paste0("Chunk", roll$chunks$chunk.id)
  tmp
})

cosine = function(a, b) sum(a*b) / sqrt(sum(a^2)) / sqrt(sum(b^2))
nchunks = nrow(roll$chunks)

z = 4
for(rel in c(0.8, 0.85)){
  q = rel
  sim1 = quantiles1 = matrix(NA_real_, ncol = K, nrow = nchunks)
  run_length1 = integer(K)
  for(i in seq_len(nchunks)[-1]){
    run_length1 = run_length1 + 1
    z1 = pmin(run_length1, z)
    
    limits = roll$chunks[i]
    tab = table(factor(unlist(assignments[roll$dates >= limits$start.date &
                                            roll$dates <= limits$end.date])+1, levels = 1:K))
    for(k in seq_len(K)){
      topics_run = rowSums(topics[[k]][,max(1,i-z1[k]):(i-1), drop = FALSE])
      sim1[i, k] = cosine(topics[[k]][,i], topics_run)
      topics_tmp = Reduce("+", topics_chunks[max(1,i-z1[k]):(i-1)]) + eta
      phi = topics_tmp / rowSums(topics_tmp)
      topics_tmp = topics_chunks[[i]] + eta
      phi_tmp = phi[k,] 
      phi = topics_tmp / rowSums(topics_tmp)
      phi_tmp = (1-rel)*phi_tmp + rel*phi[k,]
      quantiles = replicate(500, {
        topics_resampled = tabulate(
          sample(length(vocab),
                 size = tab[k],
                 replace = TRUE,
                 prob = phi_tmp), nbins = length(vocab))
        cosine(topics_resampled, topics_run)
      })
      quantiles1[i, k] = quantile(quantiles, 0.01)
    }
    run_length1[sim1[i,] < quantiles1[i,]] = 0L
  }
  saveRDS(quantiles1, file.path("publico", paste0("quantiles", rel, ".rds")))
  saveRDS(sim1, file.path("publico", paste0("sim", rel, ".rds")))
  
  # wordimpact
  quantiles = quantiles1
  sim = sim1
  events_end = apply(sim < quantiles, 2, function(d) roll$chunks$end.date[which(d)])
  events_start = apply(sim < quantiles, 2, function(d) roll$chunks$start.date[which(d)])
  events_ind = apply(sim < quantiles, 2, function(d) which(d))
  
  loo = lapply(seq_along(events_ind), function(k){
    if(length(events_ind[[k]]) > 0){
      loo = sapply(events_ind[[k]], function(i){
        tmp = rowSums(topics[[k]][,max(1,i-8):(i-1), drop = FALSE])
        loo = cosine(topics[[k]][,i], tmp) -
          sapply(seq_len(length(vocab)), function(j) cosine(topics[[k]][,i][-j], tmp[-j]))
      })
      rownames(loo) = vocab
      colnames(loo) = as.character(events_ind[[k]])
    }else loo = NULL
    loo
  })
  
  pdf(file.path("publico", paste0("wordimpact", rel, ".pdf")), height = 8, width = 10)
  for(k in seq_len(K)){
    zaehler = 0
    for(i in colnames(loo[[k]])){
      zaehler = zaehler + 1
      tmp = c(head(sort(loo[[k]][,i]), 10), tail(sort(loo[[k]][,i]), 3))
      print(ggplot() +
              geom_bar(aes(x = reorder(names(tmp), tmp), y = tmp), stat = "identity") +
              xlab("") + ylab("Impact on Cosine Similarity") +
              ggtitle(paste0(events_start[[k]][zaehler], " - ", events_end[[k]][zaehler], ", Topic ", k, ": ", topwords[k])))
    }
  }
  dev.off()
  
  ## changes
  nr = 4; nc = 3
  chunk_dates = roll$chunks$end.date
  events = data.table(k = rep(seq_along(events_end), lengths(events_end)),
                      start = as.Date(unlist(lapply(events_start, as.character))),
                      end = as.Date(unlist(lapply(events_end, as.character))))
  fwrite(events, file = file.path("publico", paste0("changes", rel, ".csv")))
  
  pdf(file.path("publico", paste0("changes", rel, ".pdf")), height = 8, width = 8)
  print(ggmatrix(lapply(1:K, function(i){
    ggplot() + ylim(c(-0.1,1)) +
      geom_vline(xintercept = events_end[[i]], col = "darkgrey") +
      geom_line(aes(x = chunk_dates, y = quantiles[,i], col = "1")) +
      geom_line(aes(x = chunk_dates, y = sim[,i], col = "3")) +
      annotate("text", x = min(chunk_dates), y = -0.08, label = paste0(i, "_", topwords[i]), hjust = 0, vjust = 0, cex = 3)
  }), nrow = nr, ncol = nc, ylab = "Cosine Similarity", title = paste0("Backward looking reference period of length ", z, " | mix = ", q)))
  dev.off()
}
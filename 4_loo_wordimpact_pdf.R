
library(data.table)
library(rollinglda)
library(tosca)
library(ggplot2)
library(GGally)

K = 12
z = 4
q = 0.85

roll = readRDS(file.path("roll", paste0(K, ".rds")))
vocab = getVocab(roll)
topwords = apply(topWords(getTopics(getLDA(roll)), 5), 2, paste, collapse = "_")
eta = getEta(getLDA(roll))
docs = getDocs(roll)
assignments = getAssignments(getLDA(roll))
topics_chunks = lapply(roll$chunks$chunk.id, function(x){
  limits = roll$chunks[chunk.id == x]
  tmp = table(factor(unlist(assignments[roll$dates >= limits$start.date & roll$dates <= limits$end.date])+1, levels = 1:K), 
              factor(unlist(lapply(docs[roll$dates >= limits$start.date & roll$dates <= limits$end.date], function(y) y[1,]))+1, levels = seq_len(length(vocab))))
  tmp = matrix(as.integer(tmp), nrow = K)
  colnames(tmp) = vocab
  tmp
})
topics = lapply(1:K, function(k){
  tmp = sapply(topics_chunks, function(x) x[k,])
  colnames(tmp) = paste0("Chunk", roll$chunks$chunk.id)
  tmp
})
cosine = function(a, b) sum(a*b) / sqrt(sum(a^2)) / sqrt(sum(b^2))

sim = readRDS(file.path("sim", K, z, paste0("sim", q, ".rds")))
quantiles = readRDS(file.path("sim", K, z, paste0("quantiles", q, ".rds")))

events_end = apply(sim < quantiles, 2, function(d) roll$chunks$end.date[which(d)])
events_start = apply(sim < quantiles, 2, function(d) roll$chunks$start.date[which(d)])
events_ind = apply(sim < quantiles, 2, function(d) which(d))

loo = lapply(seq_along(events_ind), function(k){
  if(length(events_ind[[k]]) > 0){
    loo = sapply(events_ind[[k]], function(i){
      tmp = rowSums(topics[[k]][,max(1,i-z):(i-1), drop = FALSE])
      loo = cosine(topics[[k]][,i], tmp) -
        sapply(seq_len(length(vocab)), function(j) cosine(topics[[k]][,i][-j], tmp[-j]))
    })
    rownames(loo) = vocab
    colnames(loo) = as.character(events_ind[[k]])
  }else loo = NULL
  loo
})

pdf("wordimpact_cnn.pdf", height = 8, width = 10)
for(k in seq_len(K)){
  zaehler = 0
  for(i in colnames(loo[[k]])){
    zaehler = zaehler + 1
    #tmp = c(head(sort(loo[[k]][,i]), 10), tail(sort(loo[[k]][,i]), 3))
    tmp = head(sort(loo[[k]][,i]), 15)
    prev = rowSums(topics[[k]][,max(1,as.integer(i)-z):(as.integer(i)-1), drop = FALSE])
    prev = (prev/sum(prev))[names(tmp)]
    now.tot = topics[[k]][,as.integer(i)]
    now = (now.tot/sum(now.tot))[names(tmp)]
    now.tot = now.tot[names(tmp)]
    print(ggplot() +
            geom_bar(aes(x = reorder(names(tmp), tmp), y = tmp, fill = now-prev > 0), stat = "identity") +
            xlab("") + ylab("Impact on Cosine Similarity") +
            ggtitle(paste0(events_start[[k]][zaehler], " - ", events_end[[k]][zaehler], ", Topic ", k, ": ", topwords[k])) +
            geom_text(aes(label = now.tot, y = min(tmp)*0.01, x = names(tmp)), vjust = 1) +
            theme(legend.position = "none"))
  }
}
dev.off()


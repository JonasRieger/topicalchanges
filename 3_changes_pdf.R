library(data.table)
library(rollinglda)
library(tosca)
library(ggplot2)
library(GGally)

for(K in 8:20){
  roll = readRDS(file.path("roll", paste0(K, ".rds")))
  topwords = apply(topWords(getTopics(getLDA(roll)), 5), 2, paste, collapse = "_")
  chunk_dates = roll$chunks$end.date
  
  nr = 5
  nc = 4
  if(K < 17) nr = nc = 4
  if(K < 16){nr = 5; nc = 3}
  if(K < 13){nr = 4; nc = 3}
  if(K < 10) nr = nc = 3
  
  for(q in seq(0.8, 0.9, 0.01)){
    pdf(file.path("results", K, paste0("changes", q, ".pdf")), height = 8, width = 8)
    for(z in 1:4){
      dir.create(file.path("results", K, z))
      sim = readRDS(file.path("sim", K, z, paste0("sim", q, ".rds")))
      quantiles = readRDS(file.path("sim", K, z, paste0("quantiles", q, ".rds")))
      
      events_end = apply(sim < quantiles, 2, function(d) roll$chunks$end.date[which(d)])
      
      if(length(events_end) > 0){
        events_start = apply(sim < quantiles, 2, function(d) roll$chunks$start.date[which(d)])
        events = data.table(k = rep(seq_along(events_end), lengths(events_end)),
                            start = as.Date(unlist(lapply(events_start, as.character))),
                            end = as.Date(unlist(lapply(events_end, as.character))))
        fwrite(events, file = file.path("results", K, z, paste0("changes", q, ".csv")))
        
        print(ggmatrix(lapply(1:K, function(i){
          ggplot() + ylim(c(-0.1,1)) +
            geom_vline(xintercept = events_end[[i]], col = "darkgrey") +
            geom_line(aes(x = chunk_dates, y = quantiles[,i], col = "1")) +
            geom_line(aes(x = chunk_dates, y = sim[,i], col = "3")) +
            annotate("text", x = min(chunk_dates), y = -0.08, label = paste0(i, "_", topwords[i]), hjust = 0, vjust = 0, cex = 3)
        }), nrow = nr, ncol = nc, ylab = "Cosine Similarity", title = paste0("Backward looking reference period of length ", z, " | mix = ", q)))
      }else{
        print(ggmatrix(lapply(1:K, function(i){
          ggplot() + ylim(c(-0.1,1)) +
            geom_line(aes(x = chunk_dates, y = quantiles[,i], col = "1")) +
            geom_line(aes(x = chunk_dates, y = sim[,i], col = "3")) +
            annotate("text", x = min(chunk_dates), y = -0.08, label = paste0(i, "_", topwords[i]), hjust = 0, vjust = 0, cex = 3)
        }), nrow = 4, ncol = 3, ylab = "Cosine Similarity", title = paste0("Backward looking reference period of length ", z, " | mix = ", q)))
      }
    }
    dev.off()
  }
  
}
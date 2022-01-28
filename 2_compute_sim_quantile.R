library(data.table)
library(rollinglda)
library(tosca)
library(batchtools)

reg = makeExperimentRegistry(packages = c("rollinglda", "tosca", "data.table"))

for(K in 8:20){
  addProblem(paste0("roll", K), data = K)
}

addAlgorithm("algo",
             fun = function(job, data, instance, seed, ...){
               K = data
               roll = readRDS(file.path("roll", paste0(K, ".rds")))
               
               dir.create(file.path("sim", K))
               dir.create(file.path("results", K))
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
               dir.create(file.path("results", K, "topWordsPerChunk"))
               for(i in 1:K){
                 out = topwords_chunks[[i]]
                 colnames(out) = as.character(roll$chunks$end.date)
                 write.csv(out,
                           file = file.path("results", K, "topWordsPerChunk", paste0(i, "_", topwords[i], ".csv")),
                           fileEncoding = "UTF-8")
               }
               
               topics = lapply(1:K, function(k){
                 tmp = sapply(topics_chunks, function(x) x[k,])
                 colnames(tmp) = paste0("Chunk", roll$chunks$chunk.id)
                 tmp
               })
               
               cosine = function(a, b) sum(a*b) / sqrt(sum(a^2)) / sqrt(sum(b^2))
               nchunks = nrow(roll$chunks)
               
               for(z in 1:4){
                 dir.create(file.path("sim", K, z))
                 for(rel in seq(0.8, 0.9, 0.01)){
                   sim1 = quantiles1 = matrix(NA_real_, ncol = K, nrow = nchunks)
                   run_length1 = integer(K)
                   for(i in seq_len(nchunks)[-1]){
                     run_length1 = run_length1 + 1
                     z1 = pmin(run_length1, z)
                     
                     limits = roll$chunks[i]
                     tab = table(factor(unlist(assignments[roll$dates >= limits$start.date &
                                                             roll$dates <= limits$end.date])+1, levels = 1:K))
                     for(k in seq_len(K)){
                       # 0.01
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
                   saveRDS(quantiles1, file.path("sim", K, z, paste0("quantiles", rel, ".rds")))
                   saveRDS(sim1, file.path("sim", K, z, paste0("sim", rel, ".rds")))
                 }
               }
             })

addExperiments()
submitJobs(resources = list(walltime = 8*60*60, memory = 1024*8))




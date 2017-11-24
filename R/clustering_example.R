
num_data = big_table[, sapply(big_table, class) %in% c('integer', 'numeric')]
full_data = na.omit(num_data)


candp = select(full_data, DOC, PTL)
cl = kmeans(candp, 3)
plot(candp)
plot(candp, col = cl$cluster)


ngroups = 2:10
sse = c()
for(i in seq_along(ngroups)){
  
  candp = select(full_data, DOC, PTL, NTL, CHLA, DEPTHMAX) %>% filter(DOC < 30 & PTL < 400)
  cl = kmeans(candp, ngroups[i])
  sse[i] = cl$withinss
  # plot(candp[,1:2])
  # plot(candp, col = cl$cluster)
  # points(cl$centers, col = 1:2, pch = 8, cex = 2)

}

plot(ngroups, sse)



candp = select(full_data, DOC, PTL, NTL, CHLA) %>% filter(DOC < 30 & PTL < 400) 
candp = as.data.frame(scale(candp))
cl = kmeans(candp, 4)
sse[i] = cl$withinss
 plot(candp[,1:2])
 plot(candp, col = cl$cluster)
# points(cl$centers, col = 1:2, pch = 8, cex = 2)
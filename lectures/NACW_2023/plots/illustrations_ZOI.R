rm(list=ls())

require(TuktuTools)
source("code/Elie/ZOI_sim_functions.R")

load("code/Elie/results/BCRWwithBarrier.rda")
barrier.short <- c(-2 + 1i, 2 + -1i)


eval <- FALSE
if(eval){
  Z.ZOI.list <- list()
  ds <- seq(.25,10,.25)
  for(i in 1:7){
    print(i)
    ds <- seq(.25,10,.25)
    Z.ZOI.list[[i]] <- alply(Z.sims[[i]], 1, getZOI.loglike,
          distances = ds, 
          barrier = barrier.short, 
          world = NULL, 
          n.samples = 10, habitat = FALSE)
    save(Z.ZOI.list, file = "code/Elie/results/Z.ZOI.list.rda")
  }
  save(Z.ZOI.list, file = "code/Elie/results/Z.ZOI.list.rda")
  
}  else load("code/Elie/results/Z.ZOI.list.rda")



ZOI.summary.df <- ldply(Z.ZOI.list, function(l){ 
  ldply(l) %>% group_by(distance) %>% 
    summarize(ll.mean = mean(logLike.rel),
              ll.se = sd(logLike.rel)/sqrt(length(logLike.rel)))
  })


png("presentation/images/ZOI/ZOInull.png", width = 1200, height = 600, res = 200)
ZOI.summary.df %>% 
  mutate(kappa = .id) %>%
  subset(distance > 0 & distance < 5 & 
                            .id %in% c(0,.05,.1,1)) %>% 
  ggplot(aes(distance, ll.mean, 
                          ymin = ll.mean - 2*ll.se, 
                          ymax = ll.mean + 2*ll.se)) + 
  geom_point() + geom_path() + geom_linerange() + 
  facet_grid(.~kappa, labeller = labeller(.cols = label_both)) + 
  ylab("log-Likelihood") + 
  xlab("distance from barrier")
dev.off()



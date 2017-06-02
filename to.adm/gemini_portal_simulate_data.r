# ----------------- Simulated data for GEMINI Portal demo ----------------------

set.seed(200)
N <- sample(100:1000, 10, replace = T)

patient <- rep(1:10, N)
cohort <- data.frame(EncID.new = 1: length(patient), physician = patient)
cohort$Acute.LoS = rexp(nrow(cohort), 0.5)

hist(cohort$Acute.LoS)
ddply(cohort, ~physician, summarize,
      ave.acute.los = mean(Acute.LoS)) %>% 
  ggplot(aes(x = physician, y = ave.acute.los)) + geom_bar(stat = "identity",
                                                           width = 0.5)

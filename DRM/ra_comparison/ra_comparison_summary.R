# -------------------------- Reviewer Comparison -------------------------------
rm(list = ls())
library(gemini)
lib.pa()
dat <- fread("H:/GEMINI/Results/DRM/REDCap/GEMINIDRMTEAM_DATA_2017-05-25_1134.csv")
dat <- dat[order(gemini_id)] %>% filter(!(is.na(date)|date=="")) %>% data.table

# summary of variables
count.discrep <- function(x){
  apply(x, 2, function(x)length(unique(x)))
}
var_sum <- ddply(dat, ~gemini_id,
                 count.discrep)
var_sum <- cbind(gemini_id = unique(dat$gemini_id),
                 var_sum)
result.sum <- data.table(variable_name = names(var_sum)[9:33],
                    Number_of_patient_with_discrepancy = 
                      apply(var_sum[,9:33], 2, function(x) sum(x>1)))

patient.sum <- data.table(gemini_id = var_sum$gemini_id,
                          Number_of_variables_with_discrepancy = 
                            apply(var_sum[, 9:33], 1, function(x)sum(x>1)))



# heat map
df <- var_sum[, c(1, 9:33)]
df.long <- melt(df, id.var = "gemini_id")
ggplot(df.long, aes( factor(gemini_id), variable, fill = value)) + 
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = value)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  scale_y_discrete(labels = names(df)[2:26])

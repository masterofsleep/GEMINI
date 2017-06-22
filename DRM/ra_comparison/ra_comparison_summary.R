# -------------------------- Reviewer Comparison -------------------------------
rm(list = ls())
library(gemini)
lib.pa()
dat <- fread("H:/GEMINI/Results/DRM/REDCap/GEMINIDRMTEAM_DATA_2017-05-25_1134.csv")
dat <- fread("H:/GEMINI/Results/DRM/REDCap/GEMINIDRMTEAM_DATA_2017-06-02_1030.csv")
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
df <- var_sum[, c(1, 9:30)]
df.long <- melt(df, id.var = "gemini_id")
png("R:/GEMINI-DRM-TEAM/ra_comparison/heatmap_june2.png", res = 200, width = 2000, height = 1600)
ggplot(df.long, aes( factor(gemini_id), variable, fill = value)) + 
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = value)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank()) +
  scale_y_discrete(labels = names(df)[2:26])
dev.off()






# -------------------------------- june20 --------------------------------------
dat <- readxl::read_excel("H:/GEMINI/Results/DRM/REDCap/GEMINIDRMTEAM_DATA_LABELS_Overlapping.xlsx")
dat <- data.table(dat)
dat <- dat[!is.na(`GEMINI ID:`)&!is.na(`Research Assistant:`)][order(`GEMINI ID:`)]
dat <- dat[`GEMINI ID:`%in% dat[, .N, by = `GEMINI ID:`][N>1, `GEMINI ID:`]]
names(dat)


ddply(dat, ~`GEMINI ID:`, function(x){
  df1 <- data.frame(
    N.reviewer = nrow(x),
    reviewer1 = x$`Research Assistant:`[1],
    reviewer2 = x$`Research Assistant:`[2]
  )
  df2 <- apply(x, 2, function(x)length(unique(x)))
  cbind(df1, df2)
  }
)


df2 <- ddply(dat, ~`GEMINI ID:`, 
      function(x)apply(x, MARGIN = 2, FUN = function(xx)length(unique(xx))))

df1 <- ddply(dat, ~`GEMINI ID:`, function(x){
  data.frame(
  N.reviewer = nrow(x),
  reviewer1 = x$`Research Assistant:`[1],
  reviewer2 = x$`Research Assistant:`[2]
)})

res <- cbind(df1, df2[, c(7: 28)])

fwrite(res, "H:/GEMINI/Results/DRM/REDCap/Summary_of_agreements_in_overlaps.csv")


# correlation between vars June 21
var_to_check <- names(dat)[c(8,10,13,15,17,19,23,25,27)]
dat <- data.frame(dat)
find_var_cor <- function(varname){
  compare <- merge(dat[!duplicated(dat$GEMINI.ID.)&!is.na(dat[, varname]), 
                       c("GEMINI.ID.", varname)],
                   dat[duplicated(dat$GEMINI.ID.)&!is.na(dat[, varname]),
                       c("GEMINI.ID.", varname)], by = "GEMINI.ID.")
  correlation  <- cor(compare[, 2], compare[,3])
  p.value = cor.test(compare[, 2], compare[,3])$p.value
  return(data.frame(correlation, p.value))
}

corr <- NULL
for(i in var_to_check){
  corr <- rbind(corr, find_var_cor(i))
}
cbind(var_to_check, corr) %>% fwrite("H:/GEMINI/Results/DRM/REDCap/correlations.csv")


# ------------------------- antibiotics data -----------------------------------
dat <- fread("R:/GEMINI-DRM-TEAM/Data Extracts/antibiotics-review.csv")
dat <- dat[GEMINI_ID%in% dat[,.N, by = .(GEMINI_ID, Event.Name.x)][N>1, GEMINI_ID]]


df2 <- ddply(dat, ~GEMINI_ID + Event.Name.x, 
             function(x)apply(x, MARGIN = 2, FUN = function(xx)length(unique(xx))))

df1 <- ddply(dat, ~GEMINI_ID + Event.Name.x, function(x){
  df1 <- data.frame(
    N.reviewer = nrow(x),
    reviewer1 = x$Research.Assistant[1],
    reviewer2 = x$Research.Assistant[2]
  )
})

res <- cbind(df1, df2[, c(4: 12)])
res <- data.frame(res, stringsAsFactors = F)
fwrite(res, "H:/GEMINI/Results/DRM/REDCap/Summary_of_agreements_in_overlaps_antibiotics.csv")


n.reviewered <- c(as.character(res[!is.na(res$reviewer2), "reviewer1"]),
                  as.character(res[!is.na(res$reviewer2), "reviewer2"])) %>% table %>% data.frame

melt(res[!is.na(res$reviewer2),], id.vars = c("reviewer1", "reviewer2"),
     measure.vars = names(res)[6:14])  %>% data.table -> check
n.dis <- c(as.character(check[value==2, reviewer1]),
  as.character(check[value==2, reviewer2])) %>% table %>% data.frame
names(n.dis) <- c("Research.Assistant", "Number.of.Discrepancy")
fwrite(n.dis, "H:/GEMINI/Results/DRM/REDCap/number.of.discrepancy.antibiotics.csv")

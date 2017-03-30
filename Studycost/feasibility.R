# ==============================================================================
# =========================  Studying Cost =====================================
# ==============================================================================
rm(list = ls())
library(gemini)
lib.pa()

sbk.rad <- readg(sbk, rad.csv)
sbk.echo <- readg(sbk, echo)
sbk.lab <- rbind(readg(sbk, labs_ip),
                 readg(sbk, labs_er))
price.list1 <- readxl::read_excel("H:/GEMINI/Results/Studycost/PriceList Labs DI Pharmacy FY1516.xlsx",
                                 sheet = 1, skip = 1)
price.list2 <- readxl::read_excel("H:/GEMINI/Results/Studycost/PriceList Labs DI Pharmacy FY1516.xlsx",
                                  sheet = 2, skip = 1)

names(price.list1)[6:11] <- price.list1[1, 6:11]
price.list1 <- price.list1[2:nrow(price.list1), ] %>% data.table
names(price.list2)[6:11] <- price.list2[1, 6:11]                                 
price.list2 <- price.list2[2:nrow(price.list2), ] %>% data.table
price.list1 <- price.list1[!is.na(Department)]
price.list2 <- price.list2[!is.na(Department)]

lab.notinlist <- fread("H:/GEMINI/Results/Studycost/lab names notinpricelist_ASW_AV.csv")
rad.notinlist <- fread("H:/GEMINI/Results/Studycost/rad names notinpricelist_ASW.csv")



#check radiology
sum(sbk.rad$Test.Name%in%price.list1$`Exam Name`)
sum(sbk.rad$Test.Code%in%price.list1$Exam)/100963
sum(sbk.rad$Test.Name%in%price.list1$`Exam Name`|sbk.rad$Test.Code%in%price.list1$Exam|
      sbk.rad$Test.Code%in%rad.notinlist$Test.Code[2:4]) / 100963


sum(sbk.rad$Test.Name%in%price.list1$`Exam Name`&!sbk.rad$Test.Code%in%price.list1$Exam)
sum((!sbk.rad$Test.Name%in%price.list1$`Exam Name`)&sbk.rad$Test.Code%in%price.list1$Exam)

check <- sbk.rad[!Test.Code%in%price.list1$Exam]
check.table <- table(check[,.(Test.Name, Test.Code)]) %>% data.table
check.table <- check.table[N!=0]
fwrite(check.table, "H:/GEMINI/Results/Studycost/rad.names.notinpricelist.csv")
check <- merge(check[,.(Test.Name, Test.Code)], price.list1[,c("Exam Name", "Exam")],
               by.x = "Test.Code", by.y = "Exam")
check <- sbk.rad[!Test.Code%in%price.list1$Exam]
# test name covers 64% of all the records and test codes covers 99%
# those who covered by test code but not test name, the name in data is slightly
# different from the name in price list

#if remove the "+" and "-" in codes, 99% can be covered
sbk.rad[,Test.Code := str_replace_all(Test.Code, "[+-]", "")]
check2 <- sbk.rad[!(Test.Name%in%price.list1$`Exam Name`|Test.Code%in%price.list1$Exam|
                    Test.Code%in%rad.notinlist$`Listed Test`)]


#check echo
sum(sbk.echo$TestName%in%price.list1$`Exam Name`)
table(sbk.echo$TestName)
# there are only 3 test names in echo data and none of them appeared in the pricelist

sum(sbk.lab$Test.Name%in%price.list2$`Test Name`)/6310633
# test name covers 80%
sum(sbk.lab$Test.ID%in%price.list2$Test)/6310633
# test code covers ~80%
sum(sbk.lab$Test.Name%in%price.list2$`Test Name`|sbk.lab$Test.ID%in%price.list2$Test|
      sbk.lab$Test.ID%in%lab.notinlist$Test.ID[lab.notinlist$`Listed Test`!=""]|)/6310633
 # only difference is that 1000 rows had test name only, test code NA

sum(sbk.lab$Test.Name%in%price.list2$`Test Name`)
sum(sbk.lab$Test.ID%in%price.list2$Test)
sum(sbk.lab$Test.ID%in%price.list2$Test&!sbk.lab$Test.Name%in%price.list2$`Test Name`)
sum(sbk.lab$Test.Name%in%price.list2$`Test Name`&!sbk.lab$Test.ID%in%price.list2$Test)
sbk.lab[sbk.lab$Test.ID%in%price.list2$Test&!sbk.lab$Test.Name%in%price.list2$`Test Name`]



check1 <- sbk.lab[Test.Name%in%price.list2$`Test Name`&!Test.ID%in%price.list2$Test]
check2 <- sbk.lab[(!Test.Name%in%price.list2$`Test Name`)&(Test.ID%in%price.list2$Test)]
check3 <- sbk.lab[!Test.Name%in%price.list2$`Test Name`]
check4 <- sbk.lab[(!Test.Name%in%price.list2$`Test Name`)&(!Test.ID%in%price.list2$Test)]
table(check4[,.(Test.Name, Test.ID)]) %>% data.table %>% '['(N!=0) %>%
  fwrite("H:/GEMINI/Results/Studycost/lab.names.notinpricelist.csv")




# ----------------------------- rank total cost --------------------------------
sbk.rad <- readg(sbk, rad.csv)
sbk.echo <- readg(sbk, echo)
sbk.lab <- rbind(readg(sbk, labs_ip),
                 readg(sbk, labs_er))
sbk.rad[,Test.Code := str_replace_all(Test.Code, "[+-]", "")]


freq.rad <- data.table(table(sbk.rad[,.(Test.Name, Test.Code)]))[N!=0]

#check uniqueness in pricelist
sum(duplicated(price.list1$Exam))
sum(duplicated(price.list1$`Exam Name`))
sum(duplicated(price.list1[,.(Exam, `Exam Name`)]))
names(price.list1)[2] <- "Department2"
check <- price.list1[duplicated(price.list1[,.(Exam, `Exam Name`)])|
                       duplicated(price.list1[,.(Exam, `Exam Name`)], fromLast = T)] %>%
  arrange(Exam, `Exam Name`)
fwrite(check, "H:/GEMINI/Results/Studycost/pricelist.rad.notunique.csv")


sum(duplicated(price.list2$Test))
sum(duplicated(price.list2$`Test Name`))
sum(duplicated(price.list2[,.(Test, `Test Name`)]))
names(price.list2)[2] <- "Department2"
check <- price.list2[duplicated(price.list2[,.(Test, `Test Name`)])|
                       duplicated(price.list2[,.(Test, `Test Name`)], fromLast = T)] %>%
  arrange(Test, `Test Name`)
fwrite(check, "H:/GEMINI/Results/Studycost/pricelist.lab.notunique.csv")


# freq.rad[Test.Code=="3VNBCM", listed.Code := "A1CAROTIDCM"]
# freq.rad[Test.Code=="3HEACM", listed.Code := "A1CAROTIDCM"]
# freq.rad[Test.Code=="GASTPI", listed.Code := "GASTJI"]
# freq.rad[Test.Code%in%price.list1$Exam, listed.Code := Test.Code]
# freq.rad <- merge(freq.rad, price.list1[,.(Exam, `Exam Name`, `Total Cost`)],
#                   by.x = "listed.Code", by.y = "Exam",
#                   all.x = T, all.y = F)
# names(freq.rad)[5:6] <- c("listed.Name", "Unit.Cost")
# freq.rad <- freq.rad[,c(2,3,4,1,5,6), with = F]
# freq.rad[, Total.Cost := as.numeric(Unit.Cost) * N]
# 
# fwrite(freq.rad[order(Total.Cost, decreasing = T)], "H:/GEMINI/Results/Studycost/rad.totalcost.csv")
# sum(duplicated(freq.rad$Test.Name))
# sum(duplicated(freq.rad$Test.Code))



freq.lab <- data.table(table(sbk.lab[,.(Test.Name, Test.ID)]))[N!=0]
freq.lab <- merge(freq.lab, lab.notinlist[!`Listed Test`%in%c("", "n/a"),
                                          .(Test.Name, Test.ID,
                                            `Listed Test`, `Listed Test Name`)],
                  by = c("Test.Name", "Test.ID"), all.x = T, all.y = F)
names(freq.lab)[4:5] <- c("listed.Test.ID", "listed.Test.Name")

freq.lab[Test.Name%in%price.list2$`Test Name`&!Test.ID%in%price.list2$Test] -> check

freq.lab[Test.Name%in%price.list2$`Test Name`, listed.Test.Name := Test.Name]
freq.lab[Test.ID=="SIL2R", listed.Test.Name:= price.list2$`Test Name`[price.list2$Test=="SIL2R"]]
freq.lab[is.na(listed.Test.ID)&!is.na(listed.Test.Name)]
freq.lab[is.na(listed.Test.Name)&!is.na(listed.Test.ID)]

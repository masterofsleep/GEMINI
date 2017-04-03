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
names(price.list1)[2] <- names(price.list2)[2] <- "Department2"

price.list1 <- price.list1[!is.na(Department)&!str_detect(Department2, "-OAI")]
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
lab.notinlist <- fread("H:/GEMINI/Results/Studycost/lab names notinpricelist_ASW_AV.csv")
rad.notinlist <- fread("H:/GEMINI/Results/Studycost/rad names notinpricelist_ASW.csv")

price.list1 <- readxl::read_excel("H:/GEMINI/Results/Studycost/PriceList Labs DI Pharmacy FY1516.xlsx",
                                  sheet = 1, skip = 1)
price.list2 <- readxl::read_excel("H:/GEMINI/Results/Studycost/PriceList Labs DI Pharmacy FY1516.xlsx",
                                  sheet = 2, skip = 1)

names(price.list1)[6:11] <- price.list1[1, 6:11]
price.list1 <- price.list1[2:nrow(price.list1), ] %>% data.table
names(price.list2)[6:11] <- price.list2[1, 6:11]                                 
price.list2 <- price.list2[2:nrow(price.list2), ] %>% data.table
names(price.list1)[2] <- names(price.list2)[2] <- "Department2"

price.list1 <- price.list1[!is.na(Department)&!str_detect(Department2, "-OAI")]
price.list2 <- price.list2[!is.na(Department)]



## radiology 

unique.exam <- price.list1[!Exam%in%price.list1$Exam[duplicated(price.list1$Exam)]]
unique.name <- price.list1[!`Exam Name`%in%price.list1$`Exam Name`[duplicated(price.list1$`Exam Name`)]]
unique.both <- price.list1[!duplicated(price.list1[,.(`Exam Name`, Exam)])&
                             !duplicated(price.list1[,.(`Exam Name`, Exam)], fromLast = T)]

freq.rad <- data.table(table(sbk.rad[,.(Test.Name, Test.Code)]))[N!=0]
freq.rad[Test.Code%in%c("3VNBCM", "3HEACM"), ':='(listed.code = "A1CAROTIDCM",
                                   listed.name = "A1C MRI 3D Vascular Neck with Brain with Contrast",
                                   unit.cost = price.list1[Exam=="A1CAROTIDCM", `Total Cost`])]
freq.rad[Test.Code=="GASTPI", ':='(listed.code = "GASTJI",
                                   listed.name = "Gastrojejunostomy insertion",
                                   unit.cost = price.list1[Exam=="GASTJI", `Total Cost`])]

# merge by both 
freq.rad <- merge(freq.rad, unique.both[,.(Exam, `Exam Name`, `Total Cost`)],
                  by.x = c("Test.Code", "Test.Name"), 
                  by.y = c("Exam", "Exam Name"), all.x = T, all.y = F)

# merge by test code
freq.rad <- merge(freq.rad, unique.exam[,.(Exam, `Exam Name`, `Total Cost`)],
                  by.x = "Test.Code", by.y = "Exam", all.x = T, all.y = F)
# merge by test name
freq.rad <- merge(freq.rad, unique.name[,.(Exam, `Exam Name`, `Total Cost`)],
                  by.x = "Test.Name", by.y = "Exam Name", all.x = T, all.y = F)
sum(freq.rad$`Total Cost.x`!=freq.rad$`Total Cost.y`, na.rm = T)
sum(freq.rad$`Total Cost.x`!=freq.rad$`Total Cost`, na.rm = T)
sum(freq.rad$`Total Cost.y`!=freq.rad$`Total Cost`, na.rm = T)
freq.rad[`Total Cost.y`!=`Total Cost`, ':='(`Exam Name`= NA,
                                            `Total Cost.y` = NA,
                                            Exam = NA,
                                            `Total Cost` = NA)]
freq.rad[!is.na(`Total Cost.x`), ':='(listed.code = "-",
                                      listed.name = "-",
                                      unit.cost = `Total Cost.x`)]
freq.rad[!is.na(`Total Cost.y`)&is.na(unit.cost), ':='(listed.code = "-",
                                      listed.name = `Exam Name`,
                                      unit.cost = `Total Cost.y`)]
freq.rad[!is.na(`Total Cost`)&is.na(unit.cost), ':='(listed.code = Exam,
                                      listed.name = "-",
                                      unit.cost = `Total Cost`)]
freq.rad <- freq.rad[order(N, decreasing = T)]

freq.rad <- freq.rad[,.(Test.Name, Test.Code, N, listed.name, listed.code, unit.cost, total.cost = as.integer(N)*as.numeric(unit.cost))]
fwrite(freq.rad, "H:/GEMINI/Results/Studycost/rad.cost.csv")




## ------------------------------- lab -----------------------------------------

sum(duplicated(price.list2$Test))
sum(duplicated(price.list2$`Test Name`))
sum(duplicated(price.list2[,.(Test, `Test Name`)]))

price.list2[duplicated(price.list2[,.(Test, `Test Name`)])|duplicated(price.list2[,.(Test, `Test Name`)], fromLast = T)] -> check

# check <- price.list2[duplicated(price.list2[,.(Test, `Test Name`)])|
#                        duplicated(price.list2[,.(Test, `Test Name`)], fromLast = T)] %>%
#   arrange(Test, `Test Name`)
# fwrite(check, "H:/GEMINI/Results/Studycost/pricelist.lab.dup.id.and.name.csv")
# price.list2[duplicated(Test)|duplicated(Test, fromLast = T)] %>% arrange(Test) %>%
#   fwrite("H:/GEMINI/Results/Studycost/pricelist.lab.dup.id.csv")
# price.list2[duplicated(`Test Name`)|duplicated(`Test Name`, fromLast = T)] %>% arrange(`Test Name`) %>%
#   fwrite("H:/GEMINI/Results/Studycost/pricelist.lab.dup.name.csv")
# 
# sum(!paste(sbk.lab$Test.Name, sbk.lab$Test.ID)%in%paste(price.list2$`Test Name`, price.list2$Test))/6310633
# 


# fwrite(freq.rad[order(Total.Cost, decreasing = T)], "H:/GEMINI/Results/Studycost/rad.totalcost.csv")
# sum(duplicated(freq.rad$Test.Name))
# sum(duplicated(freq.rad$Test.Code))



freq.lab <- data.table(table(sbk.lab[,.(Test.Name, Test.ID)]))[N!=0]
freq.lab <- merge(freq.lab, lab.notinlist[,
                                          .(Test.Name, Test.ID,
                                            `Listed Test`, `Listed Test Name`)],
                  by = c("Test.Name", "Test.ID"), all.x = T, all.y = F)
names(freq.lab)[4:5] <- c("listed.id", "listed.name")

unique.name <- price.list2[!`Test Name`%in%price.list2$`Test Name`[duplicated(price.list2$`Test Name`)]]
unique.id <- price.list2[!Test%in%price.list2$Test[duplicated(price.list2$Test)]]
unique.both <- price.list2[!duplicated(price.list2[,.(Test, `Test Name`)])&
                             !duplicated(price.list2[,.(Test, `Test Name`)], fromLast = T)]
freq.lab <- merge(freq.lab, unique.both[,.(Test, `Test Name`, `Total Cost`)],
                  by.x = c("listed.name", "listed.id"),
                  by.y = c("Test Name", "Test"),
                  all.x = T, all.y = F)
freq.lab <- merge(freq.lab, unique.both[,.(Test, `Test Name`, `Total Cost`)],
                  by.x = c("Test.Name", "Test.ID"),
                  by.y = c("Test Name", "Test"),
                  all.x = T, all.y = F)

#freq.lab <- merge(freq.lab, unique.id[,.(Test, `Test Name`, `Total Cost`)],
#                  by.x = "Test.ID", by.y = "Test",
#                  all.x = T, all.y = F)
# only matched two more
sum(freq.lab$`Total Cost.x`!=freq.lab$`Total Cost.y`, na.rm = T)
sum(freq.lab$`Total Cost.x`!=freq.lab$`Total Cost`, na.rm = T)
sum(freq.lab$`Total Cost.y`!=freq.lab$`Total Cost`, na.rm = T)
freq.lab[!is.na(`Total Cost.y`), ':='(listed.name = "-",
                                      listed.id = "-",
                                      unit.cost = `Total Cost.y`)]
freq.lab[!is.na(`Total Cost.x`), ':='(unit.cost = `Total Cost.x`)]
freq.lab <- freq.lab[,.(Test.Name, Test.ID, N, listed.name, listed.id, unit.cost, total.cost = as.integer(N)*as.numeric(unit.cost))]
freq.lab <- freq.lab[order(N, decreasing = T)]

fwrite(freq.lab, "H:/GEMINI/Results/Studycost/lab.cost.csv")

#does not work, there are duplciated test names here
#lab.notinlist.new <- merge(lab.notinlist.new, unique.name[,.(`Test Name`, Test)],
#                           by.x = "Test.Name",by.y = "Test Name", all.x = T)
lab.notinlist.new <- lab.notinlist.new[(is.na(`Listed Test Name`)|`Listed Test Name`=="")&
                                         is.na(`Test Name`)]

fwrite(lab.notinlist.new[order(N, decreasing = T)], 
       "H:/GEMINI/Results/Studycost/lab.cannot.be.identified.in.pricelist.csv")

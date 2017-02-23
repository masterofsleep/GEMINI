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
price.list1 <- price.list1[2:nrow(price.list1), ]
names(price.list2)[6:11] <- price.list2[1, 6:11]                                 
price.list2 <- price.list2[2:nrow(price.list2), ]


#check radiology
sum(sbk.rad$Test.Name%in%price.list1$`Exam Name`)
sum(sbk.rad$Test.Code%in%price.list1$Exam)
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

chekc2 <- sbk.rad[(!Test.Code%in%price.list1$Exam)&(Test.Name%in%price.list1$`Exam Name`)]
sbk.rad[,Test.Code := str_replace_all(Test.Code, "[+-]", "")]
#if remove the "+" and "-" in codes, 99% can be covered

#check echo
sum(sbk.echo$TestName%in%price.list1$`Exam Name`)
table(sbk.echo$TestName)
# there are only 3 test names in echo data and none of them appeared in the pricelist

sum(sbk.lab$Test.Name%in%price.list2$`Test Name`)
# test name covers 80%
sum(sbk.lab$Test.ID%in%price.list2$Test)
# test code covers ~80%
sum(sbk.lab$Test.Name%in%price.list2$`Test Name`|sbk.lab$Test.ID%in%price.list2$Test)
# only difference is that 1000 rows had test name only, test code NA


check1 <- sbk.lab[Test.Name%in%price.list2$`Test Name`&!Test.ID%in%price.list2$Test]
check2 <- sbk.lab[(!Test.Name%in%price.list2$`Test Name`)&(Test.ID%in%price.list2$Test)]
check3 <- sbk.lab[!Test.Name%in%price.list2$`Test Name`]
check4 <- sbk.lab[(!Test.Name%in%price.list2$`Test Name`)&(!Test.ID%in%price.list2$Test)]
table(check4[,.(Test.Name, Test.ID)]) %>% data.table %>% '['(N!=0) %>%
  fwrite("H:/GEMINI/Results/Studycost/lab.names.notinpricelist.csv")

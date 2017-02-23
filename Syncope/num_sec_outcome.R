#check syncope secondary outcome numbers

inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")

ctpa.smh <- fread("H:/GEMINI/Results/Syncope/ctpa.smh.csv")
ctpa.sbk <- fread("H:/GEMINI/Results/Syncope/ctpa.sbk.csv")
ctpa.uhn <- fread("H:/GEMINI/Results/Syncope/ctpa.uhn.csv")
ctpa.encid <- c(ctpa.smh$EncID.new, ctpa.sbk$EncID.new, ctpa.uhn$EncID.new)

sum(inc.ex1b$EncID.new%in%ctpa.encid)



vq.smh <- fread("H:/GEMINI/Results/Syncope/vq.smh.csv")
vq.sbk <- fread("H:/GEMINI/Results/Syncope/vq.sbk.csv")
vq.uhn <- fread("H:/GEMINI/Results/Syncope/vq.uhn.csv")
vq.encid <- c(vq.smh$EncID.new, vq.sbk$EncID.new, vq.uhn$EncID.new)

sum(inc.ex1b$EncID.new%in%vq.encid)


du.smh <- fread("H:/GEMINI/Results/Syncope/du.smh.csv")
du.sbk <- fread("H:/GEMINI/Results/Syncope/du.sbk.csv")
du.uhn <- fread("H:/GEMINI/Results/Syncope/du.uhn.csv")
du.encid <- c(du.smh$EncID.new, du.sbk$EncID.new, du.uhn$EncID.new)

sum(inc.ex1b$EncID.new%in%du.encid)

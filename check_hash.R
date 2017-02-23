#check the hash

smh <- readg(smh, adm)
sbk <- readg(sbk, adm)
uhn <- readg(uhn, adm)
thp <- readg(thp, adm)


sum(smh$Hash=="c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692")
sum(sbk$Hash=="c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692")
sum(uhn$Hash=="c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692")
sum(thp$Hash=="c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692")


sum(smh$Hash=="52845e114c8dd03943c715f65e863262b154407df893a955756aa7ecefc38f34")
sum(sbk$Hash=="52845e114c8dd03943c715f65e863262b154407df893a955756aa7ecefc38f34")
sum(uhn$Hash=="52845e114c8dd03943c715f65e863262b154407df893a955756aa7ecefc38f34")
sum(thp$Hash=="52845e114c8dd03943c715f65e863262b154407df893a955756aa7ecefc38f34")


swdr("SMH/CIHI/Archive")
link <- fread("smh.GIM_IP_LINKING_LIST.csv")

hash1 <- link[HealthCardNumber=="1"]

smh[str_sub(EncID.new, 3,8)%in%hash1]

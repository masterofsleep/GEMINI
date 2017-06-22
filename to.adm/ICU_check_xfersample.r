# -------------------------- check ICU -----------------------------------------

cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")

smh.xf <- readg(smh, xfer)
sbk.xf <- readg(sbk, xfer)
uhn.xf <- readg(uhn, xfer)
msh.xf <- readg(msh, xfer)

sample.enc <- function(){
  sample_site_enc <- function(siteid){
    set.seed(100)
    sample(cohort[startsWith(EncID.new, siteid)&SCU.adm==T, EncID.new], 10)
  }
  return(c(sample_site_enc("11"),
           sample_site_enc("12"),
           sample_site_enc("13"),
           sample_site_enc("14")))
}

sample_enc <- sample.enc()

smh.xf[EncID.new%in%sample_enc] %>% 
  arrange(EncID.new, ymd_hm(paste(Date.Check.in, Time.Check.in))) %>%
  fwrite("C:/Users/guoyi/Desktop/to.adm/check_icu/smh_icu_xfer.csv")

sbk.xf[EncID.new%in%sample_enc] %>% 
  arrange(EncID.new, ymd_hm(paste(Date.Check.in, Time.Check.in))) %>%
  fwrite("C:/Users/guoyi/Desktop/to.adm/check_icu/sbk_icu_xfer.csv")

uhn.xf[EncID.new%in%sample_enc] %>% 
  arrange(EncID.new, ymd_hm(paste(Date.Check.in, Time.Check.in))) %>%
  fwrite("C:/Users/guoyi/Desktop/to.adm/check_icu/uhn_icu_xfer.csv")

msh.xf[EncID.new%in%sample_enc] %>% 
  arrange(EncID.new, ymd_hm(paste(TRANSACTION_DT, TRANSACTION_TM))) %>%
  fwrite("C:/Users/guoyi/Desktop/to.adm/check_icu/msh_icu_xfer.csv")

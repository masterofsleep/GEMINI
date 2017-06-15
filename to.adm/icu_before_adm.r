# -------------------------- new ICU -------------------------------------------
# SCU admission
scu_before_adm <- function(){
  smh.xf <- readg(smh, ip_xfer, dt = T)[Unit.Code =="1"]
  sbk.xf <- readg(sbk, ip_xfer, dt = T)[Unit.Code %in%c("1")]
  uhn.xf <- readg(uhn, ip_xfer, dt = T)[Unit.Code %in%c("1")]
  msh.xf <- readg(msh, xfer, dt = T)[NURSE_UNIT_DISP=="ICU"]
  smh.scu <- readg(smh, ip_scu, dt = T)
  sbk.scu <- readg(sbk, ip_scu, dt = T)
  uhn.scu <- readg(uhn, ip_scu, dt = T)
  msh.scu <- readg(msh, ip_scu, dt = T)
  thp.scu <- readg(thp, ip_scu, dt = T)
  
  sbk.scu <- sbk.scu[SCU.Unit.Number!="99"]
  thp.scu <- thp.scu[SCU.Unit.Number!="99"]
  
  all.scu <- rbind(smh.scu)
  scu.admit <- unique(c(smh.xf$EncID.new, smh.scu$EncID.new,
                        sbk.xf$EncID.new, sbk.scu$EncID.new,
                        uhn.xf$EncID.new, uhn.scu$EncID.new,
                        msh.xf$EncID.new, msh.scu$EncID.new, 
                        thp.scu$EncID.new))
  
  icu.before.adm <- c(smh.scu[mdy_hm(paste(SCU.Admit.Date, SCU.Admit.Time))<=
                                ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      sbk.scu[mdy_h(paste(SCU.Admit.Date, SCU.Admit.Time))<=
                                ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      uhn.scu[ymd_hm(paste(SCU.Admit.Date, SCU.Admit.Time))<=
                                ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      msh.scu[ymd_hm(paste(SCU.Admit.Date, SCU.Admit.Time))<=
                                ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      smh.xf[ymd_hm(paste(Date.Check.in, Time.Check.in))<=
                               ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      sbk.xf[ymd_hm(paste(Date.Check.in, Time.Check.in))<=
                               ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      uhn.xf[ymd_hm(paste(Date.Check.in, Time.Check.in))<=
                               ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new],
                      msh.xf[ymd_hm(paste(TRANSACTION_DT, TRANSACTION_TM))<=
                               ymd_hm(paste(Admit.Date, Admit.Time)), EncID.new]
                      )
  return(icu.before.adm)
}

fwrite(data.frame(EncID.new = icu.before.adm.enc), "C:/Users/guoyi/Desktop/to.adm/icu.before.adm.enc.csv")

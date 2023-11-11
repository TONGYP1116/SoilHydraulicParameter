install.packages("soilhypfit")
library(soilhypfit)
# set work path
setwd("your work path")

# data preparing
data_original<-read.csv(file = "your files saved original data",sep = ",")


#-------------------------------------------------fit van Genuchten model arguements-------------------------------------------#
system.time(fit1 <- fit_wrc_hcc(wrc_formula = theta_t ~ h_t|id,
                                data = data_original,
                                control = control_fit_wrc_hcc(
                                  min_nobs_wc = 6,
                                  keep_empty_fits = TRUE,
                                  nloptr = control_nloptr(maxeval = 250),
                                  param_bound = param_boundf(
                                    alpha = c(1.490116e-07 , 100.),
                                    n = c(1., 7.)
                                  )
                                )
)
)
para_fit1 <- coef(fit1,gof = TRUE)
write.csv(para_fit1, file="vgfit_para.csv")


#----------------------------------------------predict FC & PWP & calculate wrc fit r2------------------------------------------#
predict_all<- data.frame()
for(i in c(1:1042)){
  h_fc <- 3.3651
  h_pwp <- 152.9574
  predict_th_fc_i <- wc_model(h_fc, nlp=c(alpha=fit1[["fit"]][[i]][["nlp"]][["alpha"]],n=fit1[["fit"]][[i]][["nlp"]][["n"]]), lp=c(thetar=fit1[["fit"]][[i]][["lp"]][["thetar"]],thetas=fit1[["fit"]][[i]][["lp"]][["thetas"]]),precBits = NULL, wrc_model = "vg")
  predict_th_pwp_i <- wc_model(h_pwp, nlp=c(alpha=fit1[["fit"]][[i]][["nlp"]][["alpha"]],n=fit1[["fit"]][[i]][["nlp"]][["n"]]), lp=c(thetar=fit1[["fit"]][[i]][["lp"]][["thetar"]],thetas=fit1[["fit"]][[i]][["lp"]][["thetas"]]),precBits = NULL, wrc_model = "vg")
  SST_i <- length(data1$theta_t)*var(fit1[["fit"]][[i]][["model"]][["wrc"]][["theta_t"]])
  fit_r2_i <- 1-attr(fit1[["fit"]][[i]][["objective"]],"ssq_wc")/SST_i
  th_pre_combine_i <- cbind(predict_th_fc_i,predict_th_pwp_i,fit_r2_i)
  predict.now <- get("th_pre_combine_i")
  row.names(predict.now) <- i
  predict_all <- rbind(predict_all, predict.now)
}
write.csv(predict_all, file="PredictFC&PWP&r2fit.csv")

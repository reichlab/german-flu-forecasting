## run all testing forecasts
library(foreach)
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

model_names <- c(
    #"DL4EPI",
    "EmpiricalBayes",
    "hetGP",
    "sarimaTD",
    # "SIRS_EAKF",
    "seasonalGAM"
)

# run each model one at a time
foreach(model = model_names) %dopar% {
    rcmd <- paste0("Rscript code/evaluation-code.R > evaluation-code-", model, ".Rout \"", model, "\"")
    system(rcmd)
    }
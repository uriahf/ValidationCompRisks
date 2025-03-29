rdata <- readRDS(here::here("Data/rdata.rds"))
vdata <- readRDS(here::here("Data/vdata.rds"))

rdata$hr_status <- relevel(rdata$hr_status, ref = "ER and/or PR +")
vdata$hr_status <- relevel(vdata$hr_status, ref = "ER and/or PR +")

ca_rdata <- stdca(
  data = rdata,
  outcome = "status_num",
  ttoutcome = "time",
  timepoint = horizon,
  predictors = "pred5",
  xstop = 0.35,
  ymin = -0.01,
  graph = FALSE,
  cmprsk = TRUE
)


library(rtichoke)
rtichoke:::mids

create_mids_and_counts_data_from_probs_times(
  probs = pred_1_5, 
  stratified_by = "ppcr", 
  by = 0.1
)

create_mids_and_counts_data_from_probs_times(
  probs = pred_1_5, 
  # reals = as.vector(df_surv$cancer),
  # times = df_surv$ttcancer,
  stratified_by = "probability_threshold", 
  by = 0.1
)

create_mids_and_counts_data_from_probs_times_cif <- function(
    probs, reals, times, stratified_by, 
    fixed_horizon_times, weights, by) {
  
  breaks <- round(
    seq(0, 1, by = by),
    digits = nchar(format(by, scientific = FALSE)))
  
  
  print("breaks")
  print(breaks)
  
  
  for (i in breaks[2:length(breaks)]) {
    
    
    cuninc_obj <- cmprsk::cuminc(
        reals[probs < i & probs >= i - by],
        times[probs < i & probs >= i -by])
    
    print("probs in")
    print(i)
    print(i-by)
    print(
      cmprsk::timepoints(
        cuninc_obj, 
        times = c(0, 1, 3, 5))$est
    )

    
  }
  
  
  
  tidycmprsk::cuminc(
    survival::Surv(ttdeath, death_cr) ~ trt, 
    tidycmprsk::trial) |> 
    tidycmprsk::tidy(times =  c(13, 15, 17))
  
  
  # hist_data <- hist(
  #   probs,
  #   plot = FALSE, 
  #   breaks = breaks
  # )
  # 
  # if (stratified_by == "probability_threshold") {
  #   
  #   hist_data %>%
  #     .[c("mids", "counts")] |>
  #     tibble::as_tibble() |>
  #     tibble::add_row(
  #       mids = 0, counts = as.integer(0), .before = 0)
  #   
  # }
  
}



create_mids_and_counts_data_from_probs_times_cif(
  probs = pred_1_5, 
  reals = as.vector(df_surv$cancer),
  times = df_surv$ttcancer,
  stratified_by = "probability_threshold", 
  by = 0.1
)

library(magrittr)




breaks <- round(
  seq(0, 1, by = 0.2),
  digits = nchar(format(by, scientific = FALSE)))

data_for_cmprsk <- tibble::tibble(
  probs = pred_1_5,
  reals = as.factor(as.vector(df_surv$cancer_cr)),
  times = df_surv$ttcancer
) |> 
  mutate(
    strata = cut(
      probs, breaks = breaks)
    )

df_surv$cancer_cr






cmprsk::cuminc(
  ftime = data_for_cmprsk$times,
  fstatus = data_for_cmprsk$reals,
  group = data_for_cmprsk$strata
) |> 
  cmprsk::timepoints(
    times = c(1, 3, 5, 10)
  ) %$%
  est |> 
  as_tibble(rownames = "strata")


library(survival)

survival::df_sur

library(dcurves)


breaks <- round(
  seq(0, 1, by = 0.1),
  digits = nchar(format(by, scientific = FALSE)))

# data_for_cmprsk <- tibble::tibble(
#   probs = pred_1_5,
#   reals = as.factor(as.vector(df_surv$cancer_cr)),
#   times = df_surv$ttcancer
# ) |> 
#   mutate(
#     strata = cut(
#       probs, breaks = breaks)
#   )

View(df_surv)

df_surv$ttcancer

?df_surv
  
  
  names()
  dplyr::pull(real)


dput(unique(as.vector(df_surv$cancer_cr)))
data_for_cmprsk <- tibble::tibble(
  probs = pred_1_5,
  reals = as.factor(as.vector(df_surv$cancer_cr)),
  times = df_surv$ttcancer
) |> 
  mutate(
    strata = cut(
      probs, breaks = breaks)
  )



?cut

library(dplyr)

by <- 0.01

breaks <- round(
  seq(0, 1, by = by),
  digits = nchar(format(by, scientific = FALSE)))

tidycmprsk::cuminc(
  survival::Surv(times, reals) ~ strata, 
  data_for_cmprsk) |> 
  tidycmprsk::tidy(times =  c(5)) |> 
  dplyr::filter(
    outcome == "diagnosed with cancer") |> 
  select(
    time, outcome, strata, estimate,
    n.censor, n.event, n.censor, cum.event) |> 
  View()

 gcmprsk::cuminc(
  ftime = data_for_cmprsk$times,
  fstatus = data_for_cmprsk$reals,f
  group = data_for_cmprsk$strata
) |> 
  cmprsk::timepoints(
    times = 5
  ) %$%
  est |> 
  as_tibble(rownames = "strata") |> 
  View()







cmprsk::cuminc

data_for_cmprsk |> 
  group_by(strata) |>
  summarise(
    sum(reals == TRUE)
  )



range(data_for_cmprsk$times)

cmprsk::timepoints(
  ftime = data_for_cmprsk$times,
  fstatus = data_for_cmprsk$reals,
  group = data_for_cmprsk$strata
)

?cmprsk::timepoints

cmprsk::timepoints(
  Surv(times, reals) ~ strata,
  data = data_for_cmprsk,
  times = c(1, 2, 3)
)






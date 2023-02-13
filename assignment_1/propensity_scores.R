library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(fastDummies)
library(Hmisc)
library(tableone)
library(survey)
library(MatchIt)
library(cobalt)

EXPOSURE = "qsmk"
OUTCOME = "wt82_71"

read_data = function(fp, analytic_cols, factor_cols){
  df = read_excel(fp)
  df[factor_cols] = apply(df[factor_cols], 2, factor)
  df = rename(df, studyid = seqn)
  
  df = df[c("studyid", EXPOSURE, OUTCOME, analytic_cols)]
  
  # Missing data
  replace_na_0 = list("pregnancies"=0, "alcoholhowmuch"=0)
  df[names(replace_na_0)] = replace_na(df[names(replace_na_0)], replace_na_0)
  
  return(df)
}

add_propensity_scores = function(df, m=NULL){
  if (is.null(m)){
    m = glm(qsmk ~ ., data=df[c(ANALYTIC_VARS, "qsmk")], family=binomial())
  }
  df["ps"] = predict(m, type="response")
  p_qsmk = mean(df$qsmk)
  df = mutate(df, w = if_else(qsmk == 1, p_qsmk / (ps), (1-p_qsmk)/(1-ps)))
  return(df)
  
}



calc_means = function(df, analytic_vars, factor_cols=FACTOR_COLS){
  df = df[c("qsmk", analytic_vars, "w")]
  df = dummy_cols(df, select_columns = factor_cols, remove_selected_columns = TRUE)
  tx = filter(df, qsmk == 1) %>% select(-qsmk)
  ctrl = filter(df, qsmk == 0) %>% select(-qsmk)
  
  
  unweighted = calc_means_helper(select(tx, -w), factor_cols = factor_cols ) %>%
    cbind(
      calc_means_helper(select(ctrl, -w), factor_cols = factor_cols)
    )
  colnames(unweighted) = c("mean_1", "sd_1", "mean_0", "sd_0")
  unweighted = unweighted[-1,]
  var_names = rownames(unweighted)
  
  unweighted = as.data.frame(unweighted)
  unweighted$var= var_names
  unweighted$weight = "none"
  unweighted = unweighted[c("var", "weight", "mean_1", "sd_1", "mean_0", "sd_0")]
  
  weighted = calc_means_helper(select(tx, -w), weights=tx$w, factor_cols = factor_cols ) %>%
    cbind(
      calc_means_helper(select(ctrl, -w), weights=ctrl$w, factor_cols = factor_cols )
    )
  
  colnames(weighted) = c("mean_1", "sd_1", "mean_0", "sd_0")
  weighted = weighted[-1,]
  
  weighted = as.data.frame(weighted)
  weighted$weight = "weighted"
  weighted$var = var_names
  
  weighted = weighted[c("var", "weight", "mean_1", "sd_1", "mean_0", "sd_0")]
  
  return(bind_rows(unweighted, weighted))
}

calc_means_helper = function(df, weights=1, factor_cols=FACTOR_COLS){
  print(dim(df))
  
  if (length(weights) == 1){weights = rep(1, nrow(df))}
  means = apply(df, 2, function(x){ (x %*% weights)/sum(weights)})
  sds = sqrt(apply(df, 2, function(x){wtd.var(x, weights)}))
  final = cbind(means, sds)
  
  return(final)
}

calc_sd_helper = function(df, weights=1, factor_cols=FACTOR_COLS){
  if (!is.null(factor_cols)){df = dummy_cols(df, select_columns = factor_cols, remove_selected_columns = TRUE)}
  if (length(weights) == 1){weights = rep(1, nrow(df))}
  
  return(sds)
}
add_sd_diffs = function(means){
  mutate(means, sd_diff=(mean_1 - mean_0) / sqrt(sd_1^2 + sd_0^2))
}

plot_standard_diffs = function(sd_diffs){
  vars_ordered = sd_diffs %>%
    filter(weight == "none") %>%
    arrange((abs(sd_diff))) %>%
    pull(var)
  
  sd_diffs %>%
    mutate(var=factor(var, levels=vars_ordered)) %>%
    ungroup() %>%
    ggplot(aes(y=var, x=abs(sd_diff), color=weight, shape=weight)) +
    geom_point() +
    geom_vline(xintercept=0) +
    geom_vline(xintercept=0.1)
}

create_matched_object = function(df, method="nearest", distance="glm"){
  m.out <- matchit(qsmk ~ . -wt82_71 , keep.x=TRUE, 
                   data = dummy_cols(df[c("qsmk", "wt82_71", ANALYTIC_VARS)], 
                                     select_columns = FACTOR_COLS, remove_selected_columns = TRUE),
                   method = method, distance = distance)
  return(m.out)
}

create_matched_dataset = function(m.out){
  d = match.data(m.out)
  return(match.data(m.out))
}

plot_balance = function(m.out){
  bt = bal.tab(m.out, un=TRUE,stats = c("m", "v", "ks"))
  
  bt2 = bt$Balance %>%
    mutate(unweighted = abs(Diff.Un), weighted=abs(Diff.Adj), var=rownames(bt)) 
  bt2$var = rownames(bt2)
  
  plot_standardized_mean_diffs(bt2[c(-1, -2),])+ theme_minimal() 
}
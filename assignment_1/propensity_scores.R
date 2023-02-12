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

mutate(means, (mean_1 - mean_0) / sqrt(sd_1^2 + sd_2^2))

  
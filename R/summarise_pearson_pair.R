#' Run and summarize Pearson correlation analysis
#'
#' @description
#' Runs correlation cor.test and provides long format output with correlation
#' coefficient, pval, and sample size.
#' Rerun function for various correlation pairs and use bind_rows to combine dataframes
#' @importFrom  stats cor.test
#' @import dplyr
#' @param df_in -input dataframe with IDs as rownames, 1 grouping column, and at least two columns of data to correlate
#' @param grouping_var_in -quoted name of grouping variable
#' @param var1_A quoted names of variables for pairwise comparison
#' @param var1_B quoted names of variables for pairwise comparison
#' @return dfvar_final dataframe containing the following columns; corr_var1-	strings containing the names of the variables compared;  corr_var2- strings containing the names of the variables compared; grouping_var_name- name of grouping variable; grouping_var- value of grouping variable for pairwise comparison; pearson_val- correlation coefficient value aka estimate;  p_val-  p-value;  data_n- number of data points included in comparison
#'
#' @export
summarise_pearson <- function(df_in, grouping_var_in, var1_A, var1_B){
  df_out1 <- dplyr::filter(df_in, !is.na(get(var1_A)) == T & !is.na(get(var1_B)) ==T ) %>%
    dplyr::group_by(get(grouping_var_in)) %>%
    dplyr::filter(n() >= 3) %>%
    dplyr::summarise(pearson_val = stats::cor.test(get(var1_A), get(var1_B), method="pearson", use = "pairwise.complete.obs")$estimate,
                     p_val = stats::cor.test(get(var1_A), get(var1_B),  method="pearson", use = "pairwise.complete.obs")$p.value,
                     data_n=n())%>%
    dplyr::mutate(corr_var1 = paste0(var1_A),
                  corr_var2 = paste(var1_B),
                  grouping_var_name = paste0(grouping_var_in))

  dfvar_final <- df_out1 %>%
    dplyr::select(corr_var1,	corr_var2,
                  # grouping_var_name,
                  grouping_var="get(grouping_var_in)",
                  pearson_val,	p_val, data_n)
  return(dfvar_final)
}

# Clean up the unquote (bare) variable names used in dplyr calls
corr_var1 <- corr_var2 <- pearson_val <- p_val <- data_n <- NULL

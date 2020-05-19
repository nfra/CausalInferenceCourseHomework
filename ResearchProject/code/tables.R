
# load covar balance outputs
fileConn = file('./data/results/employment_status_match_balance.txt')
cb_empstat = readLines(fileConn)
close(fileConn)

fileConn = file('./data/results/labor_force_match_balance.txt')
cb_labfor = readLines(fileConn)
close(fileConn)

fileConn = file('./data/results/looking_match_balance.txt')
cb_look = readLines(fileConn)
close(fileConn)
  
fileConn = file('./data/results/worked_last_week_match_balance.txt')
cb_wrkdwk = readLines(fileConn)
close(fileConn)
 
fileConn = file('./data/results/worked_last_year_match_balance.txt')
cb_wrkdyr = readLines(fileConn)
close(fileConn)

# define vectors for retrieving pertinent data
covar_name_vec = c(seq(from=2, to=542, by=18), 563, seq(584, 998, 18))
std_mean_diff_vec = covar_name_vec + 4

covar_name_wlw_vec = c(seq(from=2, to=542, by=18), 563, seq(584, 998, 18), 1019, seq(1040, 1180, 18))
std_mean_diff_wlw_vec = covar_name_wlw_vec + 4

# retrieve pertinent data
cb_empstat_covar_names = cb_empstat[covar_name_vec]
cb_empstat_std_mean_diffs = cb_empstat[std_mean_diff_vec]

cb_labfor_covar_names = cb_labfor[covar_name_vec]
cb_labfor_std_mean_diffs = cb_labfor[std_mean_diff_vec]

cb_look_covar_names = cb_look[covar_name_vec]
cb_look_std_mean_diffs = cb_look[std_mean_diff_vec]

cb_wrkdwk_covar_names = cb_wrkdwk[covar_name_wlw_vec]
cb_wrkdwk_std_mean_diffs = cb_wrkdwk[std_mean_diff_wlw_vec]

cb_wrkdyr_covar_names = cb_wrkdyr[covar_name_vec]
cb_wrkdyr_std_mean_diffs = cb_wrkdyr[std_mean_diff_vec]



# fix results df
results_df = read.csv('./data/results/match_results_table.csv', colClasses = rep("character", 6))[,2:7]

results_df[1,6] = -0.0082411
results_df[2,6] = 0.00090988
results_df[3,6] = -9.0573

colnames(results_df) = c(' ', "Employment Status", "Lab. Force Part.",  "Looked for Work", "Worked Last Week", "Worked Last Year")

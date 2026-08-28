# 06_group_comparison.R - final comparison of acceptable model families.

source("scripts/00_setup.R")
required_files <- paste0("output/member_", LETTERS[1:4], "_accuracy.csv")
if (!all(file.exists(required_files))) stop("Run scripts 02-05 before running the group comparison.")

all_model_results <- bind_rows(lapply(required_files, read_csv, show_col_types = FALSE)) %>% arrange(RMSE)
write_csv(all_model_results, "output/model_diagnostics_summary.csv")

comparison <- all_model_results %>%
  filter(Residuals_acceptable == "Yes") %>%
  arrange(RMSE)
if (nrow(comparison) == 0) {
  stop("No model passed both residual checks. Review output/model_diagnostics_summary.csv.")
}
write_csv(comparison, "output/model_comparison_summary.csv")
print(comparison)

plot <- ggplot(comparison, aes(reorder(model, RMSE), RMSE, fill = member)) + geom_col(show.legend = FALSE) + coord_flip() + labs(title = "Group Forecast Comparison: Lower RMSE Is Better", x = "Model", y = "RMSE (CPI index points)") + theme_minimal()
save_plot(plot, "output/model_comparison_rmse.png")

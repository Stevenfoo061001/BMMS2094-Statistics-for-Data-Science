# 06_group_comparison.R - group leader script. Run after scripts 02-05.

source("scripts/00_setup.R")
required_files <- paste0("output/member_", LETTERS[1:4], "_accuracy.csv")
if (!all(file.exists(required_files))) stop("Run scripts 02-05 before running the group comparison.")

comparison <- bind_rows(lapply(required_files, read_csv, show_col_types = FALSE)) %>% arrange(RMSE)
write_csv(comparison, "output/model_comparison_summary.csv")
print(comparison)

plot <- ggplot(comparison, aes(reorder(model, RMSE), RMSE, fill = member)) + geom_col(show.legend = FALSE) + coord_flip() + labs(title = "Group Forecast Comparison: Lower RMSE Is Better", x = "Model", y = "RMSE (CPI index points)") + theme_minimal()
save_plot(plot, "output/model_comparison_rmse.png")

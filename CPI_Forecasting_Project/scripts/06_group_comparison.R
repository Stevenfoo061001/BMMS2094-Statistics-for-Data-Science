# 06_group_comparison.R - select the final model from diagnostically eligible models.

source("scripts/00_setup.R")
required_files <- paste0("output/member_", LETTERS[1:4], "_accuracy.csv")
if (!all(file.exists(required_files))) stop("Run scripts 02-05 before running the group comparison.")

all_model_results <- bind_rows(lapply(required_files, read_csv, show_col_types = FALSE))
required_columns <- c("member", "model_family", "selected_variant", "model_specification",
                      "RMSE", "Residuals_acceptable", "Overfitting_acceptable", "Diagnostic_note")
missing_columns <- setdiff(required_columns, names(all_model_results))
if (length(missing_columns) > 0) {
  stop("Member accuracy outputs are outdated. Re-run scripts 02-05. Missing columns: ",
       paste(missing_columns, collapse = ", "))
}

diagnostics_summary <- all_model_results %>%
  mutate(Final_eligible = if_else(
    Residuals_acceptable == "Yes" & Overfitting_acceptable == "Yes", "Yes", "No"
  )) %>%
  arrange(RMSE)
write_csv(diagnostics_summary, "output/model_diagnostics_summary.csv")

# This is the accuracy ranking of every tested model. It is not the final
# selection ranking because low error cannot override failed diagnostics.
accuracy_ranking <- diagnostics_summary %>% arrange(RMSE)
write_csv(accuracy_ranking, "output/model_comparison_summary.csv")

eligible_ranking <- diagnostics_summary %>%
  filter(Final_eligible == "Yes") %>%
  arrange(RMSE)
write_csv(eligible_ranking, "output/eligible_model_comparison.csv")

if (nrow(eligible_ranking) == 0) {
  stop(
    "No model passed all three checks: Ljung-Box, residual ACF, and overfitting assessment. ",
    "No final model or future forecast may be selected; test additional variants."
  )
}

selected_final_model <- eligible_ranking %>% slice(1)
write_csv(selected_final_model, "output/selected_final_model.csv")

print(accuracy_ranking %>% select(member, model_specification, RMSE, Residuals_acceptable, Final_eligible))
cat("\nSelected final model:\n")
print(selected_final_model %>% select(member, model_family, model_specification, RMSE, Diagnostic_note))

plot <- ggplot(
  accuracy_ranking,
  aes(reorder(model_specification, RMSE), RMSE, fill = Final_eligible)
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(
    title = "Group Forecast Comparison: Holdout RMSE",
    subtitle = "Green models passed Ljung-Box, residual ACF, and overfitting checks; orange models are ineligible",
    x = "Model specification", y = "RMSE (CPI index points)", fill = "Final eligible"
  ) +
  theme_minimal()
save_plot(plot, "output/model_comparison_rmse.png")

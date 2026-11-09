# Import data
.libPaths(c("r_libs", .libPaths()))
short_form_carl <- read.csv(
  "text_emodistress_formH.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Create similarity matrix
if (!requireNamespace("reticulate", quietly = TRUE)) stop("Install the reticulate package first.")
#reticulate::use_python("/opt/anaconda3/bin/python", required = TRUE)
reticulate::use_condaenv("research", required = TRUE)
sentence_transformers <- reticulate::import("sentence_transformers")
model <- sentence_transformers$SentenceTransformer("sentence-transformers/all-mpnet-base-v2")
embeddings <- model$encode(short_form_carl$Item, normalize_embeddings = TRUE)
similarity_matrix <- tcrossprod(embeddings)
dimnames(similarity_matrix) <- list(short_form_carl$Label, short_form_carl$Label)

# Factor analysis
factor_fit <- psych::fa(similarity_matrix, nfactors = 3, fm = "ml",
                        rotate = "promax", max.iter = 10000)
factor_loadings <- factor_fit$loadings[]

# Export results
id <- short_form_carl[c("Label", "Item", "Factor")]
r <- abs(factor_fit$residual[lower.tri(factor_fit$residual)])
output_file <- "pfa_emodistress_formH.xlsx"
openxlsx::write.xlsx(list(
  Pattern = cbind(id, factor_loadings),
  Phi = cbind(Factor = rownames(factor_fit$Phi), factor_fit$Phi),
  Structure = cbind(id, factor_fit$Structure[]),
  Item_statistics = data.frame(Label = id$Label, Communality = factor_fit$communality,
                               Uniqueness = factor_fit$uniquenesses),
  Fit_statistics = data.frame(Statistic = c("RMSR", "Median |residual|", "Proportion > .05", "Proportion > .10"),
                              Value = c(factor_fit$rms, median(r), mean(r > .05), mean(r > .10)))
), output_file, overwrite = TRUE)

# Scree plot
wb <- openxlsx::loadWorkbook(output_file)
openxlsx::addWorksheet(wb, "Scree_plot")
plot(eigen(similarity_matrix, symmetric = TRUE)$values, type = "b",
     xlab = "Factor", ylab = "Eigenvalue", main = "Scree plot")
openxlsx::insertPlot(wb, "Scree_plot", width = 7, height = 5, fileType = "png")
openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

# Data Setup
# Retrieving dataset that excludes country name column
env_ind_pca <- env_ind[2:30]
# Run PCA on modified Dataset 1 (env_ind) and scaling it, using `run_pca`
reduced_env_ind <- run_pca(env_ind_pca, scale = TRUE)

# Retrieving the matrix of variable loadings from the list of returned elements from `run_pca` and making it a dataframe
env_ind_loadings <- as.data.frame(reduced_env_ind$loadings)
# Mutating a column that contains the environmental indicators to the loadings dataframe and renaming the column
env_ind_loadings <- env_ind_loadings %>% 
  add_column(colnames(env_ind_pca)) %>%
  rename("indicator" = "colnames(env_ind_pca)")

# Retrieving the matrix of principal component vectors from the list of returned elements from `run_pca` and making it a dataframe
env_ind_prin_comps <- as.data.frame(reduced_env_ind$prin_comps)
# Mutating a column that contains the country names to the principal components dataframe and renaming the column
env_ind_prin_comps <- env_ind_prin_comps %>% 
  add_column(env_ind$country) %>%
  rename("country" = "env_ind$country")

# Retrieving the vector of variances explained by each principal component from the list of returned elements from `run_pca` and making it a dataframe
var_prop <- as.data.frame(reduced_env_ind$portion_var)
# Mutating a column that contains the corresponding principal component number to the variances dataframe and renaming the column. Also renaming the column that contains the variances. 
var_prop <- var_prop %>% 
  add_column(c(1:29)) %>%
  rename("variance" = "reduced_env_ind$portion_var",
         "component" = "c(1:29)")

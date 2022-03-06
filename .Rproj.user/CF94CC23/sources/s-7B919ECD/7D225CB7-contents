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
# Mutating a column that contains the corresponding principal component number to the variances dataframe and renaming the column. Also renaming the column that contains teh variances. 
var_prop <- var_prop %>% 
  add_column(c(1:29)) %>%
  rename("variance" = "reduced_env_ind$portion_var",
         "component" = "c(1:29)")

# Figure 1
# Loading for First Two Principal Components
ggplot(data = env_ind_loadings, 
       aes(x = V1, 
           y = V2,
           label = indicator)) +
  geom_point() +
  geom_text(size = 4,
            nudge_x = 0.02,
            nudge_y = 0.01,
            color = "red") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  labs(title = "Loadings for First Two Principal Components",
       x = "Loadings for Principal Component 1",
       y = "Loadings for Principal Component 2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Figure 2
# First Two Principal Components
text_df <- env_ind_prin_comps %>%
  filter(country %in% c("United States of America", 
                        "South Korea", 
                        "Viet Nam", 
                        "China", 
                        "Germany",
                        "Japan",
                        "Thailand",
                        "Sweden"))
ggplot(data = env_ind_prin_comps, 
       aes(x = V1, 
           y = V2,
           label = country)) +
  geom_point() +
  geom_text(data = text_df, 
            size = 4,
            nudge_x = .12,
            nudge_y = .12,
            color = "red") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_hline(yintercept = 0, color = "grey") +
  labs(title = "First Two Principal Components",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Figure 3
# Scree Plot 
ggplot(data = var_prop, aes(x = component, 
                            y = variance)) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Scree Plot",
       x = "Component",
       y = "Variance Explained") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_continuous(n.breaks = 29)

# Figure 4
# First Two Principal Components (Categorized by Geographical Region)
plot_df <- data.frame(pc1 = env_ind_prin_comps$V1,
                      pc2 = env_ind_prin_comps$V2,
                      region = country_data$region) 
ggplot(data = plot_df, aes(x = pc1,
                           y = pc2,
                           color = as.factor(region))) +
  geom_point(size = 2) +
  labs(title = "First Two Principal Components",
       subtitle = "Categorized by Geographical Region",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Region") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))

# Figure 5
# First Two Principal Components (Categorized by GDP)
plot_df_2 <- data.frame(pc1 = env_ind_prin_comps$V1,
                        pc2 = env_ind_prin_comps$V2,
                        gdp = country_data$gdp)
ggplot(data = plot_df_2 %>% 
         filter(!is.na(gdp)), aes(x = pc1,
                                  y = pc2,
                                  color = gdp)) +
  geom_point(size = 2) +
  labs(title = "First Two Principal Components",
       subtitle = "Categorized by GDP",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "GDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold")) +
  scale_color_continuous(low = "blue", high = "red")

# Figure 6
# First Two Principal Components (Categorized by Developed Country Type)
plot_df_3 <- data.frame(pc1 = env_ind_prin_comps$V1,
                        pc2 = env_ind_prin_comps$V2,
                        ldc = country_data$ldc) 
plot_df_3 <- plot_df_3 %>% mutate(ldc_desc = case_when(ldc == 0 ~ "Developed Country",
                                                       ldc == 1 ~ "Less Developed Country"))

ggplot(data = plot_df_3, aes(x = pc1,
                             y = pc2,
                             color = as.factor(ldc_desc))) +
  geom_point(size = 2) +
  labs(title = "First Two Principal Components",
       subtitle = "Categorized by Developed Country Type",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Developed Country Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))

# Figure 7
# Composite Indicator of Environmental Performance (EPI) vs. First Principal Component
plot_df_4 <- data.frame(pc1 = env_ind_prin_comps$V1,
                        epi_cat = country_data$EPI_cat)
ggplot(data = plot_df_4, aes(x = pc1, y = epi_cat)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Composite Indicator of Environmental Performance (EPI) vs. First Principal Component",
       x = "Principal Component 1",
       y = "EPI_cat") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"))

# Calculate correlation matrix using `cor`
cor(plot_df_4)

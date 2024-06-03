# Correlated-data-for-Numerical-Outcome
Multilevel modeling can account for the nested nature of the data, providing more accurate and reliable results by properly handling the dependencies within the data.
The data comprises 288 participants distributed across five nursing homes in Norway. This data structure is nested because multiple staff members were sampled from each nursing home. Therefore, the data has two levels of sampling: Level 1 includes staff (each row in the dataset), and Level 2 includes nursing homes (represented by the column “nhid”).

Given this nested structure, the dataset is suitable for hierarchical data analysis. In our linear mixed model, the dependent or outcome variable, perception of management (POM), is predicted by the fixed factors of position status (permanent [1] or intermediate [2]), working condition score (1-100), and Norwegian mother tongue status (yes [1] or no [2]).

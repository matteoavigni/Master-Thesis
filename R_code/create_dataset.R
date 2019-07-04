library(readxl)

my_data_final = read_excel('C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/R_code/dataset_completo.xlsx', sheet = 'prezzi')

save(my_data_final, file = "C:/Users/matte/Desktop/Master-Thesis/Master-Thesis/R_code/my_data_final.rda")

load("my_returns.rda")





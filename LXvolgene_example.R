
install.packages("devtools")

library(devtools)

install_github("gluck4668/LXvolgene")

library(LXvolgene)

#--------------------------------

data(gene_data_example)

#--------------------------------

rm(list=ls())

if(!is.null(dev.list()))
  dev.off()

gene_data = "gene_data.xlsx"

Gene_FC = NULL #(The value should be 2 or NULL )

group2= "Model"  # (Treatment is in front and control is in rear)

group1 = "Normal"


LXvolgene (gene_data,Gene_FC,group1,group2)

devtools::document()
devtools::build()


LXvolgene <- function(gene_data,Gene_FC,group1,group2){

# 查看已安装的R包
installed_packages <- data.frame(installed.packages())

# 计划要安装的R包
pack <- c("ggplot2","openxlsx","stringr","plyr","psych","dplyr",
          "ggrepel","patchwork","raster","png","janitor")

# 真正还没安装的R包
not_install <- pack[!pack %in% installed_packages$Package]

# 如果存在未安装的R包，则用sapply()函数批量安装
if (length(not_install)>0){
fun_pack <-function(x){install.packages(x,update = F,ask = F)}
sapply(not_install, fun_pack,simplify = T)}

# 批量library
fun_lib <-function(x){library(x,character.only = T)}
sapply(pack,fun_lib,simplify = T)

group_names <- paste0("(",group2," VS ",group1,")")

#----- 数据处理---------------
  source("R/data_processing.R")
  data <- data_processing(gene_data,group1,group2)

  data$p_log10 <- (-log10(data$pvalue))

  df_order <- data[data[,7]>1.301029996,]

  df_order <- df_order[order(-df_order[,7]),]

  i <- case_when(nrow(df_order)>1000 ~0.01,
                 nrow(df_order)>500 ~0.03,
                 nrow(df_order)>200 ~0.04,
                 nrow(df_order)>100 ~0.05,
                 TRUE ~0.1)

  show_n <- round(nrow(df_order)*i,0)

  df_n <- case_when(show_n >25 ~25,
                    TRUE ~show_n)

  df_new <- df_order[c(1:df_n),]

  df_show <- df_new[abs(df_new$log2FC)>=1,]

  #----------------------------------------------------
  title_gene <- c(paste("Gene volcano graphics","(",group2,"VS",group1,")"))

  source("R/volcano_01.R")

  p1 <- volcano_01(data,title_gene)

  p1

  #---------------------

  p2 <-  p1+
    geom_label_repel(data=df_show,aes(x=log2FC,y=-log10(pvalue), label=ID),
                     label.size =0.1,size=2, box.padding = 0.4, max.overlaps =show_n)

  p2


  source("R/volcano_02.R")

  p3 <- volcano_03(data,title_gene)

  p3

 #########################################################

  p4 <- p3+
    geom_label_repel(data=df_show,aes(x=log2FC,y=-log10(pvalue), label=ID),#只显示绝对值log2FC>=1的ID
                     label.size =0.1,size=2, box.padding = 0.4, max.overlaps =show_n)

  p4


  if(dir.exists("analysis result")==FALSE)
    dir.create("analysis result")

  if(is.null(Gene_FC)==TRUE)
  {gene_df <- data[,c(1:5)]
  gene_df <- gene_df[order(gene_df[,5]),]

  # group_names <- paste0("(",group1," VS ",group2,")")

  vol_df_all_file <- paste("analysis result/Gene volcano data (P0.05, unlimited FC)_all",group_names,".xlsx")

  write.xlsx(gene_df,vol_df_all_file)

  gene_dd <- gene_df
  for(i in 1:nrow(gene_dd)){
    if(substring(tolower(gene_dd[i,5]),1,3)=="not")
      gene_dd[i,5] <- NA}
  rm(i)
  gene_dd <- na.omit(gene_dd)
  vol_df_dif_file <- paste("analysis result/Gene volcano data (P0.05, unlimited FC)_diferent",group_names,".xlsx")
  write.xlsx(gene_dd,vol_df_dif_file)

  gene_p1 <- paste("analysis result/Gene Volcano graphics 01 (P0.05, unlimited FC)",group_names,".png")
  gene_p2 <- paste("analysis result/Gene Volcano graphics 02 (P0.05, unlimited FC)",group_names,".png")
  gene_p3 <- paste("analysis result/Gene Volcano graphics 03 (P0.05, unlimited FC)",group_names,".png")
  gene_p4 <- paste("analysis result/Gene Volcano graphics 04 (P0.05, unlimited FC)",group_names,".png")

  ggsave(gene_p1,p1, width=1200, height =1000, dpi=180,units = "px")
  ggsave(gene_p2,p2, width=1200, height =1000, dpi=180,units = "px")
  ggsave(gene_p3,p3, width=1400, height =1000, dpi=180,units = "px")
  ggsave(gene_p4,p4, width=1400, height =1000, dpi=180,units = "px")} else
  {gene_df <- data[,c(1:5)]
  gene_df <- gene_df[order(gene_df[,5]),]

  vol_FC2_all_file <- paste("analysis result/Gene volcano data (P0.05, FC2)_all",group_names,".xlsx")

  write.xlsx(gene_df,vol_FC2_all_file)

  gene_dd <- gene_df
  for(i in 1:nrow(gene_dd)){
    if(substring(tolower(gene_dd[i,5]),1,3)=="not")
      gene_dd[i,5] <- NA}
  rm(i)
  gene_dd <- na.omit(gene_dd)
  vol_FC2_file <- paste("analysis result/Gene volcano data (P0.05, FC2)_diferent",group_names,".xlsx")
  write.xlsx(gene_dd,vol_FC2_file)

  gene_FC2_p1 <- paste("analysis result/Gene Volcano graphics 01 (P0.05, FC2)",group_names,".png")
  gene_FC2_p2 <- paste("analysis result/Gene Volcano graphics 02 (P0.05, FC2)",group_names,".png")
  gene_FC2_p3 <- paste("analysis result/Gene Volcano graphics 03 (P0.05, FC2)",group_names,".png")
  gene_FC2_p4 <- paste("analysis result/Gene Volcano graphics 04 (P0.05, FC2)",group_names,".png")

  ggsave(gene_FC2_p1,p1, width=1200, height =1000, dpi=180,units = "px")
  ggsave(gene_FC2_p2,p2, width=1200, height =1000, dpi=180,units = "px")
  ggsave(gene_FC2_p3,p3, width=1400, height =1000, dpi=180,units = "px")
  ggsave(gene_FC2_p4,p4, width=1400, height =1000, dpi=180,units = "px")}

  p1

}


data_processing <- function(gene_data,group1,group2){

df <- read.xlsx(gene_data)

df <- na.omit(df)

group_gene <- colnames(df)[c(5:ncol(df))]
group_df <- gsub("\\d+$","", group_gene)

if(tolower(group1) %in% tolower(group_df)==FALSE)
{ g_gene <- paste(group_gene,collapse = ",")
group_g <- paste("The groups in the gene data file are",g_gene)
group_j <- paste("'",group1,"'","is not found in the gene data file. Please check it")
print(group_g)
print(group_j)
print("-----------------------------------------------------------")} else
  group_df <- group_df

if(tolower(group2) %in% tolower(group_df)==FALSE)
{ g_gene <- paste(group_gene,collapse = ",")
group_g <- paste("The groups in the gene data file are",g_gene)
group_j <- paste("'",group2,"'","is not found in the gene data file. Please check it")
print(group_g)
print(group_j)
print("-----------------------------------------------------------")} else
  rm(group_df)

####################################################
df$sum <-rowSums(df[,c(5:ncol(df))])
df <- df[df[,ncol(df)]>0,]
df <- df[,-ncol(df)]

df$sum57 <- rowMeans(df[,c(5:7)])
df$sum810 <- rowMeans(df[,c(8:10)])

group_m <- colnames(df)[c(5:10)]
group_m <- gsub("\\d+$","", group_m)

colnames(df)[11] <- group_m[1]
colnames(df)[12] <- group_m[length(group_m)]

df_max <- df[df[,2]==max(df[,2]),]

n1 <- ncol(df_max)-1
n2 <- ncol(df_max) %>% as.numeric()

if(df_max[1,n1]<df_max[1,n2])
  df_max$high <- colnames(df_max)[n2] else
    df_max$high <- colnames(df_max)[n1]

# if(tolower(group1) %in% tolower(df_max[1,ncol(df_max)]))
#   title_gene <- c(paste("Gene volcano graphics","(",group1,"VS",group2,")")) else

df <- df[,c(1:4)]
#########################################

for(i in 1:nrow(df)){
  if(substring(tolower(df[i,2]),1,3)=="inf")
    df[i,2]=NA}
rm(i)

df <- na.omit(df)

df[,2] <- as.numeric(df[,2])
df[,3] <- as.numeric(df[,3])
df[,4] <- as.numeric(df[,4])

colnames(df) <- c("ID","log2FC","pvalue","qvalue")

df <- df[order(-abs(df[,2])),]

data <- data.frame(distinct(df, ID, .keep_all = TRUE))


if(is.null(Gene_FC)==TRUE)
{data$type <- ifelse(data$log2FC>0 & data$pvalue<0.05,"Up",
                     ifelse(data$log2FC<0 & data$pvalue<0.05,"Down","Not sig")) } else
                     {data$type <- ifelse(data$log2FC>=log2(Gene_FC) & data$pvalue<0.05,"Up",
                                          ifelse(data$log2FC<=-log2(Gene_FC) & data$pvalue<0.05,"Down","Not sig"))}

change_n <- table(data$type)
change_nm <-data.frame(change_n)

Down <- c(paste("Down",c(change_nm[1,2])))#Down
No <- c(paste("Not sig",c(change_nm[2,2])))#Not sig
UP <- c(paste("Up",c(change_nm[3,2])))#Up

data$changes <- ifelse(data$type=="Up",UP,
                       ifelse(data$type=="Down",Down,No))
data_gene <- data

}



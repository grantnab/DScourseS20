df1 <- as.data.frame(iris)
df <- createDataFrame(iris)
head(select(df, df$Sepal_Length, df$Species))
head(filter(df, df$Sepal_Length>5.5))
df %>% select("Sepal_Length", "Species") %>% filter(df$Sepal_Length > 5.5)
# I could not figure out the pipe operator in SparkR

head(summarize(groupBy(df, df$Species),
               mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))

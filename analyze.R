votes2016 <- read.csv('~/Documents/2016-08-19-partisanship/votes2016.csv', stringsAsFactors=FALSE)

analyze <- function(data) {
	results <- function(data.row) {
		return(c(min(data$Republican.Yea[data.row], data$Republican.Nay[data.row]), min(data$Democrat.Yea[data.row], data$Democrat.Nay[data.row]), min(data$Senate.Republican.Yea[data.row], data$Senate.Republican.Nay[data.row]), min(data$Senate.Democrat.Yea[data.row], data$Senate.Democrat.Nay[data.row])))
	}
	output <- as.data.frame(t(sapply(1:nrow(data), results, USE.NAMES=FALSE)))
	names(output) <- c('R.House', 'D.House', 'R.Senate', 'D.Senate')
	return(output)
}
	
	
spread2016 <- analyze(votes2016)



files <- paste0('~/Documents/2016-08-19-partisanship/votes', 2016:1990, '.csv')

doIt <- function(csvfile) {
	spread <- analyze(read.csv(csvfile, stringsAsFactors=FALSE))
	write.table(x=cbind(substr(csvfile, nchar(csvfile)-7, nchar(csvfile)-4), spread), file='~/desktop/individual.csv', append=TRUE, row.names=FALSE, col.names=FALSE)
	#house <- rowSums(spread[,1:2], na.rm=TRUE)
	#senate <- rowSums(spread[,3:4], na.rm=TRUE)
	#write.table(x=cbind(substr(csvfile, nchar(csvfile)-7, nchar(csvfile)-4), house, senate), file='~/desktop/temp.csv', append=TRUE)
}

sapply(files, doIt, USE.NAMES=FALSE)
	

bills <- read.csv('~/Desktop/bills.csv', stringsAsFactors=FALSE)

ggplot(bills, aes(factor(year), house)) + 
	geom_boxplot(fill='skyblue', outlier.shape=NA) + 
	#fte_theme() + 
	labs(x='', y='') + 
	geom_boxplot(fill='green', data=bills, aes(factor(year), senate), outlier.shape=NA) + coord_flip()
	

ggsave('~/desktop/bills2.png', dpi=600)






individual <- read.csv('~/Desktop/individual.csv', stringsAsFactors=FALSE)

ggplot(individual, aes(factor(year), D.House)) + 
	geom_boxplot(fill='skyblue') + 
	geom_boxplot(data=individual, aes(factor(year), R.House), fill='salmon')


thing <- individual[,1:3]

thing$diff <- thing$R.House - thing$D.House

thing2 <- thing[,c(1,4)]

library(reshape2)
df <- melt(thing, id='year', value.name='diff')
source('~/documents/fte_theme.R')
p <- ggplot(df, aes(factor(year), value))
p + geom_boxplot(aes(fill=variable), outlier.shape=NA)


thing2$color <- ifelse(thing2$diff > 0, 'red', ifelse(thing2$diff < 0, 'blue', 'grey'))
library(ggplot2)
ggplot(thing2, aes(factor(year), diff)) + geom_rect(aes(xmin=factor(1988), xmax=factor(2018), ymin=0, ymax=150), alpha=0.00197, fill='red') + geom_rect(aes(xmin=factor(1988), xmax=factor(2018), ymin=-150, ymax=0), alpha=0.00197, fill='blue') + 
	geom_boxplot(outlier.shape=NA) + 
	coord_flip()


ggsave('~/desktop/bills1.png', dpi=600)
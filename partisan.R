library(RCurl)

getVotes <- function(webpage) {
	html <- getURL(webpage)
	if (grepl('QUORUM', html)) { return(rep(NA, 6)) }
	if (grepl('Election of the Speaker', html)) { return(rep(NA, 6)) }
	matches <- unlist(regmatches(html, gregexpr('<td>[0-9]{1,3}</td>', html)))
	values <- as.numeric(gsub('[^0-9]', '', matches))
	if (length(values) == 0) { print(webpage); return(rep(NA, 6)) } 
	if (length(values) == 4) { values <- c(values, 0, 0) }
	else if (length(values) == 8) { values <- values[-(5:6)] }
	else if (length(values) == 9) { values <- values[-c(3, 6, 9)] }
	else if (length(values) == 12) { values <- values[-c(3,6,9,10,11,12)] }
	if (length(values) != 6) { stop(paste(webpage, 'is bad')) }
	return(values)
}

house <- paste0('https://www.govtrack.us/congress/votes/111-2010/h', 1:664)
senate <- paste0('https://www.govtrack.us/congress/votes/111-2010/s', 1:299)

webpages <- c(house, senate)

getVotes(webpages[5])


votes <- t(sapply(webpages, getVotes, USE.NAMES=FALSE))
colnames(votes) <- c('Republican Yea', 'Democrat Yea', 'Republican Nay', 'Democrat Nay', 'Republican Not Voting', 'Democrat Not Voting')

write.csv(votes, '~/documents/2016-08-19-partisanship/votes2011.csv')









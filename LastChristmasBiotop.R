# Last Christmas & Spurious Correlation project for biotop

if (!require("gtrendsR")) install.packages("gtrendsR")
if (!require("reshape2")) install.packages("reshape2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("devtools")) install.packages("devtools")
if (!require("ngramr")) install_github("seancarmody/ngramr")


# From https://en.wikipedia.org/wiki/Correlation_does_not_imply_causation:
# In statistics, many statistical tests calculate correlations between variables and when two variables are found to be 
# correlated, it is tempting to assume that this shows that one variable causes the other.[1][2] 
# That "correlation proves causation" is considered a questionable cause logical fallacy when two events occurring 
# together are taken to have established a cause-and-effect relationship. 
# This fallacy is also known as cum hoc ergo propter hoc, Latin for "with this, therefore because of this", 
# and "false cause". A similar fallacy, that an event that followed another was necessarily a consequence 
# of the first event, is the post hoc ergo propter hoc (Latin for "after this, therefore because of this.") fallacy.

# XKCD comic about Correlation / Causation.
# https://xkcd.com/552/

# Here is a nice video from Kahn academy that discusses the difference between correlation and causation using a 
# real and relevant example (Is eating breakfast a possible solution to teen obesity?)
# https://www.khanacademy.org/math/probability/scatterplots-a1/creating-interpreting-scatterplots/v/correlation-and-causality


# Load data from google trends for the passed terms and filter by the passed years (default: from 2004 [begin of google trends] until 2017).
# Data is restricted to provided geographic range (default: UK)
# If summarize=T: data is summarized per year.
# @return: a table with two columns: x: date/year, y: (summarized) search interest
loadGTdata = function(terms, ymin=2004, ymax=2017, geo="", summarize=F) {
  # load google trends data for terms 
  options(stringsAsFactors=FALSE)
  raw = gtrends(terms, gprop = "web", geo=geo, time = "all")$interest_over_time
  # split date and create new table with year, month and hits
  fil = data.frame(
    year=as.numeric(format(as.Date(raw$date, "%Y/%m/%d"), format="%Y")),
    month=as.numeric(format(as.Date(raw$date, "%Y/%m/%d"), format="%m")),
    date=as.Date(raw$date, "%Y/%m/%d"),
    hits=raw$hits
  )
  # replace "<1" with "0" and create data frame
  fil <- data.frame(lapply(fil, function(x) {
    gsub("<1", "0", x)
  }))
  # convert column class of hits to numeric
  fil=data.frame(mutate(fil, year=year,month=month, hits=as.numeric(hits)))
  if (summarize) {
    # group by year and summarize over years 
    fil = summarize(group_by(fil, year), hits=sum(hits))
  }
  #  filter by min/max year
  fil = filter(fil, year <= ymax, year >= ymin)
  # create return table
  if ( summarize ) {
    ret = data.frame(x=as.numeric(fil$year), y=fil$hits)
  } else {
    ret = data.frame(x=as.Date(fil$date), y=fil$hits)
  }
  return (ret)
}

#
# Plots two GT datasets and calculates their correlation
# Own data can be passed via d1, d2.
#
plotCorr = function(title, terms1, terms2, d1=NULL, d2=NULL, ymin=2004, ymax=2017, geo="", summarize=F, xlab="", ylab="Search Interest") {
  print(geo)
  if ( is.null(d1) ) { d1 = loadGTdata(terms1, ymin, ymax, geo, summarize) }
  if ( is.null(d2) ) { d2 = loadGTdata(terms2, ymin, ymax, geo, summarize) }
  p=ggplot() + 
    geom_line(data=d1, aes(x=x, y=y, color = "blue")) + 
    geom_line(data=d2, aes(x=x, y=y, color = "red")) +
    scale_colour_discrete(name = paste0("R=", round(cor(d1$y, d2$y), digits = 2), "  "),
                          labels=c(paste(terms1, collapse=","), paste(terms2, collapse=","))) +
    theme(legend.position="bottom") +
    labs(x = xlab, y=ylab) +
    ggtitle(title)
  return(p)
}

#
# Plots two NGRAM datasets and calculates their correlation
#
plotCorrNG = function(title, terms1, terms2, ymin=1800, ymax=2017, xlab="Year", ylab="Frequency") {
  
  ng = ngram(c(terms1, terms2), year_start = ymin, year_end = ymax, case_ins = F)
  a = ng[ng$Phrase==terms1,]$Frequency
  b = ng[ng$Phrase==terms2,]$Frequency
  p = ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) + 
    geom_line()  +
    scale_colour_discrete(name = paste0("R=", round(cor(a,b), digits = 2), "  "),
                          labels=c(paste(terms1, collapse=","), paste(terms2, collapse=","))) +
    theme(legend.position="bottom") +
    labs(x = xlab, y=ylab) +
    ggtitle(title)
}

#
# set working directory
#
setwd("/Code/R/spurious-correlations-christmas")

pdf("LastChristmasSpuriousCorrelations.pdf")

#
# Print some google trend correlations
#
print(plotCorr( title="Last Christmas was in December", "Last Christmas", "December"))
print(plotCorr( title="Last Christmas I gave you advent", terms1="Last Christmas", terms2="Advent", ymin=2006))
print(plotCorr( title="Last Christmas I gave you an Egg Nog...", terms1="Last Christmas", terms2="Egg Nog", geo="US"))
print(plotCorr( title="Last Christmas I gave you a Fruit Cake...", terms1="Last Christmas", terms2="Fruit Cake", geo="US"))
print(plotCorr( title="Last Christmas I gave you...", terms1="Last Christmas", terms2="AIDS", geo="CA", ymax=2008))
print(plotCorr( title="Last Christmas I gave you my heart...", terms1="Last Christmas", terms2="Gift", geo=""))
print(plotCorr( title="Last Christmas I gave you the flu...", terms1="Last Christmas", terms2="Influenza", geo="US"))
print(plotCorr( title="Last Christmas I gave you a divorce...", terms1="Last Christmas", terms2="Divorce", geo="GB", summarize = T))

#
# Plot correlation data with external data sources.
# Load and filter data from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/divorce/datasets/divorcesinenglandandwales
#
dr.raw = data.frame(read.table("DivorceRatesUK.csv", header = T, sep = ','))
dr.year = filter(dr.raw, Year >= 2004, Year <= 2017)
dr = data.frame(x=dr.year$Year, y=dr.year$divorces/1000)
# print data, summarized by year.
print(plotCorr( title="... but the very next day you gave it away", 
                terms1="Last Christmas", 
                terms2="Divorce Rates", 
                d2=dr, 
                summarize = T, 
                ymin=2004, 
                ymax=2017, 
                geo="GB"))



# 
# Print Google NGRAMs correlation
# 
print(plotCorrNG( title="... but the very next day you gave it away (ngrams from 1800-1950)", 
                  terms1="Christmas", 
                  terms2="divorce", ymin=1800, ymax=1950))



dev.off()


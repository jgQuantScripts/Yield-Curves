require("xml2");require("dplyr");require("pbapply");require("quantmod")
# https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
# READ DAILY RATES
YEAR <- 2021
tmp <- xml2::read_xml(x = paste0("https://data.treasury.gov/feed.svc/",
                                     "DailyTreasuryYieldCurveRateData?",
                                     "$filter=year(NEW_DATE)%20eq%20",YEAR))

# EXTRACT RATES
tmp <- pblapply(as.list(1:length(xml_children(tmp))), function(ii){
  # extract data
  dat <- xml2::as_list(x=xml_child(tmp, ii))$content$properties
  # format date
  DATE <- as.Date(dat$NEW_DATE[[1]],format="%Y-%m-%dT%H:%M:%S")
  # convert to numeric
  ONE_MO <- as.numeric(dat$BC_1MONTH[[1]])
  TWO_MO <- as.numeric(dat$BC_2MONTH[[1]])
  THREE_MO<-as.numeric(dat$BC_3MONTH[[1]])
  SIX_MO <- as.numeric(dat$BC_6MONTH[[1]])
  ONE_YR <- as.numeric(dat$BC_1YEAR[[1]])
  TWO_YR <- as.numeric(dat$BC_2YEAR[[1]])
  THREE_YR<-as.numeric(dat$BC_3YEAR[[1]])
  FVE_YR <- as.numeric(dat$BC_5YEAR[[1]])
  SVN_YR <- as.numeric(dat$BC_7YEAR[[1]])
  TEN_YR <- as.numeric(dat$BC_10YEAR[[1]])
  TWTY_YR<- as.numeric(dat$BC_20YEAR[[1]])
  THRTY_YR<-as.numeric(dat$BC_30YEAR[[1]])
  # column bind rates
  dta <- cbind(ONE_MO,TWO_MO,THREE_MO,SIX_MO,ONE_YR,
               TWO_YR,THREE_YR,FVE_YR,SVN_YR,TEN_YR,
               TWTY_YR,THRTY_YR) %>% as.data.frame
  # convert to xts object
  dta <- as.xts(dta,order.by = DATE)
  # format column names
  colnames(dta) <- c("one.mo","two.mo","three.mo","six.mo","one.yr",
                     "two.yr","three.yr","five.yr","seven.yr","ten.yr",
                     "twenty.yr","thirty.yr")
  # return data (rates)
  dta
})

# ROW BIND 
RATES <- do.call(rbind,tmp) %>% suppressWarnings

# Plot SPREAD (ten year - three month rates)
# see FRED: https://fred.stlouisfed.org/series/T10Y3M
TEN.THREE <- RATES$ten.yr - RATES$three.mo
# plot(TEN.THREE,main="Ten Year - Three Month")
chartSeries(TEN.THREE, name="Ten Year - Three Month")

# Plotting yield curve
plot(x=c(1:12),y=RATES["20210601"], type="b", ylim=c(0,2.5), xaxt='n',xlab="Maturity", ylab="Rate")
lines(x=c(1:12),y=RATES["20210623"],col="blue", type="b",xaxt='n')
axis(1, at=1:12, labels=names(RATES), lwd=0.35)
legend("topleft", legend=c("June 1", "June 23"),col=c("black", "blue"), 
       lty=1:2, cex=.5, pt.cex = .5, bty="n")

# Start of the month - JUNE 2021
START = which(index(RATES) == "2021-06-01")
# plot yield curve for selected date
plot(x=c(1:12),y=coredata(RATES[START,]), type="b", ylim=c(0,2.5), xaxt='n',xlab="Maturity", ylab="Rate",col="blue")
axis(1, at=1:12, labels=names(RATES), lwd=0.35)
# plot yiel curve for the rest of the month
for(ii in START:nrow(RATES)){
lines(x=c(1:12),y=coredata(RATES[ii,]),col="grey", type="b",xaxt='n')
}
# highlight START & END Dates / make them more visible
lines(x=c(1:12),y=coredata(RATES[START,]),col="black", type="b",xaxt='n')
lines(x=c(1:12),y=coredata(RATES[nrow(RATES),]),col="red", type="b",xaxt='n')
legend("topleft", legend=c("June 1", "June 23"),col=c("black", "red"), 
       lty=1:2, cex=.5, pt.cex = .5, bty="n")

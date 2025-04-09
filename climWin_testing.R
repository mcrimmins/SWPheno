# testing ClimWin 
# MAC 12/12/22
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0167980

# vingnette https://cran.r-project.org/web/packages/climwin/vignettes/climwin.html
library(climwin)

MassWin <- slidingwin(xvar = list(Temp = MassClimate$Temp),
                      cdate = MassClimate$Date,
                      bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass),
                      cinterval = "day",
                      range = c(150, 0),
                      type = "absolute", refday = c(20, 05),
                      stat = "mean",
                      func = "lin")

MassWin[[1]]$BestModel
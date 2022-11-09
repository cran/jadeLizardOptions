library(ggplot2)
library(dplyr)
library(tibble)
library(magrittr)
#'Calculates per share (or unit of the underlying) Profit and Loss (PnL) at expiration for Reverse Jade Lizard Options Trading Strategy and draws its Bar Plot displaying the PnL.
#'@description
#'Reverse Jade Lizard is also known as the Twisted Sister Strategy. A twisted sister is a slightly bearish (or neutral), undefined upside risk strategy. This strategy is the opposite of a jade lizard. To construct this strategy we would sell an OTM call, and sell an OTM put spread at the same time. Maximum profit is achieved if the stock price ends up between the short strikes at expiration. We have found that twisted sister under-performs in the long run when compared to jade lizard because jade lizard takes advantage of volatility skew (Tastytrade, n.d.). \cr
#'Volatility skew refers to the pricing differential in equidistant OTM options. Prior to the crash of 1987, this skew was non-existent. This skew creates an environment where the puts generally trade richer than the calls, due to the fact that the velocity of a crash is much higher than that of a rally. Since the goal for both strategies is for the stock price to expire within the short strikes, our assumption is the same for both (Tastytrade, n.d.).\cr
#'Due to volatility skew normally being downside, the opportunity to utilize the twisted sister rarely arises. The single best time to use the twisted sister is when volatility skew inverts, meaning calls are trading richer than puts. This normally happens in breakout stocks after a parabolic move to the upside (Tastytrade, n.d.). \cr
#'Goals: This trade retains the premium collected when opened as long as both the short call and bull put spread remain OTM (Stultz, 2019)\cr
#'Manage: If the price of the underlying security remains within a narrow range, let the options expire worthless. If a price rally or drop occurs that threatens the short call or the vertical bull put spread, close the vulnerable position and retain the safe position until it either expires worthless or can be closed for a reasonable profit (Stultz, 2019). \cr
#'Profit: If it is an American option that can be closed before expiration then close the positions  when this strategy achieves a profit of 30 percent or more (Stultz, 2019).\cr
#'Loss: If it is an American option that can be closed before expiration the close the positions when a loss in premium value becomes 8 percent or less (Stultz, 2019).\cr
#'@details
#'#'According to conceptual details given by Stultz (2019), a closed form solution is developed for Reverse Jade Lizard Strategy and applied to draw the Bar plot in the Plots tab. Further given examples are created, to compute per share Profit and Loss at expiration for Reverse Jade Lizard Strategy and to demonstrate the strategy through its graph in the Plots tab.
#'@param ST Spot Price at time T
#'@param XLL Lower-low Strike Price or eXercise price for bought Put
#'@param XLU Lower-upper Strike Price or eXercise price for shorted put
#'@param XH Strike Price or eXercise price for shorted call
#'@param PnL Profit and Loss
#'@param lpp Long Put Premium
#'@param spp Short Put Premium
#'@param scp Short Call Premium
#'@param spot Spot Price
#'@param pl profit and loss column of the data frame
#'@param myData Data frame
#'@param myTibble tibble
#'@param hl lower bound value for setting lower-limit of x-axis displaying spot price.
#'@param hu upper bound value for setting upper-limit of x-axis displaying spot price.
#'@return graph of the strategy
#'@importFrom magrittr %>%
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 scale_fill_manual
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 geom_col
#'@importFrom tibble as_tibble
#'@importFrom dplyr mutate
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 element_line
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 geom_text
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 scale_colour_manual
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 theme
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Stultz, R. A. (2019). The option strategy desk reference: an essential reference for option traders (First edition.). Business Expert Press.\cr
#'Tastytrade. (n.d.). Twisted Sister. https://www.tastytrade.com/definitions/twisted-sister \cr
#'Kumar M (2022)._bearishTrader: Trading Strategies for Bearish Outlook_. R package version1.0.2, <URL: https://CRAN.R-project.org/package=bearishTrader>. \cr
#'R Graphics Cookbook. (n.d.). Coloring Negative and Positive Bars Differently. https://r-graphics.org/recipe-bar-graph-color-neg \cr
#'Gross C, Ottolinger P (2016)._ggThemeAssist: Add-in to Customize 'ggplot2' Themes_. R package version 0.1.5, <URL: https://CRAN.R-project.org/package=ggThemeAssist>.
#'@examples
#'reverseJadeLizardPnL(15,11,14,17,3,8,1)
#'reverseJadeLizardPnL(46,42,47,50,5,9,3,hl=0.8,hu=1.65)
#'reverseJadeLizardPnL(410,395,405,420,11,20,4,hl=0.94,hu=1.12)
#'@export
reverseJadeLizardPnL <- function (ST,XLL,XLU,XH,lpp,spp,scp,hl=0.4,hu=2.5,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  V0 <- spp + scp-lpp
  myData$pl <- pmax((XLL-myData$spot),0) - pmax((XLU-myData$spot),0)-pmax((myData$spot-XH),0) +V0
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble(myData)
  myTbColored <- myTibble %>% mutate(PnL = pl >= 0)
  ggplot(myTbColored,aes(x=spot,y=pl,fill=PnL,label=pl)) +
    geom_col(position = "identity") +
    scale_color_manual(values = c("deeppink3","darkslateblue"), guide= "none" ) +
    scale_fill_manual(values = c("#FFCCCC","powderblue"), guide= "none" ) +
    geom_point(aes(color=PnL), size=3)+
    geom_text(nudge_y = -1,size= 3, aes(color=PnL))+
    theme(plot.caption = element_text(colour  =  'gray77'))+
    theme(axis.ticks = element_line(size  =  0.9,colour="red"))+
    theme(panel.grid.major = element_line(colour  =  'lavender'))+
    theme(panel.grid.minor = element_line(colour  =  'thistle2'))+
    theme(axis.title = element_text(colour  =  'midnightblue'))+
    theme(plot.title = element_text(colour  =  'brown3', vjust  =  1))+
    theme(legend.title = element_text(colour  =  'darkgreen'))+
    theme(panel.background = element_rect(fill  =  '#F4F9FF'))+
    theme(plot.background = element_rect(fill  =  '#E8F4FF', colour  =  'aquamarine4', linetype  =  'dashed'))+
    theme(legend.key = element_rect(fill  =  'azure1', colour  =  'darkslategray', size  =  2))+
    theme(legend.background = element_rect(fill  =  NA))+
    labs(title  =  'Reverse Jade Lizard Option Strategy', x  =  'Spot Price($) at Expiration', y  =  'PnL($) at Expiration', subtitle  =  'Bearish to Neutral Outlook', caption  =  'jadeLizardOptions / MaheshP Kumar')
}


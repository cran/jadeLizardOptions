library(ggplot2)
library(dplyr)
library(tibble)
library(magrittr)
#'Calculates per share (or unit of the underlying) Profit and Loss (PnL) at expiration for Jade Lizard Options Trading Strategy and draws its Bar Plot displaying the PnL at various Spot Prices.
#'@description
#' Jade Lizard is a slightly Bullish Option Trading Strategy. It results from combining an OTM (out of the money) short put with OTM bear call spread that consists of buying a OTM higher strike call and selling somewhat lower strike but still OTM call (Stultz, 2019). A trader can use the Jade Lizard Options Strategy for underlying securities, including stocks, or stock indices (Nasdaq, 2022). In the bear call spread component of the Jade Lizard the higher strike call that you buy is further OTM than the lower strike call that you sell (Cohen, 2015). When created properly, this strategy has no upside risk. It is best suited for oversold stocks with high implied volatility (OptionStrat, n.d.). In the article titled Jade Lizard Option Strategies, Tastytrade explains that the strategy is created to have no upside risk, which is done by collecting a total credit greater than the width of the short call spread (Tastytrade, n.d.). OptionStrat demonstrates execution of Jade Lizard Strategy on a stock from North American Markets (OptionStrat, n.d.) and Strategy Builder from Opstra Definedge demonstrates its application in stock index like Nifty or a constituent stock. The Bar Plot clearly shows that the PnL Plotting of Jade Lizard Strategy looks quite similar to the shape that of a lizard.
#'Goals: This trade retains the premium collected when opened as long as both the short put and vertical bear call spread remain OTM (Stultz, 2019).\cr
#'Manage: If the price of the underlying security remains within a narrow range, let the options expire worthless. If it is an American option that can be closed before expiration and if a price rally or drop occurs that threatens the short put or the bear call vertical spread, close the vulnerable positions and retain the safe position until it either expires worthless or can be closed for a profit (Stultz, 2019).\cr
#'Profit: If it is an American option that can be closed before expiration then close the positions when this strategy achieves a profit of 30 percent or more. If the options remain OTM, consider letting the trade expire worthless(Stultz, 2019).\cr
#'Loss: If it is an American option that can be closed before expiration the close the positions when a loss in premium value becomes 8 percent or less (Stultz, 2019).\cr
#'@details
#'According to conceptual details given by Stultz (2019), and a closed form solution is developed for Jade Lizard Strategy and applied to draw the Bar plot in the Plots tab. Further given examples are created, to compute per share Profit and Loss at expiration for Jade Lizard Strategy to demonstrate the strategy through its graph in the Plots tab.
#'@param ST Spot Price at time T
#'@param XHU Higher-Upper Strike Price or eXercise price for bought Call
#'@param XHL Higher-Low Strike Price or eXercise price for shorted call
#'@param XM Strike Price or eXercise price for OTM shorted Put
#'@param PnL Profit and Loss
#'@param lcp Long Call Premium
#'@param scp Short Call Premium
#'@param spp Short Put Premium
#'@param spot Spot Price
#'@param pl Profit and Loss column in data frame
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
#'Nasdaq (2022, April 11). Ultimate Guide to the Jade Lizard Options Strategy. https://www.nasdaq.com/articles/ultimate-guide-to-the-jade-lizard-options-strategy \cr
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
#'OptionStrat. (n.d.). What is a jade lizard? https://optionstrat.com/build/jade-lizard \cr
#'Tastytrade. (n.d.). Jade Lizard Option Strategies. https://www.tastytrade.com/concepts-strategies/jade-lizard \cr
#'https://opstra.definedge.com/strategy-builder \cr
#'R Graphics Cookbook. (n.d.). Coloring Negative and Positive Bars Differently. https://r-graphics.org/recipe-bar-graph-color-neg \cr
#'Gross C, Ottolinger P (2016)._ggThemeAssist: Add-in to Customize 'ggplot2' Themes_. R package version 0.1.5, <URL: https://CRAN.R-project.org/package=ggThemeAssist>.
#'@examples
#'jadeLizardPnL(10,17,12,15,1,2,5)
#'jadeLizardPnL(40,45,34,40,2,6,11,hl=0.25,hu=1.25)
#'jadeLizardPnL(383.7,405,395,385,3.85,6.35,11,hl=0.92,hu=1.075)
#'@export
jadeLizardPnL <- function (ST,XHU,XHL,XM,lcp,scp,spp,hl=0,hu=1.9,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  V0 <- spp + scp -lcp
  myData$pl <- pmax((myData$spot-XHU),0) - pmax((myData$spot-XHL),0)- pmax((XM-myData$spot),0)+V0
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble(myData)
  myTbColored <- myTibble %>% mutate(PnL = pl >= 0)
  ggplot(myTbColored,aes(x=spot,y=pl,fill=PnL,label=pl)) +
    geom_col(position = "identity") +
    scale_color_manual(values = c("deeppink3","darkslateblue"), guide= "none" ) +
    scale_fill_manual(values = c("#FFCCCC","powderblue"), guide= "none" ) +
    geom_point(aes(color=PnL), size=3)+
    geom_text(nudge_y = -0.8,size= 3, aes(color=PnL))+
    theme(plot.caption = element_text(colour  =  'gray77'))+
    theme(axis.ticks = element_line(size  =  0.9,colour="red"))+
    theme(panel.grid.major = element_line(colour  =  'lavender',size  =  0.4))+
    theme(panel.grid.minor = element_line(colour  =  'thistle2',size  =  0.3))+
    theme(axis.title = element_text(colour  =  'midnightblue'))+
    theme(plot.title = element_text(colour  =  'brown3', vjust  =  1))+
    theme(legend.title = element_text(colour  =  'darkgreen'))+
    theme(panel.background = element_rect(fill  =  '#F4F9FF'))+
    theme(plot.background = element_rect(fill  =  '#E8F4FF', colour  =  'aquamarine4', linetype  =  'dashed'))+
    theme(legend.key = element_rect(fill  =  'azure1', colour  =  'darkslategray', size  =  2))+
    theme(legend.background = element_rect(fill  =  NA))+
    labs(title  =  'Jade Lizard Option Strategy', x  =  'Spot Price($) at Expiration', y  =  'PnL($) at Expiration', subtitle  =  'Bullish to Neutral Outlook ', caption  =  'jadeLizardOptions / MaheshP Kumar')
}


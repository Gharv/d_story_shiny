#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(devtools)
devtools::install_github("ufarcs-prod/mydatastory")
library(mydatastory)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Two Sample T-Test"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("mu1",
                     "Population Mean 1",
                     min = -10,
                     max = 10,
                     value = 0,
                     step = 0.1),
         
         sliderInput("mu2",
                     "Population Mean 2",
                     min = -10,
                     max = 10,
                     value = 0.5,
                     step = 0.1),
         
         sliderInput("s1",
                     "Population Standard Deviation 1",
                     min = 2,
                     max = 4,
                     value = 3,
                     step = 0.1),
         
         sliderInput("s2",
                     "Population Standard Deviation 2",
                     min = 2,
                     max = 4,
                     value = 3,
                     step = 0.1),
         
         sliderInput("n1",
                     "Popluation Sample 1",
                     min = 30,
                     max = 100,
                     value = 30),
         
         sliderInput("n2",
                     "Population Sample 2",
                     min = 30,
                     max = 100,
                     value = 30),
         
         sliderInput("alpha",
                     "Alpha",
                     min = 0,
                     max = 1,
                     value = .05,
                     step = 0.01)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot1"),
        htmlOutput("text1"),
        htmlOutput("text2"),
        htmlOutput("text3"),
        htmlOutput("text4"),
        htmlOutput("text5"),
        htmlOutput("text6"),
        plotOutput("plot2")
         #plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$text1 <- renderText({
    mu1     <- input$mu1
    mu2     <- input$mu2
    s1      <- input$s1
    s2      <- input$s2
    n1      <- input$n1
    n2      <- input$n2
    alpha   <- input$alpha
    
          #<br> is used to make a new line
    paste("The above graph has <font color='blue'>Population 1 in blue</font> and <font color='green'>Population 2 in green</font> with thier respective means. <br><br>",
          "First we make <b>3</b> assumptions when preforming a two sample t-test. <br><br>",
          "<b>1.</b> Are these independent samples? <br>",
          "<b>2.</b> Are these large samples or a normal population? <br>",
          # &emsp; is a way to tab in html, <sub> and </sub> are used to make subscripts
          "&emsp; n<sub>1</sub> =", n1, "and n<sub>2</sub> =", n2, "<br><br>",
          "Large samples are sometimes defined differently by different people in different <br>",
          "areas of research. It usally falls somewhere between 22-30. Here we will use the <br> ",
          "conservative sizing of 30. Meaning we are testing for n<sub>1</sub> < 30 and n<sub>2</sub> < 30. <br><br>")
    
  })
  
  output$text2 <- renderText({
    
    n1      <- input$n1
    n2      <- input$n2
    
    ifelse(n1 < 30 & n2 <30, 
           paste("We do not have large enough samples so we need to check the normality assumption from both populations. <br><br>"),
           paste("We have large enough samples so we do not need to check the normality assumption. <br><br>"))
    
  })
  
  output$text3 <- renderText({
    paste("<b>3.</b> Do the populations have equal variance? <br>")
  })
  # Need to come back and talk about Welch's t-test for unequal variances.
  output$text4 <- renderText({
    s1     <- input$s1
    s2     <- input$s2
    ratio  <- s1/s2
    
    ifelse(ratio >= .5 & ratio <= 2,
           paste("Yes, since s<sub>1</sub> and s<sub>2</sub> are not very different. We use a common rule of thumb here. <br>",
                 "We test the ratio of s<sub>1</sub>/s<sub>2</sub> is between 0.5 and 2 and it is", round(ratio, 3), "which is inside our interval. <br><br>"),
           paste("No, since s<sub>1</sub> and s<sub>2</sub> are significantly different. We use a common rule of thumb here. <br>",
                 "We test the ratio of s<sub>1</sub>/s<sub>2</sub> is between 0.5 and 2 and it is", round(ratio, 3), "which is outside our interval. <br><br>"))
  })
  
  output$text5 <- renderText({
    mu1     <- input$mu1
    mu2     <- input$mu2
    s1      <- input$s1
    s2      <- input$s2
    n1      <- input$n1
    n2      <- input$n2
    alpha   <- input$alpha
    
    sp <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2) )
    t  <- (mu1 - mu2) / (sp * sqrt((1/n1) + (1/n2)))
    
    paste("Now that we have checked and satisfied our assumptions we can continue with our two sample t-test. <br>",
          "We will need to calculate the <b>pooled standard deviation</b> and the test statistic which is the <b>t-value</b>, given below: <br><br>",
          "<img src = 'http://res.cloudinary.com/gharv/image/upload/v1522182485/two-t-test_wkaja9.png'><br>",
          "Now we can set up our t-test: <br>",
          "<b>Step 1. </b><br>",
          "&emsp; <i>H<sub>0</sub> : &mu;<sub>1</sub> - &mu;<sub>2</sub></i> = 0 <br>",
          "&emsp; <i>H<sub>a</sub> : &mu;<sub>1</sub> - &mu;<sub>2</sub></i> &ne; 0 <br><br>",
          "<b>Step 2.</b> Significance level: <br>",
          "&emsp; <i>&alpha;</i> = ", alpha, "<br><br>",
          "<b>Step 3.</b> Compute the <i>t</i>-statistic: <br>",
          "&emsp; So pluging in the numbers we get <b>s<sub>p</sub></b> =", round(sp, 3), "and our <b>t*</b> =", round(t, 4), "<br><br>",
          "<b>Step 4.</b> Critical value: <br>",
          "&emsp; Two-tailed test <br>",
          "&emsp; Critical value = <i>t<sub>1 - &alpha;/2</sub></i> = t<sub>", 1 - alpha/2, "</sub><br>",
          "&emsp; Degrees of freedom =", n1, "+", n2, "- 2 =", n1+n2-2, "<br>",
          "&emsp; t<sub>", 1 -  alpha/2, "</sub> =", round(qt(.975, df = n1+n2-2), 4), "<br>", 
          "&emsp; Rejection region |<i>t*</i>| >", round(qt(.975, df = n1+n2-2), 4), "<br><br>",
          "<b>Step 5.</b> Check to see if the value of the test statistic is greater then our rejection region to decide if to reject <i>H<sub>0</sub></i>.<br>"
          )
  })
  
  output$text6 <- renderText({
    mu1     <- input$mu1
    mu2     <- input$mu2
    s1      <- input$s1
    s2      <- input$s2
    n1      <- input$n1
    n2      <- input$n2
    alpha   <- input$alpha
    
    sp <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2) )
    t  <- (mu1 - mu2) / (sp * sqrt((1/n1) + (1/n2)))
    
    ifelse(abs(round(t, 4)) > round(qt(.975, df = n1+n2-2), 4),
           paste("&emsp; |<i>t*</i>| =", abs(round(t, 4)), ">", round(qt(.975, df = n1+n2-2), 4), "<br>",
                 "We see that the test statistic |<i>t*</i>| is greater then our rejection region, so we reject the <i>H<sub>0</sub></i> with 1 - &alpha; confidence."),
           paste("&emsp; |<i>t*</i>| =", abs(round(t, 4)), "<", round(qt(.975, df = n1+n2-2), 4), "<br>",
                 "We see that the test statistic is less than our rejection region so we will fail to reject the <i>H<sub>0</sub></i>.")
          )
  })
  
  output$plot1 <- renderPlot({
    
    # Save input values as variables
    mu1     <- input$mu1
    mu2     <- input$mu2
    s1      <- input$s1
    s2      <- input$s2
    n1      <- input$n1
    n2      <- input$n2
    alpha   <- input$alpha
    
    # Generate fake data
    data1 <- renorm(n = n1, x_bar = mu1, sdv = s1)
    data2 <- renorm(n = n2, x_bar = mu2, sdv = s2)
    
    # Create data frame of fake data
    df  <- data.frame(x1 = data1, x2 = data2)
    
    # Create tmp value to store height of density of x1 and x2 to change height of graph
    # We take the density of each data set for the y value and take the max of each of those.
    tmp <- max(density(df$x1)$y, density(df$x2)$y)
    
    # Plot both densities with thier means as ablines
    plot(density(df$x1), main = "Density of Both Populations", col = "blue", ylim = c(0,tmp), xlab = "X")
    abline(v = mu1, col = "blue")
    lines(density(df$x2), col = "green")
    abline(v = mu2, col = "green")
  })
  
  output$plot2 <- renderPlot({
    
    # Save input values as variables
    mu1     <- input$mu1
    mu2     <- input$mu2
    s1      <- input$s1
    s2      <- input$s2
    n1      <- input$n1
    n2      <- input$n2
    alpha   <- input$alpha
    
    # Generate fake data
    data1 <- renorm(n = n1, x_bar = mu1, sdv = s1)
    data2 <- renorm(n = n2, x_bar = mu2, sdv = s2)
    
    # Create data frame of fake data
    df  <- data.frame(x1 = data1, x2 = data2)
    
    t <- t.test(x = df$x1, y = df$x2, var.equal = T)
    
    # Create polygon variables
    cord.x1 <- c(-4, seq(-4, qt(.025, df = n1+n2-2), 0.01), qt(.025, df = n1+n2-2))
    cord.y1 <- c(0, dnorm(seq(-4, qt(.025, df = n1+n2-2), 0.01)), 0)
    
    cord.x2 <- c(seq(qt(.975, df = n1+n2-2), 4, 0.01), 4, qt(.975, df = n1+n2-2))
    cord.y2 <- c(0, dnorm(seq(qt(.975, df = n1+n2-2), 4, 0.01)), 0)
    
    # Plot both densities with thier means as ablines
    x  <- seq(-4, 4, length = 100)
    hx <- dnorm(x)
    plot(x, hx, main = "T Test", type = "l", col = "orange", xlab = "t value", yaxt = "n", ylab = "")
    abline(v = t$statistic)
    polygon(cord.x1, cord.y1, col = "green")
    polygon(cord.x2, cord.y2, col = "green")
    text(-3, 0.1, "Rejection Region", col = "green")
    text(3, 0.1, "Rejection Region", col = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


libary(shiny)

ui <- fluidPage(

  titlePanel("Z critical values"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("CI", "Confidence Interval:",
			min = 0,
			max = 1,
    			value = .95),
      br(),
      radioButtons("way", "Distribution tire",
			c("Two-way" = "two",
			  "One-way" = "one"))
      ),
    mainPanel(
      plotOutput("plot")
      )
    )
  )


server <- function(input, output){
  output$plot <- renderPlot({

    CI <- input$CI
    
    curve(dnorm(x), -3.5, 3.5, ylab = "Density", xaxt = "n", bty = "n",
      ylim = c(-.05, .4), lwd = 2)
    abline(h = 0, lty = 2)
  
    if(input$way == "two"){
      title(paste("Z critical values for two-way distribution\n",
        "(Alpha=", 1-CI, ")", sep = ""), cex = 1.5) 

      vx1 <- qnorm((1-CI)/2)
      vx2 <- qnorm((1+CI)/2)
      vy1 <- dnorm(vx1)
      vy2 <- dnorm(vx2)
      lines(c(vx1, vx1), c(0, vy1), lty = 2)
      lines(c(vx2, vx2), c(0, vy2), lty = 2)

      xv1 <- seq(-3.5, vx1, .01)
      xv2 <- seq(vx2, 3.5, .01)
      yv1 <- dnorm(xv1)
      yv2 <- dnorm(xv2)

      polygon(c(-3.5, xv1, vx1), c(0, yv1, 0), col = "grey", lty = 2)
      polygon(c(vx2, xv2, 3.5), c(0, yv2, 0), col = "grey", lty = 2)

      points(c(vx1, vx2), c(0, 0), pch = 16)
      text(c(vx1, vx2), c(0, 0), c(round(vx1, 2), round(vx2, 2)), pos = 1, 
        font = 2, col = "red", cex = 1.5)

      text(0, .20, paste("%", CI*100, sep = ""), cex = 1.5)
      text(c(-3, 3), .05, paste("%", (1-CI)/2 * 100, sep = ""), cex = 1.5)
      }

    if(input$way == "one"){
      title(paste("Z critical values for one-way distribution\n",
        "(Alpha=", 1-CI, ")", sep = ""), cex = 1.5) 

      vx1 <- qnorm(CI)
      vy1 <- dnorm(vx1)
      lines(c(vx1, vx1), c(0, vy1), lty = 2)
  
      xv1 <- seq(vx1, 3.5, .01)
      yv1 <- dnorm(xv1)

      polygon(c(vx1, xv1, 3.5), c(0, yv1, 0), col = "grey", lty = 2)
    
      points(vx1, 0, pch = 16)
      text(vx1, 0, round(vx1, 2), pos = 1, font = 2, col = "red", cex = 1.5)

      text(0, .20, paste("%", CI*100, sep = ""), cex = 1.5)
      text(3, .05, paste("%", (1-CI) * 100, sep = ""), cex = 1.5)
      }
      })
    }

shinyApp(ui, server)
  

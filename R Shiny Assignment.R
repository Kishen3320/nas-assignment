library(shiny)
#install.packages("DT")
#install.packages("shinythemes")
library(shinythemes)
library(DT)


#define data
data=matrix(c(524792,743057,745282,0,798502,995659,0,0,917636,0,0,0),3,4,byrow=T)



# Define UI
ui<-fluidPage (
  
  theme=shinytheme("cerulean"),
  
  titlePanel("IBNR Estimate"),
  
  sidebarLayout(
    position="right",
    
    sidebarPanel(
      
      sliderInput(inputId="tail",
                  label = "Tail Development Factor",
                  min=1,
                  max=2,
                  value=1.1)
    ),
    
    mainPanel(
      plotOutput(outputId="distPlot"),
      DT::dataTableOutput("static")
      
    )
  )
)

#Define Server

server <- function(input,output){
  
  output$distPlot <- renderPlot({

    dev_fac=c()
    for(i in 2:3){
      col=colSums(data !=0)[i]
      factor=sum(data[,i])/sum(data[1:col,i-1])
      dev_fac=c(dev_fac,factor)
    }
    dev_fac=c(dev_fac,input$tail)
    dev_fac
    for(i in 1:3){
      for(j in 1:4){
        if(data[i,j]==0){
          data[i,j]=data[i,j-1]*dev_fac[j-1]
        }
      }
    }
    
    data=round(data)
 
    
    
    plot(data[3,],type='b',ylim=c(min(data),max(data)+100000),xaxt="n",xlab="Development Year",ylab="Cummulative Paid Claims ($)")
    text(1:ncol(data),data[3,],labels=data[3,], pos=3,cex=0.8,lwd=2)
    text(1:ncol(data),data[2,],labels=data[2,], pos=3,cex=0.8,lwd=2)
    text(1:ncol(data),data[1,],labels=data[1,], pos=3,cex=0.8)
    title(main="Developed Paid Claims")
    axis(1,at=1:ncol(data))
    lines(data[2,],col=2,type="b")
    lines(data[1,],type="b",col=3)
    legend("topleft",legend=c(2017,2018,2019),pch=c(1,1,1),col=c(3,2,1),lty=c(1,1,1),lwd=2)
    
    test=data
    
  })
  
  plot_func <- function(input){
    
    dev_fac=c()
    for(i in 2:3){
      col=colSums(data !=0)[i]
      factor=sum(data[,i])/sum(data[1:col,i-1])
      dev_fac=c(dev_fac,factor)
    }
    dev_fac=c(dev_fac,input)
    dev_fac
    for(i in 1:3){
      for(j in 1:4){
        if(data[i,j]==0){
          data[i,j]=data[i,j-1]*dev_fac[j-1]
        }
      }
    }  
    
    data=round(data)
    data=cbind(c(2017,2018,2019),data)
  
  }
  
 
  
  output$static <- DT::renderDataTable(plot_func(input$tail), 
                                       rownames=FALSE,
                                       container=sketch,
                                       fillContainer=TRUE
                                       )
}

# Create Shiny Objects
shinyApp(ui=ui, server=server)

??renderDataTable
'data=matrix(c(524792,743057,745282,0,798502,995659,0,0,917636,0,0,0),3,4,byrow=T)

data=as.data.frame(data,col.names=c(2017,2018,2019),row.names=c(1,2,3))
data
'

# a custom table container

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    
    tr(
      th(rowspan = 2, 'Loss Year'),
      th(colspan = 2, ''),
      th(colspan=4,'Development Year')
    ),
    tr(
      lapply(c(1,2,3,4), th)
      )
    )
  )
  
  )



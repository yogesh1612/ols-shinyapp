#################################################
#      Summary & OLS App                      #
#################################################

library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(jtools)
# library(gplot)

shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})


pred.readdata <- reactive({
  if (is.null(input$filep)) { return(NULL) }
  else{
    readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
    return(readdata)
  }
})

# Select variables:
output$yvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  selectInput("yAttr", "Select Y variable",
                     colnames(Dataset()), colnames(Dataset())[1])
  
})

output$xvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  checkboxGroupInput("xAttr", "Select X variables",
                     setdiff(colnames(Dataset()),input$yAttr), setdiff(colnames(Dataset()),input$yAttr))
  
})

output$fxvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  checkboxGroupInput("fxAttr", "Select factor variable in X",
                     setdiff(colnames(Dataset()),input$yAttr),"" )
  
})

mydata = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]

  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
  
})


Dataset.Predict <- reactive({
  fxc = setdiff(input$fxAttr, input$yAttr)
  mydata = pred.readdata()[,c(input$xAttr)]
  
  if (length(fxc) >= 1){
    for (j in 1:length(fxc)){
      mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
    }
  }
  return(mydata)
})

out = reactive({
data = mydata()
Dimensions = dim(data)
Head = head(data)
Tail = tail(data)
Class = NULL
for (i in 1:ncol(data)){
  c1 = class(data[,i])
  Class = c(Class, c1)
}

nu = which(Class %in% c("numeric","integer"))
fa = which(Class %in% c("factor","character"))
nu.data = data[,nu] 
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,2), factor.data = describe(fa.data))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
return(out)
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})

# output$scatterplots <- renderUI({
#   if (is.null(input$file)) {return(NULL)}
#   else {
#     
#     plot_output_list <- lapply(1:out()[[7]], function(i) {
#       plotname <- paste("plot", i, sep="")
#       plotOutput(plotname, height = 700, width = 700)
#     })
#     # Convert the list to a tagList - this is necessary for the list of items
#     # to display properly.
#     do.call(tagList, plot_output_list)
#   }
# })
# 
# # Call renderPlot for each one. Plots are only actually generated when they
# # are visible on the web page.
# max_plots = 50
# 
# for (i in 1:max_plots) {
#   # Need local so that each item gets its own number. Without it, the value
#   # of i in the renderPlot() will be the same across all instances, because
#   # of when the expression is evaluated.
#   local({
#     
#     my_i <- i 
#     plotname <- paste("plot", my_i, sep="")
#     
#     output[[plotname]] <- renderPlot({
#       out1 = out()
#       a = out1[[6]]
#       j = my_i
#       if (ncol(out1[[5]]) == a[j] + 1){
#         a1 = a[j]+1
#         a2 = a[j]-1
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#         }
#       
#       else if ( ncol(out1[[5]]) < a[j + 1]){
#         a1 = a[j]+1
#         a2 = ncol(out1[[5]])
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#       }
#       
#       else if(ncol(out1[[5]]) > a[j + 1]){
#         a1 = a[j]+1
#         a2 = a[j + 1]
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#       }
#       
#       mtext(paste("Scater plot " ,my_i), side = 3, line = 2, cex=2)
#         })
#   })
# }

output$heatmap = renderPlot({ 
  
    qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
  
})

output$correlation = renderPrint({
  round(cor(out()[[5]], use = "pairwise.complete.obs"),2)
  })

ols = reactive({
    rhs = paste(input$xAttr, collapse = "+")
    ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())
  return(ols)
})

ols2 = reactive({
  
  drop = which(input$yAttr == colnames(out()[[5]]))
               
  x0 = out()[[5]][,-drop]
  x01 = scale(x0, center = T, scale = T)
  
  y = out()[[5]][,drop]
  
  dstd = data.frame(y,x01)
  colnames(dstd) = c(input$yAttr,colnames(x01))
  
  if (ncol(data.frame(out()[[4]])) == 1) {
    fdata = data.frame(out()[[4]])
    colnames(fdata) = input$fxAttr
    dstd = data.frame(dstd,fdata)
  }
  
  else if (ncol(data.frame(out()[[4]])) > 1) {
    fdata = data.frame(out()[[4]])
    dstd = data.frame(dstd,fdata)
  }
  
  rhs = paste(input$xAttr, collapse = "+")
  ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = dstd)
  return(ols)

  })

output$resplot1 = renderPlot({
  plot(ols()$residuals)
})

output$resplot2 = renderPlot({
  plot(ols()$residuals,ols()$fitted.values)
})

output$resplot3 = renderPlot({
  plot(mydata()[,input$yAttr],ols()$fitted.values)#
})


output$olssummary <- renderPrint({
  rhs = paste(input$xAttr, collapse = " + ")
  frmla = paste(input$yAttr," = ", rhs , sep="")
  frmla
  })

output$residual <- renderPrint({round(summary(ols()$residuals),2)})

output$coeff <- renderDataTable({
  a<-summary(ols())
  round(a$coefficients,3)
})
  

output$text <- renderPrint({
  a<-summary(ols())
  str1 <- paste("R-squared:", round(a$r.squared,2))
  str2 <- paste("Adjusted R-squared:",round(a$adj.r.squared,2))
  paste(str1, str2, sep = ' | ')
  
})



output$olssummarystd = renderPrint({
  rhs = paste(input$xAttr, collapse = " + ")
  frmla = paste(input$yAttr," = ", rhs , sep="")
  frmla
})

output$residual1 <- renderPrint({round(summary(ols2()$residuals),2)})

output$coeff1 <- renderDataTable({
  a<-summary(ols2())
  round(a$coefficients,3)
})


output$text1 <- renderPrint({
  a<-summary(ols2())
  str1 <- paste("R-squared:", round(a$r.squared,2))
  str2 <- paste("Adjusted R-squared:",round(a$adj.r.squared,2))
  paste(str1, str2, sep = ' | ')
  
})

output$sampleols <- renderPrint({summ(ols())})

output$datatable = renderTable({
  Y.hat = ols()$fitted.values
  data.frame(Y.hat,mydata())
})


prediction = reactive({
  val = predict(ols(),Dataset.Predict())
  out = data.frame(Yhat = val, pred.readdata())
})

output$prediction =  renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  head(prediction(),10)
})

#------------------------------------------------#
output$downloadData1 <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(prediction(), file, row.names=F, col.names=F)
  }
)
output$downloadData <- downloadHandler(
  filename = function() { "beer data.csv" },
  content = function(file) {
    write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData2 <- downloadHandler(
  filename = function() { "beer data - prediction sample.csv" },
  content = function(file) {
    write.csv(read.csv("data/beer data - prediction sample.csv"), file, row.names=F, col.names=F)
  }
)

})


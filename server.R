library(cluster)
library(fpc)
library(devtools)
library(dplyr)
library(fmsb)
library(dataframes2xls)
#advance
library(ggplot2)
library(plotrix)
library(ggfortify)
#library(weightedKmeans)
bData <- data.frame()
shinyServer(function(input, output, session) 
{
  
  data1 <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
      #return(read.csv("out.csv",header=T))
    
    #y <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
    #quote=input$quote)
   read.csv(inFile$datapath, header=T)
   #read.csv("out.csv",header=T)
  })
  getHead <- reactive({
    #query <- parseQueryString(session$clientData$url_search)	
    #pry_name <- as.character(paste(names(query), query, collapse=" "))
    #pry_name <- names(query)[1]
    
    #uid <- names(query)[2]
    #system(paste("java -jar dtl_generator.jar",pry_name,sep=" "))
    #x <- scan("dtl.txt",what="",sep="\n")
    x <- names(data1())
    
    
  })
 
  output$code <- renderText({ paste("Survey Code:",getCode(),collapse=" ")})
  data2 <- reactive({
    
    myData <- data1()
    vars <- getHead()
    x <- myData[input$obs[1]:input$obs[2],input$vars[1]:input$vars[2]]
    names(x) <- vars[input$vars[1]:input$vars[2]]
    #x <- gsub("."," ",x)
    
    x
    
    
  })
  
  
  output$slide1 <- renderUI({
    
    sliderInput("obs","Observations", 1, nrow(data1()), step =1,value=c(1,nrow(data1())))
  })
  output$slide2 <- renderUI({
 
    sliderInput("vars","Variables", 1, ncol(data1()), step =1,value=c(1,ncol(data1())))
  })
  output$slide3 <- renderUI({
    df = data1()
    if(nrow(df) == 0)
      return(null)
  sliderInput("nclust","Number of Clusters", 2, 20, step =1,value=5)
  })
  output$choice1 <- renderUI({
    df = data1()
    if(nrow(df) == 0)
      return(null)
  selectInput('selection','Options',
              c("Overview","Statistical Analysis","Cluster Plot","Scope Chart"))
  })
  output$subdia <- renderUI({
    if(input$selection != "Statistical Analysis")
      return(NULL)
    selectInput('op_selection','Statistics',
                c("Means","Standard Deviation","Standard Score"))
  })
  output$pcatab <- renderUI({
    if(input$selection != "Scope Chart")
      return(NULL)
    selectInput('tab_select','Details',
                c("PC Summary","PC's for Variables"))
  })
  
  output$button <- renderUI({
    
    if(input$selection == "Statistical Analysis" && input$op_selection == "Means"){
      return(downloadButton('export','Download Stats'))}
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Deviation"){
      return(downloadButton('export5','Download Stats'))}
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Score"){
      return(downloadButton('export6','Download Stats'))}
    
  })
  
  output$button2 <- renderUI({
    
    if(input$selection == "Cluster Analysis"){
      return(downloadButton('export0','Export'))}
    if(input$selection == "Scope Chart")
      return(downloadButton('export1','Export'))
    if(input$selection == "Comparision Plot")
      return(downloadButton('export2','Export'))
    if(input$selection == "Cluster Plot")
      return(downloadButton('export3','Export'))
    if(input$selection == "Statistical Analysis" && input$op_selection == "Means"){
      return(downloadButton('export4','Export'))}
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Deviation"){
      return(downloadButton('export41','Export'))}
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Score"){
      return(downloadButton('export42','Export'))}
  })
  
  clusters <- reactive({
    
    set.seed(1)
    kmeans(data.frame(data2()), input$nclust,iter.max = 50)
    #ewkm(data.frame(data2()), input$nclust, lambda=0.5, maxiter=100)
  })
  clustersStd <- reactive({
    
    set.seed(1)
    kmeans(scale(data.frame(data2())), input$nclust,iter.max = 50)
    #ewkm(data.frame(data2()), input$nclust, lambda=0.5, maxiter=100)
  })



  pca <- reactive({
    
    x<- data.frame(data1())
    x <- x[input$obs[1]:input$obs[2],input$vars[1]:input$vars[2]]
    names(x) <- paste("Var",input$vars[1]:input$vars[2],sep="-")
    prcomp(x,scale=T)
  })
  pca2 <- reactive({
    
    x <- data2()
    prcomp(x,scale=T)
  })
  pca3 <- reactive({
    x <- data2()
    prcomp(x,scale=T)
  })
  output$desc1 <- renderText({
    if(input$selection == "Cluster Plot")
    {
    a <- summary(pca2())
    b <- ((a$importance[2,1]+a$importance[2,2])*100)
    paste("Above cluster graph has",b,"% variance of total data. That represents accuracy of clusters.",collapse=" ")
    }
    else if(input$selection == "Overview")
    {
      "Clusters are formed with K-means cluster algorithm. Distributions are:"
    }
    else if(input$selection == "Statistical Analysis")
    {
      "It represents Mean, standard Deviation and Standard Score for clusters."
    }
    else if(input$selection == "Scope Chart")
    {
      a <- summary(pca2())
      b <- ((a$importance[2,1]+a$importance[2,2])*100)
      paste("Arrow shows trend of a variable in observations in",b,"% variance of total data. That represents accuracy of graph. Calculated PC's are:",collapse=" ")
    }
    else
      return()
  })
  
  output$cmean <- renderTable({
    
    
    
    if(input$selection == "Overview")
    {
    
    
      x<-  data.frame(clusters()$size)
      x <- data.frame(t(x))
      x <- rbind(x,paste(round(prop.table(table(clusters()$cluster))*100,2),"%",sep=""))
      names(x) <- paste("Cluster",1:input$nclust,sep="-")
      rownames(x)[1] <-"Respondent Count"
      rownames(x)[2] <- "Percentage"
      x
    
     
    }
    else if(input$selection == "Statistical Analysis" && input$op_selection == "Means")
    {
      x <- data.frame(t(data.frame(clusters()$centers)))
      names(x) <- paste("Cluster",1:input$nclust,sep="-")
      vars <- getHead()
      vars <- vars[vars!=""]
      rownames(x) <- vars[input$vars[1]:input$vars[2]]
      
      
      x <- rbind(clusters()$size,x)
      y <- paste("(Var",paste(input$vars[1]:input$vars[2],")",sep=""),sep="-")
      rownames(x) <- paste(rownames(x),y,sep=": ")
      
      rownames(x)[1] <-"Respondent Count"
      
      round(x,2)
    }
  
    else
      return() 
  })
  
  output$stdscr <- renderTable({
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Score")
    {
      x <- data.frame(t(data.frame(clustersStd()$centers)))
      names(x) <- paste("Cluster",1:input$nclust,sep="-")
      vars <- getHead()
      vars <- vars[vars!=""]
      rownames(x) <- vars[input$vars[1]:input$vars[2]]
      
      
      x <- rbind(clusters()$size,x)
      y <- paste("(Var",paste(input$vars[1]:input$vars[2],")",sep=""),sep="-")
      rownames(x) <- paste(rownames(x),y,sep=": ")
      rownames(x)[1] <-"Respondent Count"
      
      round(x,2)
    }
 
    else if(input$selection == "Scope Chart")
    {
      if(input$tab_select == "PC's for Variables")
        return(pca2()$rotation)
      else
        return(summary(pca2()))
    }
    
    else
      return() 
  })
  sdev <- reactive({
    
    x <- data.frame(data2())
    n <- input$nclust
    x$clust <- clusters()$cluster 
    m <- ncol(x)
    sdData <- data.frame()
    for(i in 1:n)
    {
      temp <- x[x$clust==i,-m]
      sdData <- rbind(sdData,lapply(temp,sd))
      rownames(sdData)[i] <- i
      
    }
    sdt <- data.frame(t(sdData))
    vars <- getHead()
    vars <- vars[vars!=""]
    rownames(sdt) <- vars[input$vars[1]:input$vars[2]]
    sdt
  })

  output$stdev <- renderTable({
    
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Deviation")
    {
      x <- data.frame(sdev())
      x <- rbind(clusters()$size,x)
      names(x) <- paste("Cluster",1:input$nclust,sep="-")
      y <- paste("(Var",paste(input$vars[1]:input$vars[2],")",sep=""),sep="-")
      rownames(x) <- paste(rownames(x),y,sep=": ")
      rownames(x)[1] <-"Respondent Count"
      
      round(x,2)
    }

    else
      return()
  })
  
  
  
  output$export <- downloadHandler(filename = function() { paste(input$op_selection, '.csv', sep='') }, content = function(file) 
  {
    x <- t(as.data.frame(clusters()$centers))
    x <- rbind(clusters()$size,x)
    rownames(x)[1] <-"Respondent Count"
    x <- round(x,2)
    #write.xls(x, file,row.names = T,col.widths = 100)
    write.csv(x,file)
  }
  )
  output$export5 <- downloadHandler(filename = function() { paste(input$op_selection, '.csv', sep='') }, content = function(file) 
  {
    x <- as.data.frame(sdev())
    x <- rbind(clusters()$size,x)
    rownames(x)[1] <-"Respondent Count"
    names(x) <- 1:input$nclust
    x <- round(x,2)
    write.xls(x, file,row.names = T,col.widths = 100)
    write.csv(x,file)
    
  }
  )
  output$export6 <- downloadHandler(filename = function() { paste(input$op_selection, '.csv', sep='') }, content = function(file) 
  {
    x <- t(as.data.frame(clustersStd()$centers))
    x <- rbind(clustersStd()$size,x)
    rownames(x)[1] <-"Respondent Count"
    names(x) <- 1:input$nclust
    x <- round(x,2)
    #write.xls(x, file,row.names = T,col.widths = 100)
    write.csv(x,file)
    
  }
  )
  output$export7 <- downloadHandler(filename = function() { paste(input$op_selection, '.csv', sep='') }, content = function(file) 
  {
    x <- data.frame(bmean())
    vars <- getHead()
    vars <- vars[vars!=""]
    rownames(x) <- vars[input$vars[1]:input$vars[1]]
    
    
    x <- rbind(bcount(),x)
    rownames(x)[1] <-"Respondent Count"
    names(x) <- paste("Cluster",1:input$nclust,sep="-")
    x <- round(x,2)
    #write.xls(x, file,row.names = T,col.widths = 100)
    write.csv(x,file)
    
  }
  )
  output$export8 <- downloadHandler(filename = function() { paste(input$op_selection, '.xls', sep='') }, content = function(file) 
  {
    x <- data.frame(bsdev())
    vars <- getHead()
    vars <- vars[vars!=""]
    rownames(x) <- vars[input$vars[1]:input$vars[2]]
    
    
    x <- rbind(bcount(),x)
    rownames(x)[1] <-"Respondent Count"
    names(x) <- paste("Cluster",1:input$nclust,sep="-")
    x <- round(x,2)
    write.xls(x, file,row.names = T,col.widths = 100)
  }
  )
  output$export9 <- downloadHandler(filename = function() { paste(input$op_selection, '.xls', sep='') }, content = function(file) 
  {
    x <- data.frame(bmeanS())
    vars <- getHead()
    vars <- vars[vars!=""]
    rownames(x) <- vars[input$vars[1]:input$vars[2]]
    
    
    x <- rbind(bcount(),x)
    rownames(x)[1] <-"Respondent Count"
    names(x) <- paste("Cluster",1:input$nclust,sep="-")
    x <- round(x,2)
    write.xls(x, file,row.names = T,col.widths = 100)
  }
  )
  output$export1 <- downloadHandler(filename = function() { paste(input$selection, '.pdf', sep='') }, content = function(file) 
  {	
    #x <- t(as.data.frame(clusters()$centers))
    pdf(file,width=12,height=6)
    print(biplot(pca()),main=input$selection)
    dev.off()
  }
  )
  output$export2 <- downloadHandler(filename = function() { paste(input$selection, '.pdf', sep='') }, content = function(file) 
  {
    #x <- t(as.data.frame(clusters()$centers))
    pdf(file,width=12,height=6)
    print(plot(data2(),
               col = clusters()$cluster,
               pch = 20, cex = 3), main= input$selection)
    dev.off()
  }
  )
  output$export3 <- downloadHandler(filename = function() { paste(input$selection, '.pdf', sep='') }, content = function(file) 
  {
    
    #x <- t(as.data.frame(clusters()$centers))
    pdf(file,width=12,height=6)
    set.seed(10)
    x <- data.frame(pca2()$x)
    y <- data.frame(x[,1],x[,2])
    #x <- data.frame(kmeans(pca2()$rotation))
    p <- autoplot(kmeans(y,input$nclust,iter.max=50), data = y,frame=T)
    print(p)
    dev.off()
  }
  )
  output$export4 <- downloadHandler(filename = function() { paste(input$selection, '.pdf', sep='') }, content = function(file) 
  {
    #x <- t(as.data.frame(clusters()$centers))
    pdf(file,width=12,height=6)
    print(radarchart(data.frame(clusters()$centers),max=F,axistype=3,na.itp=F,axis=8,title=paste(input$selection2,input$selection,sep=" ")))
    dev.off()
  })
  output$export41 <- downloadHandler(filename = function() { paste(input$selection, '.pdf', sep='') }, content = function(file) 
  {
    #x <- t(as.data.frame(clusters()$centers))
    pdf(file,width=12,height=6)
    print(radarchart(data.frame(t(round(sdev(),2))),max=F,axistype=3,na.itp=F,axis=8,title=paste(input$selection2,input$selection,sep=" ")))
    dev.off()
  })
  output$export42 <- downloadHandler(filename = function() { paste(input$selection, '.pdf', sep='') }, content = function(file) 
  {
    #x <- t(as.data.frame(clusters()$centers))
    pdf(file,width=12,height=6)
    print(radarchart(data.frame(round(clustersStd()$centers,2)),max=F,axistype=3,na.itp=F,axis=8,title=paste(input$selection2,input$selection,sep=" ")))
    dev.off()
  })
  
  
  
  output$plot1 <- renderPlot({
    
    
    if(input$selection == "Scope Chart")
    {
      par(mar = c(6.1, 2.1, 0, 1))
      biplot(pca(),cex=0.8)
      abline(h = 0, v = 0, lty = 2, col = 8)
      #mtext("Scope Chart", side = 1, line=-1,outer = TRUE)
    }
    if(input$selection == "Overview")
    {
      #par(mar = c(6.1, 3.1, 0, 1))
      #plotcluster(data2(), round(clusters()$cluster,2),color=TRUE, shade=TRUE,lines=0)
     lab <- paste("Cluster",1:input$nclust,sep="-")
      p<-pie3D(table(clusters()$cluster),labelrad=1.4,labels=lab,
               explode=0.1,main="Cluster Distribution")
      print(p)
      
      
      
      return()
      #clusplot(data2(), clusters$cluster, color=TRUE, shade=TRUE, lines=0)
      #mtext("Cluster Plot", side = 1, line=-1,outer = TRUE)
    }
    
    if(input$selection == "Cluster Plot")
    {
      #par(mar = c(6.1, 3.1, 0, 1))
      #plotcluster(data2(), round(clusters()$cluster,2),color=TRUE, shade=TRUE,lines=0)
      set.seed(10)
      x <- data.frame(pca2()$x)
      y <- data.frame(x[,1],x[,2])
      #x <- data.frame(kmeans(pca2()$rotation))
      p <- autoplot(kmeans(y,input$nclust,iter.max=50), data = y,frame=T)
      print(p)
      return()
      #clusplot(data2(), clusters$cluster, color=TRUE, shade=TRUE, lines=0)
      #mtext("Cluster Plot", side = 1, line=-1,outer = TRUE)
    }
   
    if(input$selection == "Statistical Analysis" && input$op_selection == "Means")
    {
      
      x <- data.frame(round(clusters()$centers,2))
      names(x) <- paste("Var",input$vars[1]:input$vars[2],sep="-")
      par(mar = c(6.1, 2.1, 0, 1))
      radarchart(x,max=F,axistype=3,na.itp=F,axis=8)
      y <- 1:input$nclust
      legend(1.5,1.25,
             legend=y,
             pch=c(15,16),
             col=unique(y),
             lty=c(1,2),seg.len=0.5, title="Cluster")
      #mtext("Mean Radar Chart", side = 1, line=-1,outer = TRUE)
    }
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Deviation")
    {
      x <- data.frame(t(round(sdev(),2)))
      names(x) <- paste("Var",input$vars[1]:input$vars[2],sep="-")
      par(mar = c(6.1, 2.1, 0, 1))
      radarchart(x,max=F,axistype=3,na.itp=F,axis=8)
      y <- 1:input$nclust
      legend(1.5,1,
             legend=y,
             pch=c(15,16),
             col=unique(y),
             lty=c(1,2),seg.len=0.5, title="Cluster")
      #mtext("SD Radar Chart", side = 1, line=-1,outer = TRUE)
    }
    if(input$selection == "Statistical Analysis" && input$op_selection == "Standard Score")
    {
      x <- data.frame(round(clustersStd()$centers,2))
      names(x) <- paste("Var",input$vars[1]:input$vars[2],sep="-")
      par(mar = c(6.1, 2.1, 0, 1))
      radarchart(x,max=F,axistype=3,na.itp=F,axis=8)
      y <- 1:input$nclust
      legend(1.5,1,
             legend=y,
             pch=c(15,16),
             col=unique(y),
             lty=c(1,2),seg.len=0.5, title="Cluster")
      #mtext("SD Radar Chart", side = 1, line=-1,outer = TRUE)
    }
 

    return(NULL)
  })
  
}
)


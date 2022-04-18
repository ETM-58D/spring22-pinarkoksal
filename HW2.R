# if packages do not exist, please install
# install.packages('openxlsx')
# install.packages('ggplot2')
# install.packages('data.table')
# install.packages('skimr')
# install.packages('GGally')
# install.packages('ggcorrplot')
# install.packages(file.choose(), repos=NULL)

  require(openxlsx) #library(openxlsx)
  require(ggplot2)
  require(data.table)
  require(skimr)
  require(GGally)
  require(ggcorrplot)
  
  data_path='C:/Users/pinar/Desktop/HW2_data.xlsx'
  demand=read.xlsx(data_path,sheet='EVDS')
  
  # #correlation analysis
  correl_info=cor(demand)
  
  ggcorrplot(correl_info,
             hc.order = TRUE,
             type = "lower",
             lab = TRUE)

# #numerical statistics
summary_data=skim(demand)
print(summary_data)

#Plotting unemployement rate vs gold amount
plot(x = demand$Quarter.Coin.Amount, y= demand$Unemployement.Rate,
     main="Unemployement Rate vs Gold Amount",ylab="Unemployement Rate",xlab="Gold Amount")
grid()


#plotting interest rate vs gold amount
plot(x = demand$Quarter.Coin.Amount, y= demand$Monthly.Interest.Rate,
     main="Unemployement Rate vs Gold Amount",ylab="Interest Rate",xlab="Gold Amount")

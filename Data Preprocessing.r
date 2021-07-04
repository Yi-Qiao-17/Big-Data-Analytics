#args <- commandArgs(trailingOnly = TRUE)
#infile = args[1]
#outfile = args[2]


library(dplyr)
#input=read.csv(infile,sep = ",",header = T)
input=read.csv("DEMO.csv",sep = ",",header = T)
#input=read.csv("aqx_p_138_2021-02-01.csv",sep = ",",header = T)
input=select(input, SiteId, ItemId,ItemUnit:Concentration)
input=filter(input,input[,2]==3|input[,2]==7|input[,2]==1|input[,2]==33|input[,2]==4)
set=unique(input[,1])#有哪些不同地區
item=c(3,7,1,33,4)#汙染項目代稱
input=unique(input)#去掉不用的項目和無用的itemID
mulpower=c(2,1.88,2.62)#單位轉換倍率
answer=data.frame(SiteID=0,O3=0,NO2=0,SO2=0,PM2.5=0,P10=0,AQI=0)
answer=answer[-1,]
while(length(set)>0){#固定地區
  item_value=c(0,0,0,0,0)
  v=filter(input,input[,1]==set[1])#固定地區
  v=v%>%mutate(date=as.Date(v[,4]))#把日期挑出來
  v=v%>%mutate(time=substring(v[,4],12,13))#把時間挑出來
  v=v[,-4]
  ##############################分汙染項目處理
  k=1
  while(k<6){#v2
    v2=filter(v,v[,2]==item[k])
    v2=v2%>%arrange(time)%>%arrange(date)#按照時間排好
    number_value=as.character(v2[,4])
    number_value[which(number_value=="x")]="-1"
    number_value=as.numeric(number_value)
    noise=which(number_value==-1)#input內容x的位置
    while(length(noise)!=0){#x要算內插法
      num=1
      counter=1
      while(number_value[noise[1]+counter]=="-1"){
        num=num+1
        counter=counter+1
      }#算有幾個x是連在一起的
      a1=number_value[noise[1]-1]
      an=number_value[noise[1]+num]
      r=(an-a1)/(num+1)
      for(i in 1:num){
        temp=noise[1]+i-1
        number_value[temp]=a1+i*r
      }
      noise=which(number_value==-1)#input內容x的位置
    }#處理內插
    
    number_value=number_value[1:24]
    
    #算AQI
    ############################處理單位
    #mulpower=as.numeric(as.character(mulpower))
    for(i in 1:3){
      if(v2[,2][1]==item[i]){
        number_value=number_value*mulpower[i]
      }
    }
    ##################################
    for(i in 1:5){
      if(v2[,2][1]==item[i]){
        mark=i
      }
    }#哪一個item
    if(mark==1){##############
      sum_ar=c(1:17)
      for(i in 1:17){
        sum_ar[i]=sum(number_value[i:(i+7)])/8
      }
      value=max(sum_ar)
     # print(value)
      if(value>=241){
        item_value[mark]=10
      }else if(value>=214){
        item_value[mark]=9
      }else if(value>=188){
        item_value[mark]=8
      }else if(value>=161){
        item_value[mark]=7
      }else if(value>=141){
        item_value[mark]=6
      }else if(value>=121){
        item_value[mark]=5
      }else if(value>=101){
        item_value[mark]=4
      }else if(value>=67){
        item_value[mark]=3
      }else if(value>=34){
        item_value[mark]=2
      }else if(value>=0){
        item_value[mark]=1
      }
      
    }else if(mark==2){#############
      value=max(number_value)
      #print(value)
      if(value>=601){
        item_value[mark]=10
      }else if(value>=535){
        item_value[mark]=9
      }else if(value>=468){
        item_value[mark]=8
      }else if(value>=401){
        item_value[mark]=7
      }else if(value>=335){
        item_value[mark]=6
      }else if(value>=268){
        item_value[mark]=5
      }else if(value>=201){
        item_value[mark]=4
      }else if(value>=135){
        item_value[mark]=3
      }else if(value>=68){
        item_value[mark]=2
      }else if(value>=0){
        item_value[mark]=1
      }
      
    }else if(mark==3){#############
      value=max(number_value)
      #print(value)
      if(value>=1065){
        item_value[mark]=10
      }else if(value>=888){
        item_value[mark]=9
      }else if(value>=711){
        item_value[mark]=8
      }else if(value>=533){
        item_value[mark]=7
      }else if(value>=444){
        item_value[mark]=6
      }else if(value>=355){
        item_value[mark]=5
      }else if(value>=267){
        item_value[mark]=4
      }else if(value>=178){
        item_value[mark]=3
      }else if(value>=89){
        item_value[mark]=2
      }else if(value>=0){
        item_value[mark]=1
      }
      
    }else if(mark==4) {###############################
      value=sum(number_value)/24
      #print(value)
      if(value>=71){
        item_value[mark]=10
      }else if(value>=65){
        item_value[mark]=9
      }else if(value>=59){
        item_value[mark]=8
      }else if(value>=54){
        item_value[mark]=7
      }else if(value>=48){
        item_value[mark]=6
      }else if(value>=42){
        item_value[mark]=5
      }else if(value>=36){
        item_value[mark]=4
      }else if(value>=24){
        item_value[mark]=3
      }else if(value>=12){
        item_value[mark]=2
      }else if(value>=0){
        item_value[mark]=1
      }
      
    }else if(mark==5){#############
      value=sum(number_value)/24
     # print(value)
      if(value>=101){
        item_value[mark]=10
      }else if(value>=92){
        item_value[mark]=9
      }else if(value>=84){
        item_value[mark]=8
      }else if(value>=76){
        item_value[mark]=7
      }else if(value>=67){
        item_value[mark]=6
      }else if(value>=59){
        item_value[mark]=5
      }else if(value>=51){
        item_value[mark]=4
      }else if(value>=34){
        item_value[mark]=3
      }else if(value>=17){
        item_value[mark]=2
      }else if(value>=0){
        item_value[mark]=1
      }
      
    }
    k=k+1
  }
  #####################處理每個地區放入一個dataframe
  e=data.frame(SiteID=set[1],O3=item_value[1],NO2=item_value[2],SO2=item_value[3],PM2.5=item_value[4],P10=item_value[5],AQI=max(item_value))
  answer=rbind(answer,e)
  #####################
  set=set[-1]
}

#write.table(answer, file=outfile, row.names=F,col.names=F, sep=",")
write.table(answer, file="outfile.csv", row.names=F,col.names=T, sep=",")






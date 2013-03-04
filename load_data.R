download.file(url = "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda", 
              destfile="/tmp/samsungData.rda", method = "curl")

load("/tmp/samsungData.rda")

summary(samsungData)
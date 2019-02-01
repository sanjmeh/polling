# these are functions used for polls
library(jsonlite)
library(data.table)
library(magrittr)

males <- c("John", "William", "James", "Charles", "George", "Frank", "Joseph", 
           "Thomas", "Henry", "Robert", "Edward", "Harry", "Walter", "Arthur", 
           "Fred", "Albert", "Samuel", "David", "Louis", "Joe", "Charlie", 
           "Clarence", "Richard", "Andrew", "Daniel", "Ernest", "Will", 
           "Jesse", "Oscar", "Lewis", "Peter", "Benjamin", "Frederick", 
           "Willie", "Alfred", "Sam", "Roy", "Herbert", "Jacob", "Tom", 
           "Elmer", "Carl", "Lee", "Howard", "Martin", "Michael", "Bert", 
           "Herman", "Jim", "Francis", "Harvey", "Earl", "Eugene", "Ralph", 
           "Ed", "Claude", "Edwin", "Ben", "Charley", "Paul", "Edgar", "Isaac", 
           "Otto", "Luther", "Lawrence", "Ira", "Patrick", "Guy", "Oliver", 
           "Theodore", "Hugh", "Clyde", "Alexander", "August", "Floyd", 
           "Homer", "Jack", "Leonard", "Horace", "Marion", "Philip", "Allen", 
           "Archie", "Stephen", "Chester", "Willis", "Raymond", "Rufus", 
           "Warren", "Jessie", "Milton", "Alex", "Leo", "Julius", "Ray", 
           "Sidney", "Bernard", "Dan", "Jerry", "Calvin")
females <- c("Mary", "Anna", "Emma", "Elizabeth", "Minnie", "Margaret", 
             "Ida", "Alice", "Bertha", "Sarah", "Annie", "Clara", "Ella", 
             "Florence", "Cora", "Martha", "Laura", "Nellie", "Grace", "Carrie", 
             "Maude", "Mabel", "Bessie", "Jennie", "Gertrude", "Julia", "Hattie", 
             "Edith", "Mattie", "Rose", "Catherine", "Lillian", "Ada", "Lillie", 
             "Helen", "Jessie", "Louise", "Ethel", "Lula", "Myrtle", "Eva", 
             "Frances", "Lena", "Lucy", "Edna", "Maggie", "Pearl", "Daisy", 
             "Fannie", "Josephine", "Dora", "Rosa", "Katherine", "Agnes", 
             "Marie", "Nora", "May", "Mamie", "Blanche", "Stella", "Ellen", 
             "Nancy", "Effie", "Sallie", "Nettie", "Della", "Lizzie", "Flora", 
             "Susie", "Maud", "Mae", "Etta", "Harriet", "Sadie", "Caroline", 
             "Katie", "Lydia", "Elsie", "Kate", "Susan", "Mollie", "Alma", 
             "Addie", "Georgia", "Eliza", "Lulu", "Nannie", "Lottie", "Amanda", 
             "Belle", "Charlotte", "Rebecca", "Ruth", "Viola", "Olive", "Amelia", 
             "Hannah", "Jane", "Virginia", "Emily")

picknames<- function(audsize=20,m2f=0.8){
  m <- sample(males,size = audsize*m2f)
  f <- sample(females,size = audsize*(1-m2f))
  c(f,m)
}

crfile <- function(names,file="www/fnames.csv"){
  x1 <- data.table(names=names,assigned=F)
  fwrite(x1,file)
}

markused <- function(name,file="www/fnames.csv"){
  x1 <- fread(file)
  x1$times %<>%  ymd_hms
  x1[name==names,assigned:=T]
  x1[name==names,times:=Sys.time()]
  fwrite(x1,file)
}

unmark <- function(nam="ALL",file="www/fnames.csv"){
  x1 <- fread(file)
  if(nam=="ALL")  x1$assigned<- F else
    x1[names==nam,assigned:=F]
    x1[names==nam,times:=Sys.time()]
  fwrite(x1,file)
}

getname <- function(file="www/fnames.csv"){
  x <- fread(file)
  rname <- x[assigned==F][order(times)][1,names]
  if(is.na(rname)) {
    message("WARNING: Names exhausted..recycling oldest used")
    rname <- x[order(times)][1,names]
  }
  markused(rname,file)
  rname
}

isactive <- function(code, file="www/fnames.csv"){
  x <- fread(file)
  if(nrow(x[tolower(code)==tolower(names) & assigned==T])==1) T else F
}

logevent <- function(eventcode,user,param,conf=123,file="www/log.csv"){
  fwrite(data.table(timestamp=Sys.time(),confcode=conf,user=user,event=eventcode,parameters=param),file,append = T,sep = "|",quote = F)
}

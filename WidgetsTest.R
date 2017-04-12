cbg[1]<-Levandeskog
cbg[2]<-Dödved
cbg[3]<-Lövrikskog
cbg[4]<-Gammalskog
cbg[5]<-Ödlingslandskap
cbg[6]<-Ängsbetesmarker
cbg[7]<-Småbiotoper
cbg[8]<-Kalfjäll
cbg[9]<-Fjällskog
cbg[10]<-LevandeSjöar
cbg[11]<-MyllrandevåtmarkerS
cbg[12]<-MyllrandevåtmarkerN

Levandeskog          <-c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåig hackspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre")
Dödved               <-c("Gröngöling","Mindre hackspett","Tretåig hackspett","Entita","Talltita")
Lövrikskog           <-c("Skogsduva","Gröngöling","Mindre hackspett","Tretåig hackspett","Stjärtmes","Entita","Trädkrypare")
Gammalskog           <-c("Tjäder","Tretåig hackspett","Lavskrika","Svartmes","Tofsmes","Lappmes","Talltita","Trädkrypare","Domherre")
Odlingslandskap      <-c("Tofsvipa","Storspov","Sånglärka","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Pilfink")
Ängsbetesmarker      <-c("Tofsvipa","Storspov","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv")
Småbiotoper          <-c("Stenskvätta","Buskskvätta","Törnsångare","Törnskata","Stare","Hämpling","Gulsparv")
#Kalfjäll             <-c("Fjällripa","Ljungpipare","Fjällabb","Stenskvätta","Ängspiplärka","Lappsparv","Snösparv")
#Fjällskog            <-c("Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Svartvit flugsnappare","Gråsiska","Bergfink")
#LevandeSjöar         <-c("Storlom","Smålom","Skäggdopping","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Sothöna","Drillsnäppa","Fisktärna","Silvertärna","Forsärla")
#MyllrandevåtmarkerS  <-c("Rördrom","Kricka","Sångsvan","Brun kärrhök","Trana","Sothöna","Enkelbeckasin","Grönbena")
#MyllrandevåtmarkerN  <-c("Smålom","Kricka","Bläsand","Sångsvan","Trana","Ljungpipare","Enkelbeckasin","Småspov","Grönbena","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad sim")


Indikator<-list(Levandeskog,Dödved,Lövrikskog,Gammalskog,Odlingslandskap,Ängsbetesmarker,Småbiotoper,Kalfjäll,Fjällskog,LevandeSjöar,MyllrandevåtmarkerS,MyllrandevåtmarkerN)

Indikator<-list(
  Levandeskog          =c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåig hackspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre"),
  Dödved               =c("Gröngöling","Mindre hackspett","Tretåig hackspett","Entita","Talltita"),
  Lövrikskog           =c("Skogsduva","Gröngöling","Mindre hackspett","Tretåig hackspett","Stjärtmes","Entita","Trädkrypare"),
  Gammalskog           =c("Tjäder","Tretåig hackspett","Lavskrika","Svartmes","Tofsmes","Lappmes","Talltita","Trädkrypare","Domherre"),
  Odlingslandskap      =c("Tofsvipa","Storspov","Sånglärka","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Pilfink"),
  Ängsbetesmarker      =c("Tofsvipa","Storspov","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv"),
  Småbiotoper          =c("Stenskvätta","Buskskvätta","Törnsångare","Törnskata","Stare","Hämpling","Gulsparv"),
  Kalfjäll             =c("Fjällripa","Ljungpipare","Fjällabb","Stenskvätta","Ängspiplärka","Lappsparv","Snösparv"),
  Fjällskog            =c("Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Svartvit flugsnappare","Gråsiska","Bergfink"),
  LevandeSjöar         =c("Storlom","Smålom","Skäggdopping","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Sothöna","Drillsnäppa","Fisktärna","Silvertärna","Forsärla"),
  MyllrandevåtmarkerS  =c("Rördrom","Kricka","Sångsvan","Brun kärrhök","Trana","Sothöna","Enkelbeckasin","Grönbena"),
  MyllrandevåtmarkerN  =c("Smålom","Kricka","Bläsand","Sångsvan","Trana","Ljungpipare","Enkelbeckasin","Småspov","Grönbena","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad sim")
)


Indikator_namn<-list(
  "Levandeskog",
  "Dödved",
  "Lövrikskog",
  "Gammalskog",
  "Odlingslandskap",
  "Ängsbetesmarker",
  "Småbiotoper"
  #  "Kalfjäll",
  #  "Fjällskog",
  #  "LevandeSjöar",
  #  "MyllrandevåtmarkerS",
  #  "MyllrandevåtmarkerN"
)

flavors <- c("vanilla", "chocolate", "strawberry")

f <- function(h,...) print(
  paste("Yum",
        paste(svalue(h$obj),collapse=" and "),
        sep = " "))

w <- gwindow("checkbox example")
gp <- ggroup(container=w)
glabel("Favorite flavors:",container=gp)
cbg <- gcheckboxgroup(flavors, container=gp, handler=f)

svalue(cbg) <- c(TRUE, FALSE, TRUE)
svalue(cbg)
svalue(cbg) <- "vanilla"
svalue(cbg, index=TRUE) <- 1:2
cbg[3] <- "raspberry"

## use a table to display (toolkit specific) so that scrollars can be used
cbg <- gcheckboxgroup(letters, container=gwindow(), use.table=TRUE)



confirmDialog <- function(message, handler=NULL) {
window <- gwindow("Confirm")
group <- ggroup(container = window)
gimage("info", dirname="stock", size="dialog", container=group)

## A group for the message and buttons
inner.group <- ggroup(horizontal=FALSE, container = group)
glabel(message, container=inner.group, expand=TRUE)

## A group to organize the buttons
button.group <- ggroup(container = inner.group)
## Push buttons to right
addSpring(button.group)
gbutton("ok", handler=handler, container=button.group)
gbutton("cancel", handler = function(h,...) dispose(window),
container=button.group)

return()
}

if(interactive()) {
  w <- gwindow("Selection widgets")
  g <- gvbox(cont=w)
  
  fl <- gformlayout(cont=g)
  gcheckbox("checkbox", checked=TRUE, cont=fl, label="checkbox")
  gradio(state.name[1:4], selected=2, horizontal=TRUE, cont=fl, label="gradio")
  gcheckboxgroup(state.name[1:4], horizontal=FALSE, cont=fl, label="checkbox group")
  
  bg <- ggroup(cont=g)
  gbutton("ok", cont=bg, handler=function(h,...) print(sapply(fl$children, svalue)))
  

  
  
}

## button group example
w <- gwindow("Button examples")
g <- ggroup(container = w)
addSpring(g)   ## push to right of widget
gbutton("help", container = g)
addSpace(g, 20) ## some breathing room
gbutton("cancel", container = g)
gbutton("ok", container = g, handler = function(h, ...) cat("do it\n"))


flavors<-c("vanilla", "chocolate", "strawberry") 

w <- gwindow("checkbox example") 
gp <- ggroup(container=w) 
glabel("Favorite flavors:",cont=gp) 
cbg <- gcheckboxgroup(flavors, container=gp, handler=f)

# Here is the trick 
addHandlerClicked(cbg,handler=function(h,...){ 
  My_Flav<-svalue(cbg) 
  print(My_Flav) 
}) 

w <- gwindow(visible=FALSE) 
g <- ggroup(cont=w, horizontal=FALSE) 
tbl <- gtable(mtcars[1:5,], cont=g, multiple=TRUE, expand=TRUE) 
b <- gbutton("click", cont=g, handler=function(h,...) { 
  print(svalue(tbl)) 
}) 
visible(w) <- TRUE 


svalue(tbl, index=TRUE) <- 1:2 
print(svalue(tbl, index=TRUE)) 


group = ggroup(horizontal=FALSE, container=gwindow("two widgets"))
button = gbutton("click me", container=group)
label = glabel("Button has not been clicked", container=group)
addhandlerclicked(button, handler = function(h,...) {
  svalue(h$obj) <-"click me again"
  svalue(h$action) <- "Button has been clicked"
}, action = label)


tkwait.window



w <- gwindow("Two widgets")
g <- ggroup(container = w)
widget1 <- gbutton("Click me to update the counter", container=g,
                     handler = function(h,...) {
                       oldVal <- svalue(widget2)
                       svalue(widget2) <- as.numeric(oldVal) + 1
                       })
widget2 <- glabel(0, container=g)

list.condition <- sapply(input.list, function(x) class(x)=="desired.class")
output.list  <- input.list[list.condition]

a<-c("aap","paard","vogel")
b<-c("aap","slang","vogel","neushoorn")
c<-c("slang","paard")

Indicator<-list(a,b,c)
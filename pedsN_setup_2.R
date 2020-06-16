
# setwd('/Volumes/Samsung_T3/Data/EHR_data/PedsNet/'); getwd()
# list.files(getwd())
# d <- fread('cleaned_with_growthcleanr.csv') #### from Lucy ---------
# fwrite(d, 'cleaned_with_growthcleanr.csv.gz')
# 
# d <- fread('cleaned_with_growthcleanr.csv.gz')
# d[,':=' (id=as.integer(subjid),param=mapvalues(param,Cs(WEIGHTKG,HEIGHTCM),Cs(W,H)))]
# d[,Cs(V1):=NULL]
# d
# fxn(d)
# sn(d)
# d[param=='W',.N,code]
# d[param=='H',.N,code]
# fstwrite(d,'cleaned_with_growthcleanr.fst', compress=75)


# up to 427 -----------


fdel <- function(x,d){
  x=x[x %nin% fnames(x,d)];
  if (length(x)>0) d[,(x):=NULL]; 
}
# x=Cs(rseq,maxrle,g);
# fdel(w1,x)

# I #think# that is is the file Lucy sent me 47.3M for 3.5M kids
setwd('/Volumes/Samsung_T3/Data/EHR_data/PedsNet/'); getwd(); list.files()
st(d <- fstread('cleaned_with_growthcleanr.fst', as=T)); sn(d); dim(d) # 6 s
      # st(feawrite(d,'cleaned_with_growthcleanr.fea', compression='lz4')); # 5 s
      # st(d <- fearead('cleaned_with_growthcleanr.fea')); setDT(d); nr(d) # 5 s

setorder(d,id,agedays,param)
# d <- d[1:2000000];
d; fxn(d)
dorig <- copy(d)
# 47324246  3501151  // this file came from Lucy
d[,Cs(subjid):=NULL]; class1(d)

range(d$agedays/365.25)
d[,.(mean(agedays/365.25),mean(sex)),keyby=cat]
setnames(d,Cs(measurement),Cs(value))
d[,.N,keyby=.(cat)]; sa(d,40)

d[,':='  (age=agedays/365.25, code=factor(as.character(code)))] 
dorig[param=='W' & id==150503,.(id,agedays,param,measurement)]
st(d <- fseq(d)) 
st(d <- fxage(d))
st(x <- srm(d[,.(age,seq,maxseq)])); x # someone examined >100 000 times? id= 24 459 408
set_cols_first(d, Cs(id))
d[maxseq>100000,]
d[id==244594088 & param=='W',.N,agedays]
d[id==244594088 & param=='W',.N,.(param,code)]
d[maxseq>185200 & param=='W']
d[maxseq>185200 & param=='W' & agedays==803, s(value)]

d[,.N,param]
options(digits=10)
st(x <- dup_rows(d[param=='W'], by=Cs(id,agedays))[,.(id,agedays,param,value)][1:30]);
dcast(x, id~agedays, value.var='value')
nr(x);
options(digits=5)
# d[id==688795 & param=='H',.(id,agedays,age,value,code)]
# d[id==445088 & param=='W',.(id,agedays,age,value,code)] # maybe unit problem
# d[param == 'H' & id %in% c(2780626,10315,4106097,4682396)][order(id,agedays)]

dx <- d[param=='H']; nr(dx); some(dx)
dx[id %in% c(10968,3886324,260875)]
dx[,.N,.(param,code)]
x <- dup_rows(dx,Cs(id,agedays)); x
x <- x[,':=' (seq=seq_len(.N),maxseq=.N),by=.(id,param,agedays)]; x[,.N,keyby=maxseq]
x[id==824]
x <- x[maxseq>=4]; x; x[,.N,code]
set.seed(1234)
x[id %in% sample(x$id,4),.(id,agedays,ht=value,code)]
x[id %in% unique(x$id)[1:3]][1:50] %>% roundc(3)
x[id==824] %>% roundc(2)
# d[id==1035451 & param=='H'][order(age)]

d[param=='H',.N,code][order(-N)][,perc:=round(100*N/(nr(d)/2),3)][]
d[param=='W',.N,code][order(-N)][,perc:=round(100*N/(nr(d)/2),3)][]

d[,asmml(age),keyby=cat]
d[,Cs(cat):=NULL]
setorder(d,id,agedays,param); 

d[code %like% 'Swap',] %>% roundc(1)
# swap in hutils x %<->% value 
# require(hutils)
d[code %like% 'Swap',.(id,agedays,param,value)];
x1 <- d[param=='W' & code %flike% 'Swap'][,':=' (param='H', code='Include-S')]; nr(x1)
  nr(x1); x1[]; 
x2 <- d[param=='H' & code %flike% 'Swap'][,':=' (param='W', code='Include-S')]; nr(x2); x2[]
  nr(x2); x2[]; 
x <- rbind(x1,x2); x; nr(x);
d <- d[!x, on=Cs(id,agedays)]; nr(d)
d <- rbindlist(list(d,x)); nr(d)
d[,.N,code]; sn(d)
d[code %like% 'Include-S',.(id,agedays/365,code,param,value)][order(id)]


v=Cs(id,agedays,param,value,code); 
setorder(d,agedays,param,value)
w1 <- d[param == 'W' & code %like% 'Dup']; d[param=='W' & id %in% sample(w1$id,3),..v][1:30]
h1 <- d[param == 'H' & code %flike% 'Dup']; d[param=='H' & id %in% sample(h1$id,3),..v][1:30]

w1 <- d[param %flike% 'W' & value>0]; setnames(w1,Cs(value),Cs(wt))
w1[code %flike% 'Unit-Error-High', ':='(wt=wt/2.2046, code='Include-UH')]
w1[code %flike% 'Unit-Error-Low', ':='(wt=wt*2.2046, code='Include-UL')]
w1[code %flike% 'Inc',.N,code]

h1 <- d[param %flike% 'H' & value>0]; setnames(h1,Cs(value),Cs(ht))
h1[code %flike% 'High', ':='(ht=ht/2.54, code='Include-UH')]
h1[code %flike% 'Low', ':='(ht=ht*2.54, code='Include-UL')]
h1[code %flike% 'Inc',.N,code]; 

v=Cs(param,seq,maxseq); fdel(v,w1); sn(w1)
sn(h1); fdel(v,h1); sn(w1)

# need to know if a CF followed a good or bad value
# only keep cf heights if they follow a 'good' value
# check previous code - make it good (include) or bad (exclude)
.i <- c(254987,1565768,1988149)
d[id %in% .i & param=='H',.(id,agedays,param,value,code)]
xx <- h1[id %in% .i,.(id,agedays,ht,code)][order(agedays)]; xx

setorder(h1,id,ht,agedays); nr(h1); levels(h1$code)
x <- h1[code %flike% 'Exc',id]; x
x2 <- h1[id %in% sample(x,3),.(id,agedays,age,ht,code)]; x2
h1[id %in% x2$id,.(id,agedays,age,ht,code)][1:50]; 
h1[code %flike% 'Carr',code:='Carried-Forward']; 
h1[code %flike% 'Inc',bad:=0]
badg <- grepv('Exclude-[EMPST]',unique(h1$code)); unique(badg) 
# note the change from 'type' to 'bad' -- Excluded the Duplcates from 'bad'
h1[code %in% badg,bad:=1]
h1[,.N,keyby=.(bad,code)]
h1[id %in% x2$id,.(id,agedays,age,ht,code,bad)][1:40]
h1[id %in% .i,.(id,agedays,ht,code,bad)][order(agedays)]

h1$cfh <- 0
h1[code %flike% 'Carr', cfh:=1 ]
h1[,.N,.(cfh,bad)]
h1[cfh==0 & bad==1][1:40]
h1[cfh==1 & is.na(bad)]
x <- h1[code %flike% 'Car',id]; h1[id %in% sample(x,3)]  %>% roundc(2)
h1[id %in% sample(x,3),.(id,agedays,age,ht,code,bad)][order(id,age)][1:50]
h1[id %in% .i,.(id,agedays,ht,code,bad)][order(agedays)]

h1[,.N,bad]; 
h1$bad[1:25]
h1[1:25,.(id,age,ht,code,bad)][order(id,age)]
setnafill(h1,'locf', cols=Cs(bad))
h1[1:30,.(id,age,agedays,ht,code,bad)][order(id,age)]
h1[id %in% .i,.(id,age,agedays,ht,code,bad)][order(id,age)]
h1$bad[1:25]
x <- h1[code %ilike% 'carr' & bad==1]; h1[id %in% sample(unique(x$id),3)][1:45]
h1[,.N,keyby=.(cfh,bad)]
h1[code %flike% 'Carried',.N,keyby=.(bad)]; 
h1[id %in% .i][order(agedays)]
d[id %in% .i & param=='H',.(id,agedays,param,value,code)][order(agedays)]

xid <- h1[code %flike% 'Carried' & bad==1]$id; len(xid) # 134 K
h1[id %in% sample(xid,3),.(id,age,ht,code,bad)][order(id,age)][1:60]
h1[,.N,keyby=.(bad,code)]
nr(h1); h2 <- h1[!(bad==1 & code %flike% 'Carr')]; nr(h1)-nr(h2) # 134 K
h1 <- h2
h1[id %in% .i,.(id,agedays,age,ht,code,bad)][order(id,agedays)]

# now do the same thing for CF weigwts - see if the follow a good/bad measurement
.i <- c(3006394,448100,1988149)
xx <- w1[id %in% .i,.(id,agedays,wt,code)][order(id,agedays)]; xx

setorder(w1,id,wt,agedays); nr(w1)
x <- w1[code %flike% 'Exc',id]; x
x2 <- w1[id %in% sample(x,3),.(id,agedays,age,wt,code)]; x2
w1[id %in% x2$id,.(id,agedays,age,wt,code)][1:50]; 
w1[code %flike% 'Carr',code:='Carried-Forward']; 
w1[code %flike% 'Inc',bad:=0]
badg <- grepv('Exclude-[EMPST]',unique(w1$code)); unique(badg) 
w1[code %in% badg,bad:=1]
w1[,.N,keyby=.(bad,code)]
w1[id %in% x2$id,.(id,agedays,age,wt,code,bad)][1:40]
w1[id %in% .i,.(id,agedays,wt,code,bad)][order(agedays)]

w1$cfw <- 0
w1[code %flike% 'Carr', cfw:=1 ]
w1[,.N,.(cfw,bad)]
w1[cfw==0 & bad==1][1:40]
w1[cfw==1 & is.na(bad)]
x <- w1[code %flike% 'Car',id]; w1[id %in% sample(x,3)]  %>% roundc(2)
w1[id %in% sample(x,3),.(id,agedays,age,wt,code,bad)][order(id,age)][1:50]
w1[id %in% .i,.(id,agedays,wt,code,bad)][order(agedays)]

w1[,.N,bad]; 
w1$bad[1:45]
w1[1:45,.(id,agedays,wt,code,bad)][order(id,agedays)]
setnafill(w1,'locf', cols=Cs(bad))
w1[1:45,.(id,age,agedays,wt,code,bad)][order(id,age)]
w1[id %in% .i,.(id,age,agedays,wt,code,bad)][order(id,age)]
w1$bad[1:25]
x <- w1[code %ilike% 'carr' & bad==1];
w1[id %in% sample(unique(x$id),3)][1:35]
w1[,.N,keyby=.(cfw,bad)]
w1[code %flike% 'Carried',.N,keyby=.(bad)]; 
w1[id %in% .i][order(id,agedays)]
d[id %in% .i & param=='H',.(id,agedays,param,value,code)][order(agedays)]

xid <- w1[code %flike% 'Carried' & bad==1]$id; len(xid) # 8000
w1[id %in% sample(unique(xid),3),.(id,agedays,wt,code,bad)][order(id,agedays)]
w1[,.N,keyby=.(bad,code)]
nr(w1); w2 <- w1[!(bad==1 & code %flike% 'Carr')]; nr(w1)-nr(w2) 
w1 <- w2
w1[id %in% .i,.(id,agedays,age,wt,code,bad)][order(id,agedays)]

# now see if a CF among old kids is the same ht as the final ht
sn(h1); 
v=Cs(fin.age,rseq); fdel(v,h1); h1
x <- h1[code %ilike% 'inc' | code %ilike% 'car'][,.(fin.age=max(age)),id]; x
# h1[!x, on='id'][1:70] # these are all full of bads!!!!
nr(h1); h1 <- h1[x,on='id', nomatch=0]; sn(h1); nr(h1)
h1[id %in% sample(id,2)][1:30]
h1[id %in% .i]
h1[age==fin.age,fin.ht:=ht] # get fin.ht to see if CF run is at the end
setnafill(h1,'nocb', cols=Cs(fin.ht)); h1$fin.age <- NULL
h1[id %in% .i][order(id,age)]

xxx <- h1
h1 <- copy(xxx) # can restart from here ---------------------
i <- 290689

frle <- function(.d=h1, var){ # see also: get(var)
   setorder(.d,id,age);
    v=Cs(bad,g, rseq, nrle, maxrle, maxrle.x, maxrle.y)
    fdel(v,.d)
   .d[id %in% i];
   .d[,rseq:=1]
   st(.d[,rseq:=seq_len(.N),by=.(id,rleid(.d[[var]]))]); .d[id %in% i] #
   # st(.d[,rseq:=seq_len(.N),by=.(id,rleid(get(var)))]); .d[id %in% i] # note 'var' (which is ht)
   st(.d[, g:=cumsum(rseq==1), by='id']); .d[id %in% i]
   st(x <- .d[,.(maxrle=.N),by=.(id,g)]); x[id %in% i]
   .d <- merge(.d,x,by=Cs(id,g))
}
st(h1 <- frle(h1, 'ht')); # 25 for entire function
st(w1 <- frle(w1, 'wt'));
setcolorder(h1,Cs(id,sex,agedays,age,ht,cfh,code,g,rseq,maxrle)); h1
h1[,.N,keyby=maxrle]

# alternative is to do with setnames before and after
# also can use rleid(get(var))

h1[id %in% i][1:50]
x <- h1[between(maxrle,3,5), id]; x;1:50
h1[id %in% sample(x,3), .(id,agedays,age,ht,code,g,rseq,maxrle)]
h1[,.N,keyby=maxrle]
h1[maxrle>2500][1:100]

# get starting and ending ages for height runs
setorder(h1,id,age,rseq)
h1[rseq==1, sage:=age] # starting age
h1[rseq==maxrle, eage:=age]; h1[1:30] # ending age
setnafill(h1, 'nocb', cols='eage'); # copy them forwards and backwards
setnafill(h1, 'locf', cols='sage'); h1[30:40]
h1[,dur:=0]

# question: lucy, do we want to define duration for each child or for each group??
# h1[code %ilike% 'Carr',dur:=age-sage] # duration since beinning of run to each record
h1[code %ilike% 'Carr',dur:=eage-sage] # duration for a CF group
h1[,.N,.(dur>0)]; s(h1$dur)
qu(h1[dur>0,.(dur)]) # dur is >0 ONLY for CF records
.b=c(0,0.0001,0.25,0.5,1,2.5,5,16)
h1[,asmml(dur), keyby=cut(dur,.b,right=F)]
h1[id %in% sample(x,3),.(id,age,ht,sage,eage,dur,rseq,maxrle)][1:50] %>% roundc(2)
h1[id %in% sample(x,3),.(id,age,ht,sage,eage,dur,code)][1:50] %>% roundc(2)

h1[,.N,keyby=cut(dur,.b, right=F)][,perc:=N*100/nr(h1)] %>% roundc(2)

v=Cs(rseq,maxrle,g)
fdel(v,h1)


# define old kids - this has to be based on sex,starting age, AND
# whether the CF heights are >= to the height at the last exam
# the 2nd part (ht>=fin.ht) is new
h1[,old:=0L]
# add to definition of old that ht >=fin.ht
h1[(sex==0 & sage>17 & ht>=fin.ht) | (sex==1 & sage>16 & ht>=fin.ht), old:=1L]; 
h1[,.N,.(old,cfh)]

h1[old==1 & cfh==1, cfh:=2L]; h1[,.N,cfh] # redefine cf for older kids
h1[,asmml(sage),keyby=.(sex,cfh)] %>% round(2)
h1[cfh==1,.(id,sex,age,ht,code,cfh,sage,eage)][1:20] %>% roundc(2)
h1[cfh==2,.(id,sex,age,ht,cfh,sage,eage)][1:20]

# get rid of height CFs
h1[,.N,.(cfh,code)]
h1[id %in% .i] %>% roundc(3)
nr(h1); x <- h1[!(code %like% 'Carr' & dur > 0.25 & cfh==1)]; 
x[,.N,keyby=.(old,cfh,code)]
# cfh was recoded to 2 for old kid, about 20 lines above
nr(h1)-nr(x) # 212K deleted 
h1[,.N,cfh]; x[,.N,cfh] # still keep 2.06 M CF
s(x$dur)
h1 <- x 
xx <- h1[cfh==2 & old==1 & dur>1]; xx
h1[id %in% sample(xx$id,2)]

anyDuplicated(h1, by=Cs(id,agedays))
setorder(h1,id,agedays,cfh,-code)
x <- dup_rows(h1[5:1e5], by=Cs(id,agedays)); levels(x$code)
x[1:30,.(id,agedays,ht,cfh,code)] %>% roundc(2)
# maybe the duplicates on same day can also be CFs

anyDuplicated(w1, by=Cs(id,agedays))
setorder(w1,id,agedays,cfw,-code)
x <- dup_rows(w1[5:1e5], by=Cs(id,agedays)); levels(x$code)
x[1:30,.(id,agedays,wt,cfw,code)] %>% roundc(2)


# NOW, work with weight
xxx <- w1
w1 <- copy(xxx) 
i <- 290689
setnames(w1,Cs(wt,cfw),Cs(var,cf)); w1[id %in% i]
# st(w1 <- frle(w1)); # 25 for entire function - set up initially for HT
setnames(w1,Cs(var,cf),Cs(wt,cfw)); w1

w1[id %in% i]
x <- w1[between(maxrle,3,5), id]; x[1:50]
w1[id %in% sample(x,3), .(id,agedays,wt,code,g,rseq,maxrle)][1:60]
w1[,.N,keyby=maxrle]
w1[maxrle>20][1:100]

# get starting and ending ages for runs
setorder(w1,id,age,rseq)
w1[rseq==1, sage:=age] # starting age
w1[rseq==maxrle, eage:=age]; w1[1:30] # ending age
setnafill(w1, 'nocb', cols='eage'); # copy them forwards and backwards
setnafill(w1, 'locf', cols='sage'); w1[30:40] %>% roundc(2)
w1[,dur:=0]

w1[code %ilike% 'Carr',dur:=eage-sage] # duration for a CF group
w1[,.N,.(dur>0)]; s(w1$dur)
qu(w1[dur>0,.(dur)]) # dur is >0 ONLY for CF records
.b=c(0,0.0001,0.25,0.5,1,2.5,5,16)
w1[,asmml(dur), keyby=cut(dur,.b,right=F)]
# w1[,asmml(dur), keyby=findInterval(dur,.b,all=T)]
w1[id %in% sample(x,3),.(id,age,wt,sage,eage,dur,rseq,maxrle)][1:50] %>% roundc(2)
w1[id %in% sample(x,3),.(id,age,wt,sage,eage,dur,code)][1:50] %>% roundc(2)

w1[,.N,keyby=cut(dur,.b, right=F)][,perc:=N*100/nr(w1)] %>% roundc(2)
v=Cs(rseq,maxrle,g); fdel(v,w1)

# now do the weight CF exclusions (this has already been done for height)
nr(w1); w2 <- w1[!(code %flike% 'Carr' & dur>0.25)]; nr(w1) - nr(w2); 
w1[,.N,code]
w2[,.N,code]
w1 <- w2 # 1.2 M

# look at the Duplicates
setorder(w1,id,agedays)
x <- w1[code %flike% 'Dup',id]; .i <- c(sample(x,2),4106097); .i
x <- w1[id %in% .i,.(id,agedays,wt,code)]; x[1:40] %>% roundc(2)
d[id %in% x$id[1] & param=='W'][1:40,.(id,agedays,param,value,code)]

setorder(h1,id,agedays)
x <- h1[code %flike% 'Dup',id]; .i <- c(sample(x,2),4106097); .i
x <- h1[id %in% .i,.(id,agedays,ht,code)]; x[1:40] %>% roundc(2)
d[id %in% x$id[1] & param=='H'][1:30,.(id,agedays,param,value,code)]


# check Too Manys
x <- h1[code %flike% 'Many',.(id,agedays,ht,code)]; x
h1[id %in% sample(x$id,2),.(id,agedays,code,age,ht)]
w1[id %in% sample(x$id,2),.(id,agedays,code,age,wt)]

# exclude other errors
h1[,.N,code]
w1[,.N,code]

# there are still several Includes and CFs on the same day - pick the 'Include' (if present)
w2 <- w1[!(code %ilike% 'exclude')]; nr(w1)-nr(w2); w1[,.N,code]; w2[,.N,code]
w1 <- w2; w1 <- droplevels(w1); levels(w1$code) # Includes are first
nr(w1); uniqueN(w1, by=Cs(id,agedays)) 
# still have duplicates on same day  - these are CFs from previous days
setorder(w1,id,agedays,code)
x <- duplicated_rows(w1,by=Cs(id,agedays))[,.(id,agedays,wt,code)]; x[1:30]
x[id %in% sample(x$id,5),.(id,agedays,wt,code)]
d[id %in% sample(unique(x$id,2)) & param=='W', .(id,agedays,value,code)
  ][order(id,agedays)][1:50]
levels(w1$code); setorder(w1,id,agedays,code); w1[,.N,code] # will chose the include
w1[,seq:=seq_len(.N),.(id,agedays)]
w1[seq>1 & code %ilike% 'incl',.N];
# w1[id %in% sample(unique(x$id,2))][1:50]
nr(w1); w1 <- unique(w1, by=Cs(id,agedays)); nr(w1); w1[,.N,code]

h2 <- h1[!(code %ilike% 'exclude')]; nr(h1)-nr(h2); h1[,.N,code]; h2[,.N,code]
h1 <- h2; h1 <- droplevels(h1); levels(h1$code) # Includes are first
nr(h1); uniqueN(h1, by=Cs(id,agedays)) 
# still have duplicates on same day  - these are CFs from previous days
setorder(h1,id,agedays,code)
x <- duplicated_rows(h1,by=Cs(id,agedays))[,.(id,agedays,ht,code)]; x[1:30]
x[id %in% sample(x$id,5),.(id,agedays,ht,code)][1:50]
d[id %in% sample(unique(x$id,2)) & param=='H', .(id,agedays,value,code)
  ][order(id,agedays)][1:50]
levels(h1$code); setorder(h1,id,agedays,code); h1[,.N,code] # will chose the include
h1[,seq:=seq_len(.N),.(id,agedays)]
h1[seq>1 & code %ilike% 'incl',.N];
# h1[id %in% sample(unique(x$id,2))][1:50]
nr(h1); h1 <- unique(h1, by=Cs(id,agedays)); nr(h1); h1[,.N,code]

# ---------------------------
h1[,asmml(dur),old]  # also check fin.ht
s(h1[old==1,dur])
h1[,.N,code]; w1[,.N,code]
nr(h1); uniqueN(h1,by=Cs(id,agedays))
nr(w1); uniqueN(w1,by=Cs(id,agedays))

nr(w1); nr(w2)
x=Cs(fage,lage,adiff,sage,eage,seq,dur); 
fdel(x,w1)
fdel(x,h1)
w1[,Cs(cfw):=NULL]; h1[,Cs(cfh,old,fin.ht):=NULL]

setnames(w1,Cs(code),Cs(wcode)); setnames(h1,Cs(code),Cs(hcode))

worig <- copy(w1); horig <- copy(h1)
nr(w1); nr(h1)

h1[,':=' (htdays=agedays)][, Cs(age,sex):=NULL]; h1
w1[,':=' (wtdays=agedays)]; w1[,Cs(age):=NULL]; w1

setkey(w1,id,agedays); setkey(h1,id,agedays); 
x <- h1[w1]; univ(x) # 0.5 M missing ht
st(x <- h1[w1,roll='nearest'][,diff:=abs(wtdays-htdays)]); # 2s for the rolled merge
univ(x) # 48  K missing ht

s(x$wtdays-x$htdays)
cbind(seq(0.97,1,by=0.001),qu(x[,.(abs(wtdays-htdays))],seq(0.97,1,by=0.001))) # 95% not missing
x[,.N,.(diff==0)][,perc:=100*N/nr(x)][]

setorder(x,id,agedays)
x[,':=' (seq=1:.N,maxseq=.N), keyby=.(id)]
x[is.na(diff)] # 48 K missing
unique(x[is.na(diff)],by='id')[maxseq>1][1:40]
x[is.na(diff) & maxseq==1] # 4300 have only 1 record
x[id %in% c(2695,2095,2672,11174,4300,16562)] # none of these IDs have a height at any exam!
tot <- x

x <- copy(x[maxseq<10])
xx <- x[between(diff,1,100)]; xx
xs <- sample(xx$id,6)
x[id %in% xs][,Cs(seq,maxseq):=NULL][]
w1[id %in% xs]
h1[id %in% xs]
xout <- merge(h1[id%in% xs], x[id%in%xs], by=Cs(id,agedays), all.y=T); 
xout[,Cs(seq,maxseq,wtdays,wcode,hcode,sex):=NULL]; xout
we(xout)

tot; s(tot$diff)
tot[diff>61,':='(ht=NA)]
tot[,Cs(seq,maxseq):=NULL]
univ(tot) # added back ~250 K heights
tot[,Cs(wtdays,htdays)]; setnames(tot,Cs(diff),Cs(wh.day)); tot

nr(tot); obsi(n=10); totsi()
100*nr(tot)/nr(w1)
100*nr(tot)/nr(h1)
rm(d,dorig,h1,xxx,w2,dx,worig,w1,x)
d <- upData(tot)

class1(d)
setorder(d,id,agedays)
#d <- d[1:2000000]
d[,.N,sex]
d[,':=' (sexc=factor(sex,labels=Cs(b,g)), bmi=wt/(ht/100)^2, agemos=agedays/30.4375)]
d[,.N,.(sex,sexc)]
d[,sex:=fifelse(sexc=='b',1,2)]; d[,.N,.(sex,sexc)]

v=Cs(.b,.h,.len,.w); fdel(v,d)
sn(d); dim(d)

source('~/Sync/R/Functions/Growth_Chart_Functions/all_metrics/ext_bmiz.R')
st(d2 <- ext_bmiz(d, age=agemos, wt=wt, ht=ht,bmi=bmi)) # 

# merge with other info such as race etc
.dir1 <- '/Volumes/Samsung_T3/Data/EHR_data/PedsNet/fst_files/'
list.files(.dir1)
d1 <- fstread(p0(.dir1,'data_to_clean_mar23.fst'), as=T); dim(d1); sn(d1); head(d1)
v=Cs(id,vid,agedays,place,racec,hisp)
d1 <- d1[,..v]; d1
setorder(d1,id,agedays); nr(d1); d1 <- unique(d1,by=Cs(id,agedays)); nr(d1)

uniqueN(d2,by=Cs(id,agedays)); uniqueN(d1,by=Cs(id,agedays))
d <- d1[d2, on=Cs(id,agedays)]; nr(d1); nr(d2); nr(d)
# but what happends if there are multiple places on a day?
sn(d)
d[,.N,racec]

setnames(d,Cs(ext_bmiz,ext_bmip),Cs(ebz,ebp))
max(d$ebz); d[ebz==8.21,.(agedays,agedays/365,ebz,mod_bmiz,mod_waz,mod_haz,bmi)][order(agedays)]
d[,':='(age=agedays/365.25)]

v=Cs(id,age,agedays, wh.day, sexc,wt,ht,bmi,waz,haz,bmiz,bmip, racec,bmi_m, place,
     perc,adj_perc,adj_dist, bmip95,
     wcode,hcode,mod_haz,mod_waz,mod_bmiz,p95,ebz,ebp)
fnames(v,d)
d <- d[,..v]
rm(d2) 

d[,.N,.(hcode,wcode)]; sn(d)
xx <- d[between(mod_bmiz,8,10) | between(mod_waz,8,10)]; nr(xx); 100*nr(xx)/nr(d); 
# only 0.1 %

x <- d[!between(mod_bmiz,-4,10) & between(mod_haz,-5,4) & between(mod_waz,-5,10)]; 
nr(x); 100*nr(x)/nr(d) # delete 0.05%

x[,.(id,age,wt,ht,bmi,mod_waz,mod_haz,mod_bmiz,ebz)] %>% roundc(2)
sa(x[,.(id,age,wt,ht,bmi,mod_waz,mod_haz,mod_bmiz,ebz)],40) %>% .[order(age)] %>% roundc(2)
# almost all exclusions are due to very low BMIs ~10 to 11

d <- fseq(d)
nr(d); d <- d[between(mod_bmiz,-4,10) & between(mod_haz,-5,4) & between(mod_waz,-5,10)]; nr(d)
# 19.03 M to 18.71 M
x <- d[between(mod_bmiz,8,10)]; nr(x)
d[id %in% sample(unique(x$id),3),.(id,age,bmi,mod_bmiz,mod_waz,mod_haz,bmi)] %>% round(2)

range(d$age); sn(d)
d <- fseq(d) # next line is important
d[,.N,seq]
d[,.N,keyby=maxseq]
# save all data then exclude maxseq==1 from longitudinal analyses
# nr(d); d <- d[maxseq>1]; nr(d)
fxn(d)
# 17.3 M / 2.85 M children

x=Cs(seq,age,mod_haz,bmip95,hcode,wcode); fdel(x,d)
sn(d)
setwd('/Volumes/Samsung_T3/Data/EHR_data/PedsNet'); list.files()
st(fstwrite(d,'df_pedsnet_cleaned_060820.fst',compress=85))

mem_used(); totsi()
sapply(d,object_size)/1000000
rm(d,h2,tot,horig,d1)



# -------------------
fplo <- function(){
      ggplot(x2,aes(age,mod_waz)) + facet_wrap(~factor(id), scales='free' ) + 
        geom_line(data=x2, aes(age,mod_waz),linetype='dotted', color='grey25') + 
        geom_point(size=0.9) + theme_hc() +
        geom_point(data=x2[hres==1], aes(age,mod_waz), size=2.3, color='blue')
}
fpv <- function(){
      ggplot(x2,aes(age,wt)) + facet_wrap(~factor(id), scales='free' ) + 
        geom_line(data=x2, aes(age,wt),linetype='dotted', color='grey25') + 
        geom_point(size=0.9) + theme_hc() +
        geom_point(data=x2[hres==1], aes(age,wt), size=2.3, color='blue')
}

setwd('/Volumes/Samsung_T3/Data/EHR_data/PedsNet'); list.files()
st(d <- fstread('df_pedsnet_cleaned_060820.fst',as=T)); d # 7s
# contains kids who were examined only 1 time
nr(d[maxseq==1]) #1.1 M
uniqueN(d,by=Cs(id)) # 3.5 M total

d[,':=' (age=agedays/365.25)]

s(d[,.(age,mod_bmiz,mod_waz)])
d[,.N,keyby=maxseq]
dorig <- copy(d)

d <- d[maxseq>=3]
x=sample(d$id,400000,replace=F); len(x)
d <- d[id %in% x]; dim(d)

ds <- d
ds[,':=' (mv=mod_waz,v=wt)]

st(ds[,':=' (msm=supsmu(age,mv, span=0.3)$y), by='id']) # 108 seconds
st(ds[,':=' (wsm=supsmu(age,v, span=0.3)$y), by='id']) 
st(ds[,':=' (diff=abs(msm-mv), diff2=abs(wsm-v))]);

ds[,.(id,age,wt,v,mod_waz,mv,msm,diff,diff2,haz,waz)][1:20]  
s(ds[,.(diff,diff2)])
cp=qu(ds$diff,99.99/100); cp # 1.5 for 99.99
cp2=(qu(ds$diff2,99.9975/100)); cp2 # 11.4
ds[,':=' (hres=i.e(diff>cp & diff2>cp2, 1, 0))];   ds[,.N,hres] # 403 obs
x=ds[hres==1]; x$id[1:20]; len(unique(x$id)) # 296 kids [delete them]

xx <- sample(unique(x$id),25); xx
x2 <- ds[id %in% xx]
fplo() %+% labs(x='Age (y)', y='Mod Weight-Z')
fpv() %+% labs(x='Age (y)', y='Weight (kg)')
  
v=Cs(id,age,agedays,wt,ht,bmi)  
ds[id %in% c(86619,244477443),..v]

  nr(ds); d <- ds[id %nin% x$id]; nr(ds) - nr(d) # 4464 records
  lu(ds$id) - lu(d$id) # 296 kids
  
  d <- fxage(d)(d)
  d <- age14(d); fxn(d) # [1] 17 991518  2 845757
  n1 - c(nr(d),lu(d$id))
  x <- Cs(seq,maxseq,adiff,diff,fage,hres,lage,mmod_bmiz,mv,v,wsm); fnames(x,d)
  d[,(x):=NULL]; sn(d)

x=d[,.N,keyby=race]; x; x$perc=100*x$N/nr(d); x
class(d$race)

# I think the supermooths (below) are better and much faster
library(lme4); #library(lmerTest)
st(m1 <- lmer(mod_waz ~ sexc + ns(agedays,3) + (agedays|id), data=d, 
         control = lmerControl(calc.derivs = FALSE)))
                   # control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))) )

# w 300 000 IDs, this takes 85 s 
s(m1); sigma(m1); len(residuals(m1))

st(m2 <- lmer(bmi ~ sexc + ns(agedays,4) + (agedays|id), data=d, 
         control = lmerControl(calc.derivs = FALSE)))
s(m2); sigma(m2); len(residuals(m2))

d[,':=' (resm=residuals(m1),resw=residuals(m2),  
         predm=predict(m1, type='response', re.form=NULL),
         predw=predict(m2, type='response', re.form=NULL)
)]
d[,':='(resm2=abs(predm-mod_waz), resw2=abs(predw-wt))]

s(d[,.(predm,resm,resm2,predw,resw,resw2)])

qm <- qu(abs(d$resm2),.9995); qm
qw <- qu(abs(d$resw2),.9995); qw

x <- d[abs(resm2)>qm & abs(resw2)>qw,.(id,agedays,mod_waz,resm2,wt,resw2)]; x
d[,hres:=fifelse(abs(resm2)>qm & abs(resw2)>qw,1,0)]; d[,.N,hres]
x <- d[hres==1,.(id,age,mod_waz,resm,hres)]; x
x2 <- d[id %in% sample(x$id,25),.(id,age,wt,mod_waz,resm,hres)]; x2[1:55]

fplo()  
fpv()

# -----------------------------

# https://rdrr.io/cran/akmedoids/man/outlierDetect.html
# install.packages('akmedoids'); require(akmedoids)
# outlierDetect(traj, id_field = FALSE, method = 1, threshold = 0.95,
# st(d[maxseq>3, pred:=predict(rlm(bmi~poly(age,3), method='M',maxit=200)),id])
# takes 



# Ethnicity: 0-5; 0 = African American, 1 = Asian, 2 = Caucasian, 3 = Hispanic, 
# 4 = Other, 5 = Uknown   

dx <- copy(d)
dx[,':=' (racen=race,race=mapvalues(race,c(0:5),Cs(black,asian,white,hisp,other,unknown)))]
dx[,race:=factor(race)]
dx[,.N, keyby=.(racen,race)];
d <- dx

fxn(d) #   17 991518  2 845757 
sn(d); 
x <- Cs(id_link,diff2,exp.bp,hcode,wcode,msm,p97); fnames(x,d)
d[,(x):=NULL]; sn(d)

class1(d)
d <- upData(d)
setwd('/Volumes/Samsung_T3/Data/EHR_data/IQVIA/Ob_HAZ'); list.files()
fstwrite(d, 'iqvia_ob_ht_clean_110619.fst', compress=75)






####### --------------------









# https://en.wikipedia.org/wiki/Studentized_residual
# If the estimate σ2 includes the i th case, it is the internally studentized residual,
# Excluding the i th case is called the externally studentized residual
# rstandard(model, ...)	internally studentized. See [2]
# rstudent(model, ...)	externally studentized. See [3]

# So, want rstudent

# this idea really sucks - don't use it!!!!!!
lm(mod_bmiz~sexc+poly(age,2), data=d)
st(d[maxseq==3,res:=rstudent(lm(mod_bmiz~agedays)), id])
st(d[maxseq>6,res:=rstudent(lm(mod_bmiz~poly(agedays,1))), id]) 
# 4 min 0.5 M // 
pdes(d$res)
ds <- d[abs(res) > 100]; ds
d[id %in% ds$id,.(id,seq,age,wt,ht,bmi,mod_bmiz,ebz,res)][order(id,age)][1:50]


library("doFuture")
registerDoFuture()
plan(multiprocess)
library(margins)
getDoParWorkers()
doParallel::registerDoParallel(cores = 6) # do NOT use 7 or 8 cores !

st(x <- ddply(d[maxseq>3], ~id, .parallel=T, function(df){
    d$res2 <- rstudent(lm(mod_bmiz~poly(agedays,2), data=df))
}))




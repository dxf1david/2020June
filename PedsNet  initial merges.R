# check out rowidv for speed !!! -------

# line 227 might be the answer - is it that pid in deme doesnt match with pid in visit?
# maybe merge visit and deme by both pid AND vid


# setwd(p0(.ss, '/EHR_data/PedsNet/')); getwd(); list.files()
# st(d <- vroom('cleaned_with_growthcleanr.csv.gz')) # 15 s
# st(d <- read_fst('cleaned_with_growthcleanr.fst', as=T)) # 15 s
# some(d); fxn(d) # 47324246  3501151
# class1(d); class(d)
# setnames(d,'subjid','id')
# 
# d[,':=' (age=agedays/365.25, code=factor(as.character(code)))]
# d[1:10]; d[,.N,param]
# fseq(d); s(d[,.(seq,maxseq)]); # maxseq is 185240  
# d[,.N,maxseq][order(maxseq)][maxseq>2000]
# d[maxseq>180000]
# d[id==244594088 & param=='W',.N,agedays]
# d[id==244594088 & param=='W' & agedays==803,s(measurement)]
# d[id==244594088 & param=='W' & agedays==984,s(measurement)]
# truehist(d[id==244594088 & param=='W' & agedays==803,measurement])
# 
# unique(d[maxseq>2000,.N])
# x <- d[maxseq>2000 & seq==1,.(id,seq,maxseq,agedays,param,measurement),.N]
# x
# x[order(maxseq)]
# fwrite(x,'kids with more than 2000 values.csv')
# --------------

# d <- read_sas('/Users/davidfreedman/Sync/R/Anal/PedsNet/lookup.sas7bdat'); 
# setDT(d); d; we(d)
# 
# 
# list.files(p0(.ss,'EHR_data/PedsNet'));
# list.files(p0(.ss,'EHR_data/PedsNet/SAS files'));
# setwd(p0(.ss,'EHR_data/PedsNet/CSV files'));
# 
# d <- read_sas('/Volumes/Samsung_T3/Data/EHR_data/PedsNet/lookup.sas7bdat') %>% setDT
# d; we(d)
# setwd(p0(.ss,'EHR_data/PedsNet/CSV_files')); list.files()
# list.files() ---------------------
# demo <- fread('pat_dem_tbl.csv'); dim(demo); demo
#    fstwrite(demo,'pat_dem_tbl.fst', compress=75)
# codes <- fread('chron_cond_codeset_tbl.csv'); dim(codes); codes
#    fstwrite(codes,'chron_cond_codeset_tbl.fst', compress=75); dim(codes); codes
# chron <-  fread('chron_cond_tbl.csv'); dim(chron); chron # 38 705 661 
#    fstwrite(chron, 'chron_cond_tbl.fst', compress=75); dim(chron); chron # 38 705 661 
# ins <-  fread('ins_tbl.csv'); dim(ins); ins; # 77 675 377
#    fstwrite(ins,'ins_tbl.fst', compress=75); dim(ins); ins; # 77 675 377
# meas <-  fread('meas_tbl.csv'); dim(meas); meas; # wts and hts in 1 column; 91 M
#    fstwrite(meas, 'meas_tbl.fst'); dim(meas); meas; # wts and hts in 1 column; 91 M
# visit <-  fread('visit_tbl.csv'); dim(visit); visit; # agedays, 92 M
#    fwrite(visit,'visit_tbl.csv.gz'); visit; # agedays, 92 M
#    fstwrite(visit, 'visit_tbl.fst'); dim(visit); visit; # agedays, 92 M
#-------------
# look at 244594088 -----------
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
.pid <- 244594088
demo <- read_fst('pat_dem_tbl.fst',as=T)[person_id==.pid]; demo

meas <-  read_fst('meas_tbl.fst', as=T)[person_id==.pid]; meas # 8222
unique(meas,by='visit_occurrence_id') #19
unique(meas,by='measurement_id') # 8222

meas[,.N,.(visit_occurrence_id)]; 
meas <- unique(meas,by='visit_occurrence_id'); meas #19

visit <-  read_fst('visit_tbl.fst', as=T)[person_id==.pid]; visit # 475
visit[order(age_at_visit_days)]
unique(visit,by='visit_occurrence_id') # 475

x <- merge(meas,visit,by=Cs(person_id,visit_occurrence_id), all=F)
x[unit_concept_id==9529][order(age_at_visit_days)] #13
x[unit_concept_id==8582][order(age_at_visit_days)] # 6
x
#-------------------------

## do we want emergency room visits ##### ???

# measurements: pid / mid (measurement) / vid (visit)
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
st(meas <-  read_fst('meas_tbl.fst', as=T)); dim(meas); meas; # 81 M, 4 s
setnames(meas, Cs(mid, vid, pid,   mconc.id, uid, value,   vconc.id)); meas
obsi(); meas <- upData(meas); obsi()
meas[,.N,mconc.id]; meas[,.N,uid]
# delete head circ: (and kg/cm ==0) / delete 4.5 M
nr(meas); meas <- meas[mconc.id != 3001537 & between(uid,1,9999)]; nr(meas) 
names(meas)
# mid = uniq meas ID / vid = uniq visit ID/ pid = uniq patient id / 
# mconc.id=meas conc id / uid = unit concept id
# wt,ht,etc / kg,lbs,etc /  value / value_as_concept ID? (0 or NA)
set_cols_first(meas,Cs(pid,vid))
meas[pid %in% c(244594088)][order(value)]

setorder(meas,pid)
st(x <- uniqueN(meas, by='pid')); x # 5.2 M kids, 75.6 M records
# and faster the len(unique(.))
.keep1=meas$pid[1:20e6]; len(.keep1); .keep1
.keep2=meas[pid %in% c(244594088,4366790)]$pid; .keep2
.keep <- unique(c(.keep1,.keep2)); len(.keep); .keep
# meas <- meas[pid %in% .keep]; 
nr(meas); meas

st(meas[, ':=' (param=as.factor(fcase(
   mconc.id==3025315, 'W', # not Wr - # Sam says that all weights are measured:
   mconc.id==3013762, 'W',
   mconc.id==3023540, 'H',
   mconc.id==3036277, 'Hr')),
             code=as.factor(fcase(
   uid==9529, 'k',
   uid==8739, 'l',
   uid==8582, 'c')
   ))]) #8 s much faster than mapvalues

meas[,.N,.(param,mconc.id)];  meas[,Cs(mconc.id):=NULL]
meas[,.N,.(uid,code)]; meas[,Cs(uid):=NULL]  # 820 were in pounds

meas[,.N,vconc.id]; meas[,Cs(vconc.id):=NULL]  #aout 50% 0 and 50% NA
meas[pid %in% c(244594088) & param=='W'] # 5936


# visit ---------
st(visit <-  read_fst('visit_tbl.fst', as=T)); dim(visit); visit; # agedays, 92 M
visit[,Cs(chron_cond_flag):=NULL]; visit
setnames(visit, Cs(vid,pid,place.id,agedays)); # fxn(visit) # 92 M, 5.3 M
# visit occurence id // person id // visit_concept_id // age at visit
# visit <- visit[pid %in% .keep]; nr(visit)
visit <- visit[between(agedays,2*365.25,19.999*365.25)]; nr(visit) # 70.1 M
setorder(visit,pid,agedays)
set_cols_first(visit,'pid'); visit[1:30]
visit[,.N,place.id]
visit[,place:=factor(fcase(
   place.id==9202, 'outpat',
   place.id==9201, 'inpat',
   place.id==9203, 'emerg',
   place.id==2000000469, 'out_nomd',
   place.id==2000000088, 'obs',
   place.id==2000000048, 'emerg.stay'
))]
visit
visit[,.N,.(place,place.id)]; visit[,Cs(place.id):=NULL]

setkey(visit,pid,vid)
st(x <- visit[,.(.N,min=min(agedays),max=max(agedays)),.(pid,vid)]); x; range(x$N)
some(x,45)[order(min)]
x[min!=max] # all the same - each vid is only 1 day !!!!
# so it looks like a visit_id covers only 1 days
nr(visit) # 70 197 019 after age exclusions, deleted about 20 M

# now look at vid in meas dataset - decisions about different IDs within a VID
setkey(meas,vid)
x <- meas[,.(mpid=mean(pid)),vid]; setkey(x,vid); 
st(meas <- meas[x,nomatch=0]); # 5s
# 75 screwed up smae vid, but different IDs
# Decision -- what to do if there are multiple patient IDs per visit_id?
meas[pid!=mpid, .(vid,mpid,pid,value)][order(vid,pid)][1:55]; 
meas[mpid!=pid]; unique(meas[mpid!=pid], by='mpid')  # N= 199, bu only 12 unique 
nr(meas); meas <- meas[mpid==pid]; nr(meas); # delete
meas[,Cs(mpid):=NULL]

# now work on the numerous wts/hts on the same pid/vid
# meas2 <- meas[,.(value=mean(value)),.(pid,vid,param)]; nr(meas); nr(meas2)
meas.orig <- copy(meas);

dw <- meas[param=='W' & !is.na(value)]; dw[,wt:=value]; 
dw[,Cs(value,param,code,mid):=NULL]; quantile(dw$wt,1:20/20)
dh <- meas[param=='H' & !is.na(value)]; dh[,ht:=value]; dh <- dh[,.(pid,vid,ht)]
quantile(dh$ht,1:20/20)
dw[1:10]; dh[1:10]

# Decision - what to do about the MANY measurements for a pid/vid combo?
# for pid/vid combos, keep only the the first 10 values wt and 10 values of ht
x <- dw[,.(sdwt=sd(wt)),.(pid,vid)]; s(x$sdwt[!is.na(x$sdwt)]); 
x[sdwt>6000]
dw[pid %in% c(1941346)][order(pid,wt)]
dw[pid %in% c(2346370)][order(pid,wt)]

# dw[,Cs(wseq,seq,max,N):=NULL]
.i <- 244594088
set.seed(1234); dw$rand <- rnorm(nr(dw),0,1); 
setorder(dw,pid,vid,rand)
dw[pid %in% .i][order(wt)] #,4366790)]
dw[pid %in% .i][order(vid)] #,4366790)]
st(dw$wseq <- rowidv(dw,Cs(pid,vid)))
range(dw$wseq)
dw[pid==.i & vid==260821490] #[order(pid,vid)]
nr(dw); dw <- dw[wseq<=10]; nr(dw); dw[1:20] # drop 4 M (48 to 44 M), limit to those with <10 on same day
dw[pid==.i & vid==260821490] #[order(pid,vid)]
dw[,Cs(rand):=NULL]; dw

# dh[,Cs(hseq,seq,max,N):=NULL]
set.seed(1234); dh$rand <- rnorm(nr(dh),0,1); 
setorder(dh,pid,vid,rand)
st(dh$hseq <- rowidv(dh,Cs(pid,vid))); dh[1:20] #1.2
nr(dh); dh <- dh[hseq<=10]; nr(dh); dh[1:20] # drop 200 K - limit to those with <10 on same day
dh[,Cs(rand):=NULL]; dh

nr(dw); nr(dh); setkey(dw,pid,vid); setkey(dh,pid,vid)
x1  <- dw[dh, nomatch=0]; nr(x1); x1[1:20] # 35 M (from merging 44M with 27M)
meas[pid %in% .i] 
x1[pid %in% .i] # N goes from >5000 to 1400
x <- unique(x1, by=Cs(pid,vid,wt,ht)); nr(x1)-nr(x); nr(x) # delete 2.8 M, 32 M
x[pid %in% .i][order(pid,wt)] #1353 for .i
s(x[pid %in% .i][,.(pid,vid,wseq,hseq)])

nr(x); nr(meas); nr(meas.orig) # 75 to 32 M
x; s(x[,.(wseq,hseq)])
meas <- x1[, Cs(wseq,hseq):=NULL]; meas[1:20]
class1(meas)
 
nr(meas); # 35 M
meas


# demo person_id is 'pid'
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
st(demo <- read_fst('pat_dem_tbl.fst',as=T)); head(demo); dim(demo) # 5.3 M
setnames(demo,Cs(pid,sex,race,ethnic,ybirth));  fxn(demo)
# person ID // gender // race // ehnicity /// year of birth
# demo <- demo[pid %in% .keep]; 
set_cols_first(demo,'pid')

demo[,.N,sex]; demo[,.N,race]; demo[,.N,ethnic];
demo[,.N,ybirth][order(-N)]

st(demo <- mutate(demo, 
               ybirth=factor(as.character(ybirth)),
               sexc=factor(sex,Cs(8532,8507), Cs(f,m))
               ))
st(demo[,':=' (
     racec=as.factor(fcase(
        race==8657, 'aind',
        race==8515, 'as',
        race==8516, 'b',
        race==8557, 'pac',
        race==8527, 'w',
        race==44814659,'mult',
        race==44814649, 'other',
        race %in% c(44814650, 44814660, 44814653,188013), 'miss')),
     hisp=as.factor(fcase(
      ethnic== 38003563, 'y',
      ethnic==38003564, 'n',
      ethnic==44814649, '49',
      ethnic==44814653, '53',
      ethnic==44814650, '50'))
)]) # 1 s
# demo[race %in% c(44814650, 44814660, 44814653),racec:='miss']
demo; lu(demo$pid) # 1 unique ID per record
demo; demo[,.N,keyby=ybirth]
demo[,.N,.(sex,sexc)]; demo[,.N,sexc]; demo <- demo[!is.na(sexc)]
demo[,.N,.(race,racec)][order(race)]; demo[,.N,racec]
demo[,.N,.(hisp,ethnic)]
demo[1:25]
v=Cs(ethnic, sex, race); fnames(v,demo); demo[,(v):=NULL]; names(demo); class1(demo)
demo[1:25]


# merge demo and meas by pid
dim(demo); dim(meas); 
class1(demo); class1(meas)
setkey(demo,pid); setkey(meas,pid)
st(deme <- demo[meas, nomatch=0]); nr(demo); nr(meas); nr(deme)
# about 900 deleted from meas dataset
deme[pid==.i] 
deme[1:25]
nr(demo); nr(meas); nr(deme) 

x <- duplicated_rows(deme, by=Cs(pid,vid,wt,ht)); nr(x) # 4.46 M
nr(deme); uniqueN(deme,by=Cs(pid,vid,wt,ht))
setorder(x,pid,vid,wt,ht); x[1:45] # 2.8 M not unqiue
deme$seq <- rowidv(deme, Cs(pid,vid,wt,ht)); deme[,.N,keyby=seq]
setorder(deme,pid,vid,wt,ht)
deme[pid==824][1:50]
nr(deme); deme <- deme[seq==1]; nr(deme); # 35 to 32 M
deme[,Cs(seq):=NULL]; deme

deme <- upData(deme, force.single = T)
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
# st(fstwrite(deme,'demo_meas_mar22.fst', compress=75)) #
   
# st(deme <- fstread('demo_meas_mar22.fst', as=T)) # 1s
# the visit data look fine - multiple visits per id, but only 1 id for each visit !!!

# now merge deme (demo/meas) with visit
sn(visit); visit
x <- visit[,.N,.(pid,vid,agedays)]; x[N>1] # each visit covers only 1 day
tab(x$N)
visit[pid==4366790 & agedays==1086] # SHIT !!!! n=1766
deme[pid==4366790] # SHIT !!!! n=52
deme[pid==.i] # 1360

# merge visit and deme
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
deme <- read_fst('demo_meas_mar22.fst', as=T); nr(deme)
# deme <- deme[pid %in% .keep]

deme[pid==.i]; visit[pid==.i];  
sn(visit); nr(visit); 
sn(deme); nr(deme)
setkey(deme,vid,pid); setkey(visit,vid,pid)

st(d1 <- merge(deme,visit,by=Cs(vid),all=F)); 
# st(d1 <- visit[deme,on=Cs(vid,pid),nomatch=0]); 
nr(deme); nr(visit); nr(d1) # 21.9 M (from 32 M in deme)
d1[pid.x != pid.y, .N] # 78
d1[pid.x != pid.y, .(pid.x,pid.y,vid,agedays) ][order(pid.x,pid.y)] 
d1 <- d1[pid.x==pid.y]
setnames(d1,'pid.x','pid'); d1$pid.y <- NULL
# same visit id has different pid's in deme and visit data sets
# d1[pid.x==244594088 | pid.y==244594088]

# setkey(deme,pid,vid); setkey(visit,pid,vid)
# a merge this ways gives the same N as above with the 78 deletions

# nr(deme); nr(visit)
# st(d2 <- merge(deme,visit,by=Cs(pid,vid),all=F)); nr(d2) # 23 M
# nr(d1); nr(d2)
# nr(d2); d3 <- unique(d2,by=Cs(pid,vid,agedays)); nr(d3)
d1[pid==.i][order(agedays)] # N=480
d <- d1

# nr(d1)-nr(d) # 633 000 diff
nr(d)
uniqueN(d,Cs(pid)) # 3.5 M
uniqueN(d,Cs(pid,vid)) # 19.7 M
uniqueN(d,Cs(pid,vid,agedays)) # same as above
d # 22M

# d$seq <- rowidv(d,Cs(pid,vid)); range(d$seq) # 1 to 100
x <- d[,.N,.(pid,vid,agedays)]; qu(x$N,probs=seq(.975,1,len=20))
x <- d[place=='emerg',.N,.(pid,vid,agedays)]; qu(x$N,probs=seq(.95,1,len=20))
d
d$seq <- rowidv(d,Cs(pid,agedays,wt,ht)); range(d$seq) #1 to 7
x <- d[,.(max=max(seq)),.(pid,vid)]; 
x[,.N,max]
x[max==7]; d[pid %in% c(3893307)][order(pid,agedays)] #[place=='emerg']
d[pid %in% c(2087)][order(pid,agedays)] #[place=='emerg']
meas.orig[pid==1392 & param=='W'][order(value)]
d <- d[x,on=Cs(pid,vid)]; range(d$max);

x <- d[max==7]; d[pid %in% x$pid[1]][order(pid,vid,seq)]
d[max>=6]; d$max <- NULL
nr(d); d <- unique(d, by=Cs(pid,agedays,wt,ht)); nr(d) # delete 400K to 21.47 M
d$seq <- rowidv(d,Cs(pid,agedays)); range(d$seq)
x <- d[,.(max=max(seq)),.(pid,agedays)]; 
d <- d[x,on=Cs(pid,agedays)]
d[,.N,keyby=max]
d[max>=4][order(pid,agedays)][1:40][,.(pid,vid,wt,ht,agedays,seq,max)]
d[pid %in% c(1034473)]

setorder(d,pid,vid)
x <- d[,.(bage=min(agedays),lage=max(agedays)),by=.(pid,vid)]
x[,diff:=lage-bage]; s(x$diff) 

d[,Cs(seq,max,i.max):=NULL]; d
d <- upData(d)
class1(d)

dm <- melt(d, measure.vars=Cs(wt,ht)); dm # 43 M
uniqueN(dm, by=Cs(pid,agedays,variable,value))
dm <- unique(dm, by=Cs(pid,agedays,variable,value))
nr(dm) #40.0055 

dm
setnames(dm,Cs(pid,sexc,variable),Cs(id,sex,param))
dm[,.N,sex]; dm[,.N,param]
dm[,':=' (sex=fifelse(sex=='m',0,1),
          param=fifelse(param=='wt','WEIGHT','HEIGHT')
          )]
some(dm,20)
obsi()
setwd(p0(.ss,'EHR_data/PedsNet/fst_files')); list.files()
st(fstwrite(dm, 'data_to_clean_mar23.fst', compress=75)) #50
# st(fwrite(dm, 'data_to_clean_mar23.csv.gz')) # 72 (same size)
# x <- spec_csv("data_to_clean_mar23.csv.gz"); x



# test ----

setwd(p0(.ss,'EHR_data/PedsNet/fst_files')); list.files()
# st(d <- fstread('data_to_clean_mar23.fst')); class1(d). nr(d) # 7 s
st(d <- vroom('data_to_clean_mar23.csv.gz')); class1(d) # 7 s
st(d <- vroom('data_to_clean_mar23.csv.gz'))
setDT(d)
# st(d <- fread('data_to_clean_mar23.csv.gz')); class1(d) # 17 s
d[,':=' (hisp=factor(hisp),param=factor(hisp),place=factor(place),
         racec=factor(racec), agedays=integer(agedays),id=integer(id),
         vid=integer(vid),ybirth=integer(ybirth),sex=factor(sex))]
class1(d)

d[,(hisp,param,place,racec):=lapply(.SD,function(x)factor(x)), .SDcols=Cs(hisp,param,place,racec)]







# END -------------

dm <- read_fst(p0(.ss,'EHR_data/PedsNet/fst_files/data_to_clean_mar23.fst'),as=T) # 51.5 M
nr(d); sapply(d,fmiss); 
d[,.N,racec]
d[,.N,hisp]
d[,id:=pid]
st(fxage(d))
# st(d[,':=' (seq=1:.N,maxseq=.N, fage=age[1], lage=age[.N]),.(id)][, adiff:=lage-fage])
s(d$lage-d$fage)
d[lage-fage>10 & fage<8 & (age==fage  | age==lage),.(id,age,fage,lage,param,value)][order(id)]
d[lage-fage>10 & fage<8 & (age==fage  | age==lage),.(id,age,fage,lage,param,value)][order(id)][1:50]
dw <- d[param=='W',.(id,sexc,age,value)]; dw # 31 M
dh <- d[param=='H',.(id,age,value)]; dh # 20.2 M
ds <- dw[dh,on=Cs(id,age),nomatch=0]
nr(ds) # 23.8 M
setnames(ds,Cs(id,sexc,age,wt,ht)); ds[1:50]
uniqueN(ds,by=Cs(id,age)) # 19.2 M

# for samantha - bad IDS

setkey(d1,pid,vid); 
d1[,maxseq:=.N,by=.(pid,vid)]; d1[,.N,keyby=maxseq] # up to 1900 times
d1[maxseq>100,.N]

setkey(d1,vid,pid); 
d1[,maxseq:=.N,by=.(vid,pid)]; d1[,.N,keyby=maxseq][] # up to 1900 times
d1[maxseq>100,.N]
d1[maxseq>100 & (pid == 244594088 | pid.y == 244594088) & param=='W'][order(agedays)][1:40]

setkey(d1,vid,pid.y); 
d1[,maxseq:=.N,by=.(vid,pid.y)]; d1[,.N,keyby=maxseq] # up to 1990 time


v=Cs(vid,pid,pid.y,mid,vid,agedays,place,sexc,param,value); v
d1[pid == 244594088 | pid.y == 244594088,..v] # pid is deme, pid.y is visit
xx <- d1[pid != pid.y,..v]; xx
setorder(xx,vid,pid,pid.y)
xx[1:30]
setnames(xx,Cs(visit.id, id.meas,id.demo,id.visit, agedays, place,value,sex,param,value)); xx
fwrite(xx,'multiple ids per visit_id.csv')
# same did it the first way - send this to Samantha
# sam merged only by visit.id



# ------------------------------

xx <- setdiff(d1$id,d2$id); xx
# setdiff(d2$id,d1$id) # N=0
deme[id %in% xx,.(id,vid,value,param)][order(id,vid)][1:10]
visit[id %in% xx,.(id,vid,age,place)][order(id,vid)][1:20]

deme[id==25224]
visit[id==25224][order(vid)]




fxn(d1); fxn(d2)
setorder(d,id,agedays,vid)
d[,.(id,vid,agedays,value,param)][1:30]
d[,.N,.(param,code)]; nr(d)
# 7186 W but don't know the coding - are they c
d[param=='W',asmml(value),.(code)]

# d <- upData(d)
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
# fstwrite(d, 'temp1_merged.fst', compress=75)


st(d <- read_fst('temp1_merged.fst', as=T)); fxn(d) # 53 290754  4 346964  // 10s
# all have a value but 7186 Ws are missing code (uid = unit_concept_id)
sn(d); fvalid(d$value); fmiss(d$value)
d[,.N,sex]
d[,':=' (age=agedays/365.25)]; range(d$age)
s(d$value); 
d[,.N,.(code,param)] # 33012784 W /  20270784 H / 8186 code is NA
fxage(d)
qu(d[maxseq>1 & (lage-fage>1),.(lage-fage)])
x <- d[maxseq>1 & (lage-fage>1),.N]; x # 46 M
uniqueN(d[maxseq>1 & (lage-fage)>1],by='id') # 22 M
ds <- d[maxseq>1 & (lage-fage)>1]
setorder(ds,adiff)
ds[,.N,.(fu=int(adiff))]
ds[,.N,keyby=maxseq]


# ------------------


sn(meas); nr(meas)
sn(visit); nr(visit)
meas;
visit
# st(setkey(meas,id,vid)) # 7
# st(setkey(visit,id,vid)) # 4
# st(d <- meas[visit, nomatch=0]); nr(d) # 9 s
st(d <- meas[visit, on=Cs(id,vid), nomatch=0]); nr(d) # 23 s / 53 M
d <- cleanup.import(d)
fstwrite(d,'initial_merge.fst',compress=75)



look <- fread(p0(.ss,'EHR_data/PedsNet/CSV files/lookup_tbl.csv')); look
cond <- fread(p0(.ss,'EHR_data/PedsNet/CSV files/chron_cond_codeset_tbl.csv')); cond

.dir <- p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data'); setwd(.dir); list.files()
st(ins <- read_fst('ins_tbl.fst', as=T)) # 5 s, 78 M
ins; nr(ins)
setnames(ins,Cs(payer.id,id,vid,ins.type)); ins
ins[,.N,ins.type]

st(cc <- read_fst('chron_cond_tbl.fst', as=T)) # 5 s, 78 M
cc
setnames(cc,Cs(condition.id, vid, cond.conc.id, system, prog)); cc
cc[,.N,cond.conc.id]
cc[,.N,system]
cc[,.N,prog]





#########################################
setwd(p0(.ss,'EHR_data/PedsNet/fst_files/Orig_data')); list.files()
st(d <- read_fst('initial_merge.fst',as=T)) # 3 s
fxn(d); fxn(demo); d <- d[demo, nomatch=0, on=Cs(id)]; fxn(d)
uniqueN(d,by=Cs(id,vid)); nr(d)
uniqueN(d,by=Cs(id,agedays)); 
uniqueN(d,by=Cs(id,vid,agedays)); 
x <- duplicated_rows(d,by=Cs(id,agedays))
# fxn(d); fxn(cc); d <- cc[d, on=Cs(vid)]; fxn(d) 
d[]

d[,max1:=.N, by=.(id,vid)]; s(d$max1); d[max1==1990]
d[,max2:=.N, by=.(id,agedays)]; s(d$max2); d[max2==1990]
st(d[,max3:=.N, by=.(id,agedays,vid)]); max(d$max3) 

ds <- d[id==244794085]; ds
ds[,.N,param]; 
ds[param=='W',.N,keyby=agedays]; 
ds[param=='W',.N,keyby=.(agedays,vid)]
ds[param=='W',.(id,vid,mid,agedays,param,param2,value)][1:99][order(value)]

ds[param=='W' & vid==245542892,.(length(unique(value)))]
ds[param=='W' & vid==264915076,.(length(unique(value)))]
ds[param=='W' & vid==247602312,.(length(unique(value)))]
ds[param=='W',.N,vid]


meas[,.N,vconc.id] # half are 0, rest are NA
meas[,Cs(vconc.id):=NULL] # iqvia 9529=kg, 8582=cm, 8739=lbs, 9330=inch (none)
meas[,.N,uid]
meas
meas[,':=' (seq=1:.N,maxseq=max(.N)),by=.(person_id,visit_occurrence_id)]
meas[maxseq==1991,.(person_id,visit_occurrence_id,value_as_number,uid)
     ][order(uid, visit_occurrence_id)]
unique(meas[maxseq==1991,.(person_id,visit_occurrence_id,value_as_number,uid)
     ][order(uid, visit_occurrence_id)], by=Cs(value_as_number))

names(visit);
names(visit); setnames(visit,Cs(vid, id, concept.visit, agedays)); visit
visit <- visit[agedays>365*2]; nr(visit); visit[1:25] # 71.77 M
uniqueN(visit,by=Cs(id,agedays)) # 57 M
uniqueN(visit,by=Cs(id,vid)) # 71.77 M
# so for an ageday, there must be multiple visits
setorder(visit,id,vid)

visit <- visit[1:1e7]
visit[,':=' (seq=1:.N,maxseq=.N), by=.(id,agedays)]
visit[maxseq>20,.(id,agedays,seq,maxseq,vid)][order(id,agedays)][1:50]

names(meas); 
meas
setnames(meas,Cs(measid, vid, id,  
                 meas_concept_id, unit.concept, value)) 
meas # 81 M
meas[,.N,unit.concept] #  almost all are 9529 or 8582
uniqueN(meas,by=Cs(id,vid)) # 41 M

names(visit)
names(meas)

meas[id==824][order(vid)][1:30]
visit[id==824][vid==30122872]


# tot <- merge(meas,visit,by=Cs(id,vid)); dim(tot) #56 M
tot <- visit[meas, on=Cs(id,vid), nomatch=0]; dim(tot) #56 M
setorder(tot,id,agedays,vid,measid)
d <- tot[,.(id,agedays,vid, concept.meas, value)]; nr(d)
d[1:40]

d <- d[1:10000000]

uniqueN(d,by=Cs(id,agedays)); nr(d) # 30 M vs 56 M
uniqueN(d,by=Cs(id,agedays,value)); nr(d) # 53 M vs 56 M
rm(meas,visit,ins)
# d[,':=' (seq=1L:.N, maxseq=.N), by=.(id,agedays,value)]
d[,':=' (seq=1L:.N, maxseq=.N), by=.(id,agedays,vid)]
srm(d[,.(seq,maxseq)])
d[maxseq>50,.N,keyby=.(maxseq,seq)]
uniqueN(d,by=Cs(id,agedays))
uniqueN(d,by=Cs(id,vid))
uniqueN(d,by=Cs(id,agedays,vid)) 
# can be more than 1 vid on same date, but not that often

setorder(d,id,agedays); d[,':=' (seqage=1L:.N), by=.(id,agedays)]
setorder(d,id,vid); d[,':=' (seqvis=1L:.N), by=.(id,vid)]
setorder(d,id,agedays,vid)
d[seqage == seqvis & maxseq>20][1:30]
d[seqage != seqvis]
d[id==708 & seqage !=seqvis]
d[id==708]

d[,':=' (seq=1L:.N, maxseq=.N), by=.(id,agedays,vid)]
d[maxseq==25][1:50] 
d[maxseq==50][1:50] 
d[maxseq==101 & concept.meas==8582] 
d[maxseq==101 & concept.meas==9529][1:30] 
# same visit id on same day for same kid - the values are different for each record
d[maxseq==40]

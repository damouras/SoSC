# install.packages('gsheet')
library(gsheet)
library(readr)

gs_url=c(
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=0',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=362070266',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1846708016',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=285222350',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=287963984',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=902352170',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1321569172',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=107089535',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=777810774',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=978836460',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1644061781',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1531659979',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=2037057410', #"UQaM"
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=600598602',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=810571285',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1890498929',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1801766388',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=417639363',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=10843667',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=172180791',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=2055915148',#PEI
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=2110546081',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=259609452',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=2087105691',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1432031843',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=2050655731',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=281283445',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=363641782',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=285775522',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=992465662',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=1975153583',
  'docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=111048053'
)

lop.gs=gsheet2tbl(gs_url[1])
write_csv(lop.gs,"./Data/Stats Program Data - List of Programs.csv")

for(i in 1:31){
  temp=gsheet2tbl(gs_url[i+1])
  fname=paste("./Data/Stats Program Data - ", lop.gs$UNIVERSITY[i], ".csv", sep="")
  write_csv(temp,fname)
}

# temp=gsheet2tbl('docs.google.com/spreadsheets/d/1PCuh9TvNxHxZWvUWeH_jy5tbp1IKP9JsFbkJP0PEgwU/gid=2037057410')
# fname=paste("./Data/Stats Program Data - UQaM.csv", sep="")
# write_csv(temp,fname)


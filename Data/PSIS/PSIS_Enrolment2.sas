*
*RTRA Frequency
*Created: May 24, 2017
*Data set: PSIS Post Secondary Institution System
*
*
*****************************************************;

data work.psis;
set RTRAData.Enrolment;
if psis_insttype_d_code="1";
if psis_prgcodecip6_d_code="13.0603" | psis_prgcodecip6_d_code="26.1102" | psis_prgcodecip6_d_code="27.0501" | psis_prgcodecip6_d_code="27.0502" | psis_prgcodecip6_d_code="27.0599" | psis_prgcodecip6_d_code="27.9999" | psis_prgcodecip6_d_code="52.1302";
if psis_pcsce_d_code="66" | psis_pcsce_d_code="76" | psis_pcsce_d_code="86";
if psis_refyear_d_code = "2005" | psis_refyear_d_code = "2006" | psis_refyear_d_code = "2007" | psis_refyear_d_code = "2008" | psis_refyear_d_code = "2009" | psis_refyear_d_code = "2010" | psis_refyear_d_code = "2011" | psis_refyear_d_code = "2012" | psis_refyear_d_code = "2013" | psis_refyear_d_code = "2014" ;
run;


%RTRAFreq(
   InputDataset=work.psis,
   OutputName=psisfreq,
   ClassVarList=psis_refyear_d_code psis_provofstudy_code psis_prgcodecip6_d_code psis_pcsce_d_code psis_gender_code,
   UserWeight=Weight);

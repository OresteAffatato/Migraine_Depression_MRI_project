# Setting working directory

setwd("Set your own working directory")

########################### Libraries


library(data.table)


# Reading the Dataset

Dataset = fread(file = "Set your own path")

#################### Preliminary data curation

# Exclusion from the Dataset of all the participants without MRI scan
# The exclusion is going to be based on the presence of the scan of
# one brain region, Crus I, to account for the complete scanning, i.e. if
# the participant has the scan for this region it means that his brain has
# been scanned

CrusI = Dataset$`25900-2.0` + Dataset$`25901-2.0` + Dataset$`25902-2.0`

Dataset = subset(Dataset, !is.na(CrusI))

# Creating the Migraine and depression variables

Sex = Dataset$`31-0.0`

# Migraine

Migraine  = rep(0, length(Sex))

for (i in 294:552) {
  
  Migraine[Dataset[, ..i] == 	"G43"	] = 1
  Migraine[Dataset[, ..i] == 	"G430"	] = 1
  Migraine[Dataset[, ..i] == 	"G431"	] = 1
  Migraine[Dataset[, ..i] == 	"G432"	] = 1
  Migraine[Dataset[, ..i] == 	"G433"	] = 1
  Migraine[Dataset[, ..i] == 	"G438"	] = 1
  Migraine[Dataset[, ..i] == 	"G439"	] = 1
  
  
}

sum(Migraine)

# Depression

Depression  = rep(0, length(Sex))

for (i in 294:552) {
  
  Depression[Dataset[, ..i] == 	"F32"	] = 1
  Depression[Dataset[, ..i] == 	"F320"	] = 1
  Depression[Dataset[, ..i] == 	"F321"	] = 1
  Depression[Dataset[, ..i] == 	"F322"	] = 1
  Depression[Dataset[, ..i] == 	"F323"	] = 1
  Depression[Dataset[, ..i] == 	"F328"	] = 1
  Depression[Dataset[, ..i] == 	"F329"	] = 1
  Depression[Dataset[, ..i] == 	"F33"	] = 1
  Depression[Dataset[, ..i] == 	"F330"	] = 1
  Depression[Dataset[, ..i] == 	"F331"	] = 1
  Depression[Dataset[, ..i] == 	"F332"	] = 1
  Depression[Dataset[, ..i] == 	"F333"	] = 1
  Depression[Dataset[, ..i] == 	"F334"	] = 1
  Depression[Dataset[, ..i] == 	"F338"	] = 1
  Depression[Dataset[, ..i] == 	"F339"	] = 1
  
}

sum(Depression)

# Creating new diagnosis variables to have controls without both migraine
# and depression

Migraine_Final = rep(2, length(Sex))

for (i in 1:length(Sex)) {
  
  if (Migraine[i] == 0 & Depression[i] == 0) {
    
    Migraine_Final[i] = 0
    
  } else {
    
    if (Migraine[i] == 1 & Depression[i] == 1) {
      
      Migraine_Final[i] = 1
      
    } else { 
      
      if (Migraine[i] == 1 & Depression[i] == 0) {
        
        Migraine_Final[i] = 1
        
      } 
      
      
    }
  }
  
}

Depression_Final = rep(2, length(Sex))

for (i in 1:length(Sex)) {
  
  if (Migraine[i] == 0 & Depression[i] == 0) {
    
    Depression_Final[i] = 0
    
  } else {
    
    if (Migraine[i] == 1 & Depression[i] == 1) {
      
      Depression_Final[i] = 1
      
    } else { 
      
      if (Migraine[i] == 0 & Depression[i] == 1) {
        
        Depression_Final[i] = 1
        
      } 
      
      
    }
  }
  
}

table(Migraine_Final)
table(Depression_Final)

# Removing the participants with depression from the controls
# Use the following line when you want to run the analyses for the migraine group

Dataset = subset(Dataset, Migraine_Final != 2)

# Removing the participants with migraine from the controls
# Use the following line when you want to run the analyses for the depression group

Dataset = subset(Dataset, Depression_Final != 2)


#################### Creation of the covariates

# Sex

Sex = Dataset$`31-0.0`

Sex[Sex == 1] = "Male"
Sex[Sex == 0] = "Female"

Sex = as.factor(Sex)

# Age

Age = Dataset$`21003-2.0`

# UK Biobank assessment centre
  
AssessmentCentre = Dataset$`54-2.0`

AssessmentCentre = as.factor(AssessmentCentre)

# Body Mass Index

BMI = Dataset$`21001-0.0`

# Ethnic Background

EthnicBackground = Dataset$`21000-0.0`

table(EthnicBackground)

EthnicBackground = as.factor(EthnicBackground)

# Volume of brain, grey+white matter (normalised for head size)

TotalVolume = Dataset$`25009-2.0`

# Diastolic blood pressure, automated reading

Diastolic1 = Dataset$`4079-2.0`
Diastolic2 = Dataset$`4079-2.1`

DiastolicBP = (Diastolic1 + Diastolic2)/2

# Systolic blood pressure, automated reading

Systolic1 = Dataset$`4080-2.0`
Systolic2 = Dataset$`4080-2.1`

SystolicBP = (Systolic1 + Systolic2)/2

# Indexes of material deprivation at recruitment

IMDEngland = Dataset$`26410-0.0`

IMDWales = Dataset$`26426-0.0`

IMDScotland = Dataset$`26427-0.0`

IMD = IMDEngland

for (i in 1:length(IMD)) {
  if (is.na(IMD[i])) {
    IMD[i] = IMDScotland[i]
  } 
}

for (i in 1:length(IMD)) {
  if (is.na(IMD[i])) {
    IMD[i] = IMDWales[i]
  } 
}

sum(is.na(IMD))


# Smoking

Smoking = Dataset$`1239-2.0`

Smoking = as.factor(Smoking)

# Alcohol

Alcohol = Dataset$`1558-2.0`

Alcohol = as.factor(Alcohol)

##################### Diagnosis

# Viral and bacterial infections of the nervous system

ViralBacterialNSInfections  = rep(0, length(Sex))


for (i in 294:552) {
  
  ViralBacterialNSInfections[Dataset[, ..i] == "A800"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A801"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A802"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A803"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A804"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A809"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A81"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A810"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A811"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A812"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A818"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A819"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A82"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A820"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A821"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A829"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A83"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A830"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A831"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A832"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A833"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A834"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A835"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A836"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A838"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A839"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A84"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A840"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A841"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A848"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A849"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A85"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A850"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A851"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A852"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A858"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A86"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A87"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A870"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A871"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A872"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A878"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A879"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A88"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A880"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A881"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A888"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A89"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A90"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == "A91"] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G00"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G000"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G001"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G002"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G003"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G008"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G009"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G01"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G02"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G020"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G021"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G028"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G03"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G030"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G031"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G032"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G038"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G039"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G04"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G040"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G041"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G042"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G048"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G049"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G05"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G050"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G051"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G052"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G058"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G06"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G060"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G061"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G062"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G07"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G08"	] = 1
  ViralBacterialNSInfections[Dataset[, ..i] == 	"G09"	] = 1
  
}


# Diabetes

Diabetes  = rep(0, length(Sex))

for (i in 294:552) {
  Diabetes[Dataset[, ..i] == 	"E100"	] = 1
  Diabetes[Dataset[, ..i] == 	"E101"	] = 1
  Diabetes[Dataset[, ..i] == 	"E102"	] = 1
  Diabetes[Dataset[, ..i] == 	"E103"	] = 1
  Diabetes[Dataset[, ..i] == 	"E104"	] = 1
  Diabetes[Dataset[, ..i] == 	"E105"	] = 1
  Diabetes[Dataset[, ..i] == 	"E106"	] = 1
  Diabetes[Dataset[, ..i] == 	"E107"	] = 1
  Diabetes[Dataset[, ..i] == 	"E108"	] = 1
  Diabetes[Dataset[, ..i] == 	"E109"	] = 1
  Diabetes[Dataset[, ..i] == 	"E11"	] = 1
  Diabetes[Dataset[, ..i] == 	"E110"	] = 1
  Diabetes[Dataset[, ..i] == 	"E111"	] = 1
  Diabetes[Dataset[, ..i] == 	"E112"	] = 1
  Diabetes[Dataset[, ..i] == 	"E113"	] = 1
  Diabetes[Dataset[, ..i] == 	"E114"	] = 1
  Diabetes[Dataset[, ..i] == 	"E115"	] = 1
  Diabetes[Dataset[, ..i] == 	"E116"	] = 1
  Diabetes[Dataset[, ..i] == 	"E117"	] = 1
  Diabetes[Dataset[, ..i] == 	"E118"	] = 1
  Diabetes[Dataset[, ..i] == 	"E119"	] = 1
  Diabetes[Dataset[, ..i] == 	"E12"	] = 1
  Diabetes[Dataset[, ..i] == 	"E120"	] = 1
  Diabetes[Dataset[, ..i] == 	"E121"	] = 1
  Diabetes[Dataset[, ..i] == 	"E122"	] = 1
  Diabetes[Dataset[, ..i] == 	"E123"	] = 1
  Diabetes[Dataset[, ..i] == 	"E124"	] = 1
  Diabetes[Dataset[, ..i] == 	"E125"	] = 1
  Diabetes[Dataset[, ..i] == 	"E126"	] = 1
  Diabetes[Dataset[, ..i] == 	"E127"	] = 1
  Diabetes[Dataset[, ..i] == 	"E128"	] = 1
  Diabetes[Dataset[, ..i] == 	"E129"	] = 1
  Diabetes[Dataset[, ..i] == 	"E13"	] = 1
  Diabetes[Dataset[, ..i] == 	"E130"	] = 1
  Diabetes[Dataset[, ..i] == 	"E131"	] = 1
  Diabetes[Dataset[, ..i] == 	"E132"	] = 1
  Diabetes[Dataset[, ..i] == 	"E133"	] = 1
  Diabetes[Dataset[, ..i] == 	"E134"	] = 1
  Diabetes[Dataset[, ..i] == 	"E135"	] = 1
  Diabetes[Dataset[, ..i] == 	"E136"	] = 1
  Diabetes[Dataset[, ..i] == 	"E137"	] = 1
  Diabetes[Dataset[, ..i] == 	"E138"	] = 1
  Diabetes[Dataset[, ..i] == 	"E139"	] = 1
  Diabetes[Dataset[, ..i] == 	"E14"	] = 1
  Diabetes[Dataset[, ..i] == 	"E140"	] = 1
  Diabetes[Dataset[, ..i] == 	"E141"	] = 1
  Diabetes[Dataset[, ..i] == 	"E142"	] = 1
  Diabetes[Dataset[, ..i] == 	"E143"	] = 1
  Diabetes[Dataset[, ..i] == 	"E144"	] = 1
  Diabetes[Dataset[, ..i] == 	"E145"	] = 1
  Diabetes[Dataset[, ..i] == 	"E146"	] = 1
  Diabetes[Dataset[, ..i] == 	"E147"	] = 1
  Diabetes[Dataset[, ..i] == 	"E148"	] = 1
  Diabetes[Dataset[, ..i] == 	"E149"	] = 1
  
}

# Diseases of the nervous system

NSDiseases  = rep(0, length(Sex))

for (i in 294:552) {
  NSDiseases[Dataset[, ..i] == 	"G10"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G11"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G110"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G111"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G112"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G113"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G114"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G118"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G119"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G12"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G120"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G121"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G122"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G128"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G129"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G13"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G130"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G131"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G132"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G138"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G14"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G20"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G21"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G210"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G211"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G212"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G213"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G214"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G218"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G219"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G22"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G23"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G230"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G231"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G232"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G233"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G238"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G239"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G24"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G240"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G241"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G242"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G243"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G244"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G245"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G248"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G249"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G25"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G250"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G251"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G252"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G253"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G254"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G255"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G256"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G258"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G259"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G26"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G30"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G300"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G301"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G308"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G309"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G31"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G310"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G311"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G312"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G318"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G319"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G32"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G320"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G328"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G35"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G36"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G360"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G361"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G368"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G369"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G37"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G370"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G371"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G372"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G373"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G374"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G375"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G378"	] = 1
  NSDiseases[Dataset[, ..i] == 	"G379"	] = 1
  
}


# Mental and behavioural disorders due to psychoactive substances

MentalBehaviouralDisordersPsychoactiveSubstances  = rep(0, length(Sex))

for (i in 294:552) {
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F10"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F100"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F101"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F102"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F103"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F104"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F105"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F106"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F107"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F108"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F109"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F11"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F110"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F111"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F112"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F113"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F114"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F115"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F116"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F117"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F118"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F119"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F12"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F120"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F121"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F122"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F123"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F124"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F125"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F126"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F127"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F128"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F129"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F13"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F130"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F131"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F132"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F133"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F134"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F135"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F136"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F137"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F138"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F139"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F14"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F140"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F141"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F142"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F143"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F144"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F145"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F146"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F147"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F148"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F149"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F15"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F150"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F151"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F152"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F153"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F154"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F155"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F156"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F157"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F158"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F159"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F16"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F160"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F161"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F162"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F163"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F164"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F165"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F166"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F167"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F168"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F169"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F17"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F170"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F171"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F172"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F173"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F174"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F175"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F176"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F177"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F178"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F179"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F18"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F180"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F181"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F182"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F183"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F184"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F185"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F186"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F187"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F188"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F189"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F19"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F190"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F191"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F192"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F193"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F194"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F195"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F196"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F197"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F198"	] = 1
  MentalBehaviouralDisordersPsychoactiveSubstances[Dataset[, ..i] == 	"F199"	] = 1
  
}


# Psychiatric, mental, behavioural disorders

PsychiatricMentalBehaviouralDisorders_NoDepression  = rep(0, length(Sex))

for (i in 294:552) {
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F00"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F000"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F001"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F002"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F009"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F01"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F010"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F011"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F012"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F013"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F018"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F019"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F02"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F020"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F021"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F022"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F023"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F024"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F028"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F03"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F04"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F05"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F050"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F051"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F058"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F059"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F06"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F060"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F061"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F062"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F063"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F064"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F065"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F066"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F067"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F068"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F069"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F07"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F070"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F071"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F072"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F078"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F079"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F09"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F20"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F200"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F201"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F202"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F203"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F204"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F205"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F206"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F208"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F209"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F21"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F22"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F220"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F228"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F229"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F23"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F230"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F231"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F232"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F233"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F238"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F239"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F24"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F25"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F250"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F251"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F252"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F258"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F259"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F28"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F29"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F30"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F300"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F301"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F302"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F308"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F309"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F31"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F310"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F311"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F312"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F313"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F314"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F315"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F316"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F317"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F318"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F319"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F34"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F340"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F341"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F348"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F349"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F38"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F380"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F381"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F388"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F39"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F40"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F400"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F401"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F402"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F408"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F409"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F41"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F410"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F411"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F412"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F413"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F418"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F419"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F42"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F420"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F421"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F422"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F428"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F429"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F43"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F430"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F431"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F432"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F438"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F439"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F44"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F440"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F441"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F442"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F443"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F444"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F445"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F446"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F447"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F448"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F449"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F45"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F450"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F451"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F452"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F453"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F454"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F458"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F459"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F48"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F480"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F481"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F488"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F489"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F50"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F500"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F501"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F502"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F503"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F504"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F505"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F508"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F509"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F53"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F530"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F531"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F538"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F539"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F54"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F55"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F59"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F62"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F620"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F621"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F628"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F629"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F63"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F630"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F631"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F632"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F633"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F638"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F639"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F68"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F680"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F681"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F688"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F69"	] = 1
  PsychiatricMentalBehaviouralDisorders_NoDepression[Dataset[, ..i] == 	"F99"	] = 1
  
}


# Developmental disorders

DevelopmentalDisorders  = rep(0, length(Sex))

for (i in 294:552) {
  
  DevelopmentalDisorders[Dataset[, ..i] == 	"F70"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F700"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F701"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F708"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F709"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F71"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F710"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F711"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F718"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F719"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F72"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F720"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F721"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F728"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F729"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F73"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F730"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F731"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F738"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F739"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F78"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F780"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F781"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F788"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F789"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F79"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F790"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F791"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F798"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F799"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F80"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F800"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F801"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F802"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F803"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F808"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F809"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F81"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F810"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F811"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F812"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F813"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F818"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F819"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F82"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F83"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F84"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F840"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F841"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F842"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F843"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F844"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F845"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F848"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F849"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F88"	] = 1
  DevelopmentalDisorders[Dataset[, ..i] == 	"F89"	] = 1
  
}


# Epilepsy and sleep disorders

EpilepsySleepDisorders  = rep(0, length(Sex))

for (i in 294:552) {
  
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G40"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G400"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G401"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G402"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G403"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G404"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G405"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G406"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G407"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G408"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G409"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G41"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G410"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G411"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G412"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G418"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G419"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G47"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G470"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G471"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G472"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G473"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G474"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G478"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"G479"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F51"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F510"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F511"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F512"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F513"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F514"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F515"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F518"	] = 1
  EpilepsySleepDisorders[Dataset[, ..i] == 	"F519"	] = 1
  
  
}


# Muscle disorders

MuscleDisorders  = rep(0, length(Sex))

for (i in 294:552) {
  
  MuscleDisorders[Dataset[, ..i] == 	"G56"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G560"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G708"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G709"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G71"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G710"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G711"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G712"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G713"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G718"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G719"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G72"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G720"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G721"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G722"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G723"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G724"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G728"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G729"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G73"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G730"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G731"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G732"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G733"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G734"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G735"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G736"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G737"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G80"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G800"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G801"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G802"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G803"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G804"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G808"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G809"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G81"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G810"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G811"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G819"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G82"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G820"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G821"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G822"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G823"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G824"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G825"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G83"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G830"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G831"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G832"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G833"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G834"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G835"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G838"	] = 1
  MuscleDisorders[Dataset[, ..i] == 	"G839"	] = 1
  
  
}

# Headaches other than migraine

OtherHeadaches  = rep(0, length(Sex))

for (i in 294:552) {
  
  OtherHeadaches[Dataset[, ..i] == 	"G44"	] = 1
  OtherHeadaches[Dataset[, ..i] == 	"G440"	] = 1
  OtherHeadaches[Dataset[, ..i] == 	"G441"	] = 1
  OtherHeadaches[Dataset[, ..i] == 	"G442"	] = 1
  OtherHeadaches[Dataset[, ..i] == 	"G443"	] = 1
  OtherHeadaches[Dataset[, ..i] == 	"G444"	] = 1
  OtherHeadaches[Dataset[, ..i] == 	"G448"	] = 1
  
}


# Neuropathies

Neuropathies  = rep(0, length(Sex))

for (i in 294:552) {
  
  Neuropathies[Dataset[, ..i] == 	"G70"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G700"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G701"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G702"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G561"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G562"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G563"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G564"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G568"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G569"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G57"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G570"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G571"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G572"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G573"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G574"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G575"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G576"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G578"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G579"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G58"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G580"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G587"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G588"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G589"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G59"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G590"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G598"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G60"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G600"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G601"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G602"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G603"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G608"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G609"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G61"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G610"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G611"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G618"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G619"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G62"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G620"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G621"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G622"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G628"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G629"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G63"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G630"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G631"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G632"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G633"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G634"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G635"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G636"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G638"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G64"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G901"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G902"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G903"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G904"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G908"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G909"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G50"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G500"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G501"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G508"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G509"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G51"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G510"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G511"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G512"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G513"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G514"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G518"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G519"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G52"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G520"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G521"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G522"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G523"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G527"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G528"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G529"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G53"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G530"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G531"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G532"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G533"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G538"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G54"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G540"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G541"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G542"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G543"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G544"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G545"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G546"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G547"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G548"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G549"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G55"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G550"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G551"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G552"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G553"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G558"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G90"	] = 1
  Neuropathies[Dataset[, ..i] == 	"G900"	] = 1
  
  
}


# Brain and spine malformations/abnormalities

BrainSpineMalformationsAbnormalities  = rep(0, length(Sex))

for (i in 294:552) {
  
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G91"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G910"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G911"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G912"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G913"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G918"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G919"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G92"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G93"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G930"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G931"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G932"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G933"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G934"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G935"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G936"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G937"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G938"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G939"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G94"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G940"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G941"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G942"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G948"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G95"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G950"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G951"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G952"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G958"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G959"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G96"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G960"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G961"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G968"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G969"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G97"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G970"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G971"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G972"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G978"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G979"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G98"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G99"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G990"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G991"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G992"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"G998"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q00"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q000"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q001"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q002"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q01"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q010"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q011"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q012"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q018"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q019"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q02"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q03"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q030"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q031"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q038"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q039"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q04"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q040"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q041"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q042"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q043"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q044"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q045"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q046"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q048"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q049"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q05"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q050"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q051"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q052"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q053"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q054"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q055"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q056"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q057"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q058"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q059"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q06"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q060"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q061"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q062"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q063"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q064"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q068"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q069"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q07"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q070"	] = 1
  BrainSpineMalformationsAbnormalities[Dataset[, ..i] == 	"Q078"	] = 1
  
  
  
}

# Cerebrovascular diseases

CerebrovascularDiseases  = rep(0, length(Sex))

for (i in 294:552) {
  
  CerebrovascularDiseases[Dataset[, ..i] == 	"I60"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I600"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I601"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I602"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I603"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I604"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I605"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I606"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I607"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I608"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I609"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I61"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I610"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I611"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I612"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I613"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I614"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I615"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I616"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I618"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I619"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I62"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I620"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I621"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I629"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I63"	] = 1
  CerebrovascularDiseases[Dataset[, ..i] == 	"I630"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I631"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I632"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I633"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I634"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I635"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I636"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I638"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I639"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I64"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I65"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I650"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I651"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I652"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I653"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I658"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I659"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I66"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I660"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I661"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I662"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I663"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I664"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I668"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I669"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I67"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I670"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I671"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I672"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I673"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I674"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I675"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I676"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I677"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I678"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I679"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I68"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I680"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I681"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I682"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I688"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I69"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I690"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I691"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I692"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I693"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I694"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"I698"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G45"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G450"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G451"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G452"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G453"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G454"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G458"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G459"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G46"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G460"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G461"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G462"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G463"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G464"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G465"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G466"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G467"	] = 1
CerebrovascularDiseases[Dataset[, ..i] == 	"G468"	] = 1

  
}


# Head and spine injuries and fractures

HeadSpineInjuriesFractures  = rep(0, length(Sex))

for (i in 294:552) {
  
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S001"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S007"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S008"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S009"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S01"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S010"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S018"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S019"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S02"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S020"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0200"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0201"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S021"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0210"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0211"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S022"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0220"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0221"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S023"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0230"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0231"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S024"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0240"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0241"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S026"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0260"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0261"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S027"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0270"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0271"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S028"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0280"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0281"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S029"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0290"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0291"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S04"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S040"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S041"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S042"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S043"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S044"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S045"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S046"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S047"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S048"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S049"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S06"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S060"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0600"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0601"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S061"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0610"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0611"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S062"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0620"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0621"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S063"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0630"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0631"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S064"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0640"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0641"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S065"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0650"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0651"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S066"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0660"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0661"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S067"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0670"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0671"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S068"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0680"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0681"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S069"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0690"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S0691"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S07"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S070"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S071"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S078"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S079"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S08"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S080"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S081"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S088"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S089"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S09"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S090"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S091"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S092"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S097"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S098"	] = 1
  HeadSpineInjuriesFractures[Dataset[, ..i] == 	"S099"	] = 1
  
  
}


# Cardiovascular Diseases

CardiovascularDisease  = rep(0, length(Sex))

for (i in 294:552) {
  
  CardiovascularDisease[Dataset[, ..i] == 	"I00"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I01"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I010"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I011"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I012"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I018"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I019"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I02"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I020"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I029"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I05"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I050"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I051"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I052"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I058"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I059"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I06"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I060"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I061"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I062"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I068"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I069"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I07"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I070"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I071"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I072"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I078"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I079"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I08"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I080"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I081"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I082"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I083"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I088"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I089"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I09"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I090"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I091"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I092"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I098"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I099"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I10"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I11"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I110"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I119"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I12"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I120"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I129"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I13"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I130"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I131"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I132"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I139"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I15"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I150"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I151"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I152"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I158"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I159"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I20"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I200"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I201"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I208"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I209"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I21"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I210"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I211"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I212"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I213"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I214"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I219"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I21X"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I22"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I220"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I221"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I228"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I229"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I23"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I230"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I231"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I232"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I233"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I234"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I235"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I236"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I238"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I24"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I240"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I241"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I248"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I249"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I25"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I250"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I251"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I252"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I253"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I254"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I255"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I256"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I258"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I259"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I26"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I260"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I269"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I27"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I270"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I271"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I272"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I278"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I279"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I28"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I280"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I281"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I288"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I289"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I30"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I300"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I301"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I308"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I309"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I31"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I310"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I311"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I312"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I313"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I318"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I319"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I32"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I320"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I321"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I328"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I33"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I330"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I339"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I34"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I340"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I341"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I342"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I348"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I349"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I35"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I350"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I351"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I352"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I358"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I359"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I36"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I360"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I361"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I362"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I368"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I369"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I37"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I370"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I371"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I372"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I378"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I379"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I38"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I39"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I390"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I391"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I392"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I393"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I394"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I398"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I40"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I400"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I401"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I408"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I409"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I41"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I410"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I411"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I412"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I418"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I42"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I420"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I421"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I422"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I423"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I424"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I425"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I426"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I427"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I428"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I429"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I43"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I430"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I431"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I432"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I438"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I44"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I440"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I441"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I442"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I443"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I444"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I445"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I446"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I447"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I45"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I450"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I451"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I452"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I453"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I454"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I455"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I456"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I458"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I459"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I46"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I460"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I461"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I469"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I47"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I470"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I471"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I472"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I479"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I48"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I480"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I481"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I482"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I483"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I484"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I489"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I49"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I490"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I491"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I492"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I493"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I494"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I495"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I498"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I499"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I50"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I500"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I501"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I509"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I51"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I510"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I511"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I512"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I513"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I514"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I515"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I516"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I517"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I518"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I519"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I52"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I520"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I521"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I528"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I70"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I700"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7000"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7001"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I701"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7010"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7011"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I702"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7020"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7021"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I708"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7080"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7081"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I709"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7090"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I7091"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I71"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I710"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I711"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I712"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I713"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I714"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I715"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I716"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I718"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I719"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I72"	] = 1
  CardiovascularDisease[Dataset[, ..i] == 	"I720"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I721"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I722"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I723"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I724"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I725"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I726"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I728"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I729"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I73"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I730"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I731"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I738"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I739"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I74"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I740"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I741"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I742"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I743"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I744"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I745"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I748"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I749"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I77"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I770"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I771"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I772"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I773"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I774"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I775"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I776"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I778"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I779"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I78"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I780"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I781"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I788"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I789"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I79"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I790"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I791"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I792"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I798"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I80"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I800"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I801"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I802"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I803"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I808"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I809"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I81"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I82"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I820"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I821"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I822"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I823"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I828"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I829"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I83"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I830"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I831"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I832"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I839"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I84"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I840"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I841"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I842"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I843"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I844"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I845"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I846"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I847"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I848"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I849"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I85"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I850"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I859"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I86"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I860"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I861"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I862"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I863"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I864"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I868"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I87"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I870"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I871"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I872"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I878"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I879"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I88"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I880"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I881"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I888"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I889"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I89"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I890"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I891"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I898"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I899"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I95"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I950"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I951"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I952"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I958"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I959"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I97"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I970"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I971"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I972"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I978"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I979"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I98"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I980"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I981"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I982"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I983"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I988"	] = 1
CardiovascularDisease[Dataset[, ..i] == 	"I99"	] = 1

  
}

# Brain cancer

BrainCancer  = rep(0, length(Sex))

for (i in 294:552) {
  
  BrainCancer[Dataset[, ..i] == 	"C70"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C700"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C701"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C709"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C71"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C710"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C711"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C712"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C713"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C714"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C715"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C716"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C717"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C718"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C719"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C72"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C720"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C721"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C722"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C723"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C724"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C725"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C728"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C729"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C73"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C74"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C740"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C741"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C749"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C75"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C750"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C751"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C752"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C753"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C754"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C755"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C758"	] = 1
  BrainCancer[Dataset[, ..i] == 	"C759"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D32"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D320"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D321"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D329"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D33"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D330"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D331"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D332"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D333"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D334"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D337"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D339"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D43"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D430"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D431"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D432"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D433"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D434"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D437"	] = 1
  BrainCancer[Dataset[, ..i] == 	"D439"	] = 1
  
  
}


# Comorbidities

Comorbidities = ViralBacterialNSInfections + Diabetes + NSDiseases + MentalBehaviouralDisordersPsychoactiveSubstances+ PsychiatricMentalBehaviouralDisorders_NoDepression + DevelopmentalDisorders + EpilepsySleepDisorders + MuscleDisorders + OtherHeadaches + Neuropathies + BrainSpineMalformationsAbnormalities + CerebrovascularDiseases + HeadSpineInjuriesFractures + CardiovascularDisease + BrainCancer

table(Comorbidities)

Comorbidities = as.factor(Comorbidities)

# Migraine

Migraine  = rep(0, length(Sex))

for (i in 294:552) {
  
  Migraine[Dataset[, ..i] == 	"G43"	] = 1
  Migraine[Dataset[, ..i] == 	"G430"	] = 1
  Migraine[Dataset[, ..i] == 	"G431"	] = 1
  Migraine[Dataset[, ..i] == 	"G432"	] = 1
  Migraine[Dataset[, ..i] == 	"G433"	] = 1
  Migraine[Dataset[, ..i] == 	"G438"	] = 1
  Migraine[Dataset[, ..i] == 	"G439"	] = 1
  
  
}

sum(Migraine)

Migraine = as.factor(Migraine)

# Depression

Depression  = rep(0, length(Sex))

for (i in 294:552) {
  
  Depression[Dataset[, ..i] == 	"F32"	] = 1
  Depression[Dataset[, ..i] == 	"F320"	] = 1
  Depression[Dataset[, ..i] == 	"F321"	] = 1
  Depression[Dataset[, ..i] == 	"F322"	] = 1
  Depression[Dataset[, ..i] == 	"F323"	] = 1
  Depression[Dataset[, ..i] == 	"F328"	] = 1
  Depression[Dataset[, ..i] == 	"F329"	] = 1
  Depression[Dataset[, ..i] == 	"F33"	] = 1
  Depression[Dataset[, ..i] == 	"F330"	] = 1
  Depression[Dataset[, ..i] == 	"F331"	] = 1
  Depression[Dataset[, ..i] == 	"F332"	] = 1
  Depression[Dataset[, ..i] == 	"F333"	] = 1
  Depression[Dataset[, ..i] == 	"F334"	] = 1
  Depression[Dataset[, ..i] == 	"F338"	] = 1
  Depression[Dataset[, ..i] == 	"F339"	] = 1
  
}

sum(Depression)

Depression = as.factor(Depression)

################### Brain Regions

######## Gray matter volumes - FAST segmentation tool

# Thalamus

ThalamusGM = Dataset$`25878-2.0` + Dataset$`25879-2.0`

# Caudate 

CaudateGM = Dataset$`25880-2.0` + Dataset$`25881-2.0`

# Putamen

PutamenGM = Dataset$`25882-2.0` + Dataset$`25883-2.0`

# Pallidum

PallidumGM = Dataset$`25884-2.0` + Dataset$`25885-2.0`

# Hippocampus

HippocampusGM = Dataset$`25886-2.0` + Dataset$`25887-2.0`

# Amygdala

AmygdalaGM = Dataset$`25888-2.0` + Dataset$`25889-2.0`

####### Overall volumes - FIRST segmentation tool

# Thalamus

Thalamus = Dataset$`25011-2.0` + Dataset$`25012-2.0`

# Caudate 

Caudate = Dataset$`25013-2.0` + Dataset$`25014-2.0`

# Putamen

Putamen = Dataset$`25015-2.0` + Dataset$`25016-2.0`

# Pallidum

Pallidum = Dataset$`25017-2.0` + Dataset$`25018-2.0`

# Hippocampus

Hippocampus = Dataset$`25019-2.0` + Dataset$`25020-2.0`

# Amygdala

Amygdala = Dataset$`25021-2.0` + Dataset$`25022-2.0`

# Nucleus Accumbens

Accumbens = Dataset$`25023-2.0` + Dataset$`25024-2.0`


################## Analyses

# Descriptive statistics

# Creating functions to calculate mean and standard deviation, skipping NA values

Mean_NA = function(x) {
  
  mean(x, na.rm = TRUE)
}

SD_NA = function(x) {
  
  sd(x, na.rm = TRUE)
}

############## Descriptive statistics for the migraine group

# Sex

tapply(Sex, Depression, table)

# Age - mean and standard deviations

tapply(Age, Depression, mean)
tapply(Age, Depression, sd)

hist(Age)

# BMI - mean and standard deviations

tapply(BMI, Depression, Mean_NA)
tapply(BMI, Depression, SD_NA)

hist(BMI)

# Ethnic background

tapply(EthnicBackground, Depression, table)

# IMD - mean and standard deviations

tapply(IMD, Depression, summary)

hist(IMD)

# Smoking

tapply(Smoking, Depression, table)

# Alcohol 

tapply(Alcohol, Depression, table)

# Descriptives of the brain regions (overall)

tapply(Thalamus, Depression, summary)
tapply(Thalamus, Depression, SD_NA)

tapply(Caudate, Depression, summary)
tapply(Caudate, Depression, SD_NA)

tapply(Putamen, Depression, summary)
tapply(Putamen, Depression, SD_NA)

tapply(Pallidum, Depression, summary)
tapply(Pallidum, Depression, SD_NA)

tapply(Hippocampus, Depression, summary)
tapply(Hippocampus, Depression, SD_NA)

tapply(Amygdala, Depression, summary)
tapply(Amygdala, Depression, SD_NA)

tapply(Accumbens, Depression, summary)
tapply(Accumbens, Depression, SD_NA)

# Descriptives of the brain regions (gray matter)

tapply(ThalamusGM, Depression, summary)
tapply(ThalamusGM, Depression, SD_NA)

tapply(CaudateGM, Depression, summary)
tapply(CaudateGM, Depression, SD_NA)

tapply(PutamenGM, Depression, summary)
tapply(PutamenGM, Depression, SD_NA)

tapply(PallidumGM, Depression, summary)
tapply(PallidumGM, Depression, SD_NA)

tapply(HippocampusGM, Depression, summary)
tapply(HippocampusGM, Depression, SD_NA)

tapply(AmygdalaGM, Depression, summary)
tapply(AmygdalaGM, Depression, SD_NA)


# Generalized linear model - Overall brain volumes

# Thalamus

Thalamus.glm = glm(Thalamus ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)


summary(Thalamus.glm)

tvalue = -0.283
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Thalamus.glm)

# Caudate

Caudate.glm = glm(Caudate ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(Caudate.glm)

tvalue = 0.756
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Caudate.glm)

# Putamen

Putamen.glm = glm(Putamen ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(Putamen.glm)

tvalue = 1.703
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Putamen.glm)

# Pallidum

Pallidum.glm = glm(Pallidum ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(Pallidum.glm)

tvalue = 0.187
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Pallidum.glm)

# Hippocampus

Hippocampus.glm = glm(Hippocampus ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(Hippocampus.glm)

tvalue = -0.744
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Hippocampus.glm)

# Amygdala

Amygdala.glm = glm(Amygdala ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(Amygdala.glm)

tvalue = 1.408
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Amygdala.glm)

# Nucleus Accumbens

Accumbens.glm = glm(Accumbens ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(Accumbens.glm)

tvalue = -0.440
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(Accumbens.glm)

# Generalized linear model - Gray matter volumes

# ThalamusGM

ThalamusGM.glm = glm(ThalamusGM ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(ThalamusGM.glm)

tvalue = 0.350
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(ThalamusGM.glm)

# CaudateGM

CaudateGM.glm = glm(CaudateGM ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(CaudateGM.glm)

tvalue = 0.368
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(CaudateGM.glm)

# PutamenGM

PutamenGM.glm = glm(PutamenGM ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(PutamenGM.glm)

tvalue = 2.040
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(PutamenGM.glm)

# PallidumGM

PallidumGM.glm = glm(PallidumGM ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(PallidumGM.glm)

tvalue = -0.518
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(PallidumGM.glm)

# HippocampusGM

HippocampusGM.glm = glm(HippocampusGM ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(HippocampusGM.glm)

tvalue = -1.278
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(HippocampusGM.glm)

# AmygdalaGM

AmygdalaGM.glm = glm(AmygdalaGM ~ Depression + Sex + Age + BMI + Alcohol + Smoking + DiastolicBP + IMD + AssessmentCentre + TotalVolume + EthnicBackground + Comorbidities, data = Dataset)

summary(AmygdalaGM.glm)

tvalue = -1.795
df = 33425

CohenD = (tvalue*(712+43930))/(sqrt(712*43930)*sqrt(df))
CohenD

confint(AmygdalaGM.glm)

# Plotting the results

# Overall brain volumes

Brain_Regions = c("Thalamus", "Caudate", "Putamen", "Pallidum", "Hippocampus", "Amygdala", "Nucleus Accumbens")

Means = c(103, 66, 45, 11, -9, -8, 2)

Lower_Bound = c(-2, -3, -38, -27, -79, -45, -14)

Upper_Bound =  c(208, 135, 129, 49, 61, 27, 17)

Group = c("Migraine", "Migraine","Migraine","Migraine","Migraine","Migraine","Migraine")

Plot_results_Migraine = data.frame(Brain_Regions, Means, Lower_Bound, Upper_Bound, Group)

Means = c(-10, 17, 47, 2, -17, 17, -2)

Lower_Bound = c(-77, -27, -7, -22, -61, -7, -12)

Upper_Bound =  c(58, 61, 100, 26, 28, 40, 8)

Group = c("Depression", "Depression","Depression","Depression","Depression","Depression","Depression")

Plot_results_Depression = data.frame(Brain_Regions, Means, Lower_Bound, Upper_Bound, Group)

Plot_results = rbind(Plot_results_Migraine, Plot_results_Depression)



install.packages('ggplot2')
library(ggplot2)


# Creating the forest plot

Forest_plot = ggplot(data=Plot_results,
                     aes(y = Plot_results$Brain_Regions, x =Plot_results$Means, xmin = Plot_results$Lower_Bound, xmax = Plot_results$Upper_Bound, col = Group))+
  xlab('Mean difference (95% Confidence Interval)')+ ylab("Subcortical Regions (Overall volumes)")+
  geom_point(aes(x=Means), shape=15, size=3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin=Plot_results$Lower_Bound, xmax=Plot_results$Upper_Bound),width=0.2,cex=1, position = position_dodge(width = 0.5)) +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"), 
        panel.background = element_blank(),
        panel.grid.major.x = element_line(size = 0.5, color = "black", linetype = "dashed"),
        axis.line = element_line(size = 0.6, colour = "black")) +
  geom_vline(xintercept=0, linetype="dashed", color = "red")

# Printing the plot
Forest_plot

# Gray matter volumes

Brain_Regions = c("Thalamus", "Caudate", "Putamen", "Pallidum", "Hippocampus", "Amygdala")

Means = c(8, -13, 45, -1, -10, 8)

Lower_Bound = c(-40, -134, -28, -8, -75, -28)

Upper_Bound =  c(56, 108, 118, 6, 55, 44)

Group = c("Migraine", "Migraine","Migraine","Migraine","Migraine","Migraine")

Plot_results_Migraine = data.frame(Brain_Regions, Means, Lower_Bound, Upper_Bound, Group)

Means = c(5, 15, 49, -1, -27, -21)

Lower_Bound = c(-25, -63, 2, -6, -69, -44)

Upper_Bound =  c(36, 92, 95, 3, 15, 2)

Group = c("Depression", "Depression","Depression","Depression","Depression","Depression")

Plot_results_Depression = data.frame(Brain_Regions, Means, Lower_Bound, Upper_Bound, Group)

Plot_results = rbind(Plot_results_Migraine, Plot_results_Depression)


# Creating the forest plot

Forest_plot = ggplot(data=Plot_results,
                     aes(y = Plot_results$Brain_Regions, x =Plot_results$Means, xmin = Plot_results$Lower_Bound, xmax = Plot_results$Upper_Bound, col = Group))+
  xlab('Mean difference (95% Confidence Interval)')+ ylab("Subcortical Regions (Gray matter volumes)")+
  geom_point(aes(x=Means), shape=15, size=3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin=Plot_results$Lower_Bound, xmax=Plot_results$Upper_Bound),width=0.2,cex=1, position = position_dodge(width = 0.5)) +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"), 
        panel.background = element_blank(),
        panel.grid.major.x = element_line(size = 0.5, color = "black", linetype = "dashed"),
        axis.line = element_line(size = 0.6, colour = "black")) +
  geom_vline(xintercept=0, linetype="dashed", color = "red")

# Printing the plot
Forest_plot

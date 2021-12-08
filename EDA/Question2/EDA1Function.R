
library(dplyr)
library(stringr)
library(tidytext)
library(gsubfn)

generate_dataset <- function(injuries){  #takes in a dataset
  
  injuries <- read.csv("../Data/all_injuries_clean.csv")
  injuries <- injuries %>% mutate(injury_types = tolower(injury_types))
  
  concussion <- sum(str_detect(injuries$injury_types, pattern = "concussion"))
  head <- sum(str_detect(injuries$injury_types, pattern = "head"))
  neck <- sum(str_detect(injuries$injury_types, pattern = "neck"))
  eye <- sum(str_detect(injuries$injury_types, pattern = "eye"))
  ear <- sum(str_detect(injuries$injury_types, pattern = "ear"))
  chin <- sum(str_detect(injuries$injury_types, pattern = "chin"))
  migraine <- sum(str_detect(injuries$injury_types, pattern = "migraine|migraines"))
  jaw <- sum(str_detect(injuries$injury_types, pattern = "jaw"))
  nose <- sum(str_detect(injuries$injury_types, pattern = "nose"))
  tooth <- sum(str_detect(injuries$injury_types, pattern = "tooth"))
  stinger <- sum(str_detect(injuries$injury_types, pattern = "stinger"))
  faciallaceration <- sum(str_detect(injuries$injury_types, pattern = "faciallaceration"))
  eyelid <- sum(str_detect(injuries$injury_types, pattern = "eyelid"))
  facial <- sum(str_detect(injuries$injury_types, pattern = "facial"))
  mouth <- sum(str_detect(injuries$injury_types, pattern = "mouth"))
  nose <- sum(str_detect(injuries$injury_types, pattern = "nose"))
  throat <- sum(str_detect(injuries$injury_types, pattern = "throat"))
  seizure <- sum(str_detect(injuries$injury_types, pattern = "seizure"))
  
  
  
  individual_head <- c(concussion, head, neck, eye, ear, chin, migraine, jaw, nose, tooth, stinger, faciallaceration, eyelid, facial, mouth, nose, throat, seizure)
  total_head <- concussion + head + neck + eye + ear + chin + migraine + jaw + nose + tooth + stinger + faciallaceration + eyelid + facial + mouth + nose + throat + seizure
  shoulder <- sum(str_detect(injuries$injury_types, pattern = "shoulder|leftshoulder|jointshoulder|rightshoulder"))
  clavicle <- sum(str_detect(injuries$injury_types, pattern = "clavicle"))
  collarbone <- sum(str_detect(injuries$injury_types, pattern = "collarbone"))
  scapula <- sum(str_detect(injuries$injury_types, pattern = "scapula"))
  sternoclavicular <- sum(str_detect(injuries$injury_types, pattern = "sternoclavicular"))
  
  
  individual_shoulder <- c(shoulder, clavicle, collarbone, scapula, sternoclavicular)
  total_shoulder <- shoulder + clavicle + collarbone + scapula + sternoclavicular
  rib <- sum(str_detect(injuries$injury_types, pattern = "rib|ribs"))
  back <- sum(str_detect(injuries$injury_types, pattern = "back"))
  pectoral <- sum(str_detect(injuries$injury_types, pattern = "pectoral|pec"))
  chest <- sum(str_detect(injuries$injury_types, pattern = "chest"))
  oblique <- sum(str_detect(injuries$injury_types, pattern = "oblique"))
  lumbar <- sum(str_detect(injuries$injury_types, pattern = "lumbar"))
  core <- sum(str_detect(injuries$injury_types, pattern = "core"))
  spine <- sum(str_detect(injuries$injury_types, pattern = "spine"))
  trapezius <- sum(str_detect(injuries$injury_types, pattern = "trapezius"))
  kidney <- sum(str_detect(injuries$injury_types, pattern = "kidney"))
  lung <- sum(str_detect(injuries$injury_types, pattern = "lung"))
  heart <- sum(str_detect(injuries$injury_types, pattern = "heart"))
  abdominal <- sum(str_detect(injuries$injury_types, pattern = "abdomen|abdominal"))
  solarplexus <- sum(str_detect(injuries$injury_types, pattern = "solarplexus"))
  spleen <- sum(str_detect(injuries$injury_types, pattern = "spleen"))
  hernia <- sum(str_detect(injuries$injury_types, pattern = "hernia"))
  arrhythmia <- sum(str_detect(injuries$injury_types, pattern = "arrhythmia"))
  liver <- sum(str_detect(injuries$injury_types, pattern = "liver"))
  stomach <- sum(str_detect(injuries$injury_types, pattern = "stomach"))
  appendix <- sum(str_detect(injuries$injury_types, pattern = "appendix"))
  
  
  total_uppertorso <- rib + back + pectoral + chest + oblique + lumbar + core + spine + trapezius + kidney + lung + heart + abdominal + solarplexus + spleen + hernia + arrhythmia + liver + stomach + appendix
  individual_uppertorso <- c(rib, back, pectoral, chest, oblique, lumbar, core, spine, trapezius, kidney, lung, heart, abdominal, solarplexus, spleen, hernia, arrhythmia, liver, stomach, appendix)
  
  glute <- sum(str_detect(injuries$injury_types, pattern = "glute|buttocks"))
  hip <- sum(str_detect(injuries$injury_types, pattern = "hip|righthip|lefthip"))
  pelvis <- sum(str_detect(injuries$injury_types, pattern = "pelvis"))
  tailbone <- sum(str_detect(injuries$injury_types, pattern = "tailbone"))
  
  
  total_lowertorso <- glute + hip + pelvis + tailbone
  individual_lowertorso <- c(glute, hip, pelvis, tailbone)
  
  arm <- sum(str_detect(injuries$injury_types, pattern = "arm|forearm|rightupperarm"))
  elbow <- sum(str_detect(injuries$injury_types, pattern = "elbow|rightelbow|leftelbow"))
  tricep <- sum(str_detect(injuries$injury_types, pattern = "tricep|triceps"))
  bicep <- sum(str_detect(injuries$injury_types, pattern = "bicep|biceps"))
  
  total_arm <- arm + elbow + tricep + bicep
  individual_arm <- c(arm, elbow, tricep, bicep)
  
  
  hand <- sum(str_detect(injuries$injury_types, pattern = "righthand|lefthand|hand"))
  thumb <- sum(str_detect(injuries$injury_types, pattern = "thumb|rightthumb|leftthumb"))
  finger <- sum(str_detect(injuries$injury_types, pattern = "finger|rightfinger|leftfinger"))
  wrist <- sum(str_detect(injuries$injury_types, pattern = "wrist|rightwrist|leftwrist"))
  
  total_hand <- hand + thumb + finger + wrist
  individual_hand <- c(hand, thumb, finger, wrist)
  
  hamstring <- sum(str_detect(injuries$injury_types, pattern = "hamstring|righthamstring|lefthamstring"))
  knee <- sum(str_detect(injuries$injury_types, pattern = "knee|rightknee|leftknee|acl|mcl|meniscus|bothknees"))
  thigh <- sum(str_detect(injuries$injury_types, pattern = "thigh|rightthigh|leftthigh"))
  calf <- sum(str_detect(injuries$injury_types, pattern = "calf|leftcalf|rightcalf"))
  quadricep <- sum(str_detect(injuries$injury_types, pattern = "quadricep|leftquadricep|rightquadricep|quad"))
  groin <- sum(str_detect(injuries$injury_types, pattern = "groin|rightgroin|leftgroin"))
  lowerleg <- sum(str_detect(injuries$injury_types, pattern = "tibia|fibula|lowerleg|shin|rightshin|leftshin"))
  adductor <- sum(str_detect(injuries$injury_types, pattern = "adductor"))
  contusion <- sum(str_detect(injuries$injury_types, pattern = "contusion"))
  
  
  
  total_leg <- hamstring + knee + thigh + calf + quadricep + groin + lowerleg + adductor + contusion
  individual_leg <- c(hamstring, knee, thigh, calf, quadricep, groin, lowerleg, adductor, contusion)
  
  foot <- sum(str_detect(injuries$injury_types, pattern = "foot|rightfoot|leftfoot"))
  ankle <- sum(str_detect(injuries$injury_types, pattern = "ankle|rightankle|leftankle"))
  achilles <- sum(str_detect(injuries$injury_types, pattern = "achilles"))
  toe <- sum(str_detect(injuries$injury_types, pattern = "toe|toes"))
  heel <- sum(str_detect(injuries$injury_types, pattern = "heel"))
  
  total_foot <- foot + ankle + achilles + toe + heel
  individual_foot <- c(foot, ankle, achilles, toe, heel)
  
  injurytype <- c("concussion", "head", "neck", "eye", 'ear', 'chin', 'migraine', 'jaw', 'nose', 'tooth', "stinger", "faciallaceration", "eyelid", "facial", "mouth", "nose", "throat", "seizure",
                  "shoulder", "clavicle", "collarbone", "scapula", "sternoclavicular",
                  'rib', 'back', 'pectoral', 'chest', 'oblique', 'lumbar', "core", "spine", 'trapezius', 'kidney', 'lung', 'heart', 'abdominal', 'solarplexus', 'spleen', 'hernia', "arrhythmia", "liver", "stomach", "appendix",
                  'glute', 'hip', 'pelvis', "tailbone",
                  'arm', 'elbow', 'tricep', "bicep",
                  'hand', 'thumb', 'finger', 'wrist',
                  'hamstring', 'knee', 'thigh', 'calf', 'quadricep',  'groin', 'lowerleg', 'adductor', "contusion",
                  'foot', 'ankle', 'achilles', 'toe', 'heel')
  
  count <- c(concussion, head, neck, eye, ear, chin, migraine, jaw, nose, tooth, stinger, faciallaceration, eyelid, facial, mouth, nose, throat, seizure, 
             shoulder, clavicle, collarbone, scapula, sternoclavicular,
             rib, back, pectoral, chest, oblique, lumbar, core, spine, trapezius, kidney, lung, heart, abdominal, solarplexus, spleen, hernia, arrhythmia, liver, stomach, appendix,
             glute, hip, pelvis, tailbone,
             arm, elbow, tricep, bicep,
             hand, thumb, finger, wrist,
             hamstring, knee, thigh, calf, quadricep, groin, lowerleg, adductor, contusion,
             foot, ankle, achilles, toe, heel)
  
  bodypart <- c(rep("Head", length(individual_head)), 
                rep("Shoulder", length(individual_shoulder)), 
                rep("Upper Torso", length(individual_uppertorso)),
                rep("Lower Torso", length(individual_lowertorso)),
                rep("Arm", length(individual_arm)),
                rep("Hand", length(individual_hand)),
                rep("Leg", length(individual_leg)),
                rep("Foot", length(individual_foot))
  )
  
  injury_count <- data.frame(injurytype, count, bodypart)
}
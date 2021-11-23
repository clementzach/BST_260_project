library(dplyr)
library(stringr)

team_key <- c("buf" = "Buffalo Bills", 
              "nwe" = "New England Patriots", 
              "nyj" = "New York Jets",
              "oti" = "Tennessee Titans",
              "clt" = "Indianapolis Colts",
              "jax" = "Jacksonville Jaguars",
              "htx" = "Houston Texans",
              "rav" = "Baltimore Ravens",
              "pit" = "Pittsburgh Steelers",
              "cin" = "Cincinnati Bengals",
              "cle" = "Cleveland Browns",
              "rai" = "Las Vegas Raiders",
              "sdg" = "Los Angeles Chargers",
              "den" = "Denver Broncos",
              "mia" = "Miami Dolphins",
              "kan" = "Kansas City Chiefs",
              "dal" = "Dallas Cowboys",
              "nyg" = "New York Giants",
              "phi" = "Philadelphia Eagles",
              "was" = "Washington Football Team",
              "tam" = "Tampa Bay Buccaneers",
              "nor" = "New Orleans Saints",
              "atl" = "Atlanta Falcons",
              "car" = "Carolina Panthers",
              "gnb" = "Green Bay Packers",
              "min" = "Minnesota Vikings",
              "chi" = "Chicago Bears",
              "det" = "Detroit Lions",
              "crd" = "Arizona Cardinals",
              "ram" = "Los Angeles Rams",
              "sea" = "Seattle Seahawks",
              "sfo" = "San Francisco 49ers"
)


injuries <- read.csv("injuries_2009_2021.csv")

injuries <- injuries %>% mutate(name = sapply(Player, function(x){full = str_split(x, ",")[[1]]; return(paste(full[2], full[1]) %>% str_trim())}),
                                injury_types = tolower(injury_types), #easier for string processing if these are all consistent
                                team = str_trim(team),
                                full_team = team_key[team]) #not sure if there's whitespace around these

upper_duplicates <- which(duplicated(injuries[,c('name', "year", "team")]))

injuries <- injuries[-upper_duplicates,] #get rid of higher index



##Now start Willow/Lauren work

# Replace any "/" with " " ?
injuries$injury_types <- gsub("/", " ", injuries$injury_types)
 

## Body part vectors
 
Head <- c("concussion", "head", "neck", "eye", "ear", "chin", "migranes", "migrane", "jaw", "nose", "tooth", "stinger", "faciallaceration", "eyelid", "facial", "mouth", "nose", "throat", "seizure")

Shoulder <- c("shoulder", "clavicle", "collarbone", "leftshoulder", "rightshoulder", "jointshoulder", "scapula", "sternoclavicular")

UpperTorso <- c("rib", "back", "pectoral", "pec", "chest", "oblique", "lumbar", "core", "spine", "trapezius", "kidney", "lung", "heart", "abdomen", "abdominal", "solarplexus", "spleen", "hernia", "arrhythmia", "liver", "stomach", "appendix")

LowerTorso <- c("glute", "buttocks", "hip", "righthip", "pelvis", "tailbone")


Arm <- c("arm", "rightupperarm", "forearm", "bicep", "biceps", "elbow", "rightelbow", "tricep", "triceps")

Hand <- c("hand", "righthand", "thumb", "rightthumb", "finger", "rightfinger", "wrist", "rightwrist")

Leg <- c("hamstring", "righthamstring", "knee", "rightknee", "acl", "mcl", "meniscus", "bothknees", "thigh", "rightthigh", "calf", "rightcalf", "quadricep", "rightquadricep", "quad", "groin", "rightgroin", "lowerleg", "tibia", "fibula", "shin", "rightshin", "adductor", "contusion")

Foot <- c("foot", "rightfoot", "ankle", "achilles", "toe", "toes", "heel")


# Function to count broad body part injuries 
 
# Bodypart input must be a string
broad <- function(bodylist, bodypart) {
  
  pasted <- paste(bodylist, collapse = "|")
  injuries$broad_injury <- gsub(pasted, bodypart, injuries$injury_types)
  str_count(injuries$broad_injury, pattern = bodypart)
  
}
 

 
# Head
head_counts <- broad(Head, "head")

# Shoulder
shoulder_counts <- broad(Shoulder, "shoulder")

# Upper Torso
uppertorso_counts <- broad(UpperTorso, "uppertorso")

# Lower Torso
lowertorso_counts <- broad(LowerTorso, "lowertorso")

# Arm
arm_counts <- broad(Arm, "arm")

# Hand
hand_counts <- broad(Hand, "hand")

# Leg
leg_counts <- broad(Leg, "leg")

# Foot
foot_counts <- broad(Foot, "foot")

injuries <- injuries %>% 
  mutate(head = head_counts) %>%
  mutate(shoulder = shoulder_counts) %>%
  mutate(upper_torso = uppertorso_counts) %>%
  mutate(lower_torso = lowertorso_counts) %>%
  mutate(arm = arm_counts) %>%
  mutate(hand = hand_counts) %>%
  mutate(leg = leg_counts) %>%
  mutate(foot = foot_counts)

 
write.csv(injuries,"../all_injuries_clean.csv", row.names = TRUE)
 


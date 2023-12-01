
#'  Return a Factored Version of the MESA Dataset
#'
#' @import dplyr magrittr

make_factored_mesa <- function(mesa) {

  # born in the US  - Mother
  # 1. One of the 50 US states 
  # 2. Puerto Rico 
  # 3. Another country
  mesa$mbth1 %<>% factor(., labels = c("Yes", "Yes", "No"))

  # born in the US  - Father
  # 1. One of the 50 US states 
  # 2. Puerto Rico 
  # 3. Another country
  mesa$fbth1 %<>% factor(., labels = c("Yes", "Yes", "No"))

  # mother education level 
  mesa$momschl2 %<>% factor(labels = c(
    "No Schooling",
    "Some Schooling,\nDid Not Complete H.S.",
    "High School Degree",
    "Some College",
    "College Degree",
    "Graduate Degree"
  ))

  # father education level 
  mesa$dadschl2 %<>% factor(labels = c(
    "No Schooling",
    "Some Schooling,\nDid Not Complete H.S.",
    "High School Degree",
    "Some College",
    "College Degree",
    "Graduate Degree"
  ))

  mesa$smkstat5 %<>% factor(., levels = 0:4,
    labels = c("0: NEVER SMOKED",
      "1: FORMER SMOKER QUIT MORE THAN 1 YEAR AGO",
      "2: FORMER SMOKER QUIT LESS THAN 1 YEAR AGO",
      "3: CURRENT SMOKER",
      "4: DO NOT KNOW"))

  mesa$race1c %<>% factor(.,
        levels = attr(., 'labels'), 
        labels = c("White N.H.", 'Chinese-American', 'Black N.H.', 'Hispanic'))

  mesa$race1c %<>% forcats::fct_relevel("Black N.H.")

  mesa$race1c %<>% forcats::fct_drop()

  mesa$gender1 %<>% factor(.,
        levels = attr(., 'labels'), 
        labels = c("Female", "Male"))

  mesa$gender1 %<>% relevel("Female")

  mesa$educ1 %<>% factor(.,
    labels = 
  c("No Schooling", 
    "Grades 1-8", 
    "Grades 9-11", 
    "Completed High School/GED", 
    "Some College but No Degree", 
    "Technical School Certificate", 
    "Associate Degree", 
    "Bachelor's Degree", 
    "Graduate or Professional School"
  ))

  mesa$diabet1 %<>% factor(., 
    labels = c("No", "Yes")) %>% 
    forcats::fct_relevel("Yes")

  # insulin or hypoglycemics for diabetes 
  mesa$diabins5 %<>% factor(labels = c("No", "Yes", "Don't know"))

  mesa$htn5c %<>% factor(.,
    labels = c("No", "Yes")) %>% 
    forcats::fct_relevel("Yes")

  # hypertension medication
  mesa$htnmed5c %<>% factor(labels = c("No", "Yes"))
  
  # home type 
  mesa$hometyp1 %<>% factor(labels = c(
    'Rent',
    'Pay a Mortgage',
    'Own Free and Clear',
    'Other'
  ))

  mesa$income5 %<>% factor(.,
  labels = c(
    "< $5000",
    "$5000 - $7999",
    "$8000 - $11999",
    "$12000 - $15999",
    "$16000 - $19999",
    "$20000 - $24999",
    "$25000 - $29999",
    "$30000 - $34999",
    "$35000 - $39999",
    "$40000 - $49999",
    "$50000 - $74999",
    "$75000 - $99999",
    "$100,000 - $124,99",
    "$125,000 - $149,999",
    "$150,000 or more"))

  mesa$curjob1 %<>% factor(., labels = names(attr(mesa$curjob1, 'labels')))

  mesa$metsyn5c %<>% factor(., labels = c("No", "Yes"))

  return(mesa)
}

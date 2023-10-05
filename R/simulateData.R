# simulate data based on the original dataset

#' Create a jumbled version of the dataset with rows sampled from any row within each coloumn 
#' @param n number of rows for the new dataset 
#'
#' @return tibble 
#' @export
#'
#' @examples
#' new_data<-simulate_data(n=100)
simulateData<-function(n){
  library(tidyverse)
  Hjerkinn_Salix_Data <- read_excel(paste0(here::here(), "/data/Hjerkinn Salix Monitoring Data - copy for analysis.xlsx"), sheet = "Willows_Details")
  
  #Hjerkinn_Salix_Data |> str()
  Hjerkinn_Salix_Data$Planting_Site_No<-as.factor(Hjerkinn_Salix_Data$Planting_Site_No)
  Hjerkinn_Salix_Data$`Catkins_and_buds_present?_(y=1,n=0)`<-as.factor(Hjerkinn_Salix_Data$`Catkins_and_buds_present?_(y=1,n=0)`)
  Hjerkinn_Salix_Data$`Mammal_damage_(0=none/negligable,1= minor,2= significant,3= severe)`<-as.factor(Hjerkinn_Salix_Data$`Mammal_damage_(0=none/negligable,1= minor,2= significant,3= severe)`)
  Hjerkinn_Salix_Data$`Insect_damage_(0=none/negligable,1= minor,2= significant,3= severe)`<-as.factor(Hjerkinn_Salix_Data$`Insect_damage_(0=none/negligable,1= minor,2= significant,3= severe)`)
  Hjerkinn_Salix_Data$`Galls_present_(y=1/n=0)`<-as.factor(Hjerkinn_Salix_Data$`Galls_present_(y=1/n=0)`)
  Hjerkinn_Salix_Data$`Winter_dieback?_(y=1,n=0)`<-as.factor(Hjerkinn_Salix_Data$`Winter_dieback?_(y=1,n=0)`)
  Hjerkinn_Salix_Data$`Rust_present?_(y=1/n=0)`<-as.factor(Hjerkinn_Salix_Data$`Rust_present?_(y=1/n=0)`)
  Hjerkinn_Salix_Data$`Early_senescence?_(y=1/n=0)`<-as.factor(Hjerkinn_Salix_Data$`Early_senescence?_(y=1/n=0)`)
  Hjerkinn_Salix_Data$`Dung_or_signs_present_in_Vacinity_(y=1/n=0)`<-as.factor(Hjerkinn_Salix_Data$`Dung_or_signs_present_in_Vacinity_(y=1/n=0)`)
  Hjerkinn_Salix_Data |> str()
  
  Planting_Site_No<-sample(Hjerkinn_Salix_Data$Planting_Site_No, n, replace=TRUE)
  Unit_No<-sample(Hjerkinn_Salix_Data$Unit_No, n, replace=TRUE)                                                            
  Year_planted<-sample(Hjerkinn_Salix_Data$Year_planted, n, replace=TRUE)                                                       
  Salix_number<-sample(Hjerkinn_Salix_Data$Salix_number, n, replace=TRUE)                                                       
  Species<-sample(Hjerkinn_Salix_Data$Species, n, replace=TRUE)                                                            
  Health_Status<-sample(Hjerkinn_Salix_Data$Health_Status, n, replace=TRUE)                                                      
  Max_diameter<-sample(Hjerkinn_Salix_Data$`Max_diameter_(cm)`, n, replace=TRUE)                                                  
  Perpendicular_Diameter<-sample(Hjerkinn_Salix_Data$`Perpendicular_Diameter_(cm)`, n, replace=TRUE)                                        
  Max_Height<-sample(Hjerkinn_Salix_Data$`Max_Height_(cm)`, n, replace=TRUE)                                                    
  Max_Canopy_volume<-sample(Hjerkinn_Salix_Data$`Max_Canopy_volume_(cm^3)`, n, replace=TRUE)                                           
  No_of_main_branches<-sample(Hjerkinn_Salix_Data$No_of_main_branches, n, replace=TRUE)                                                
  Max_stem_diameter<-sample(Hjerkinn_Salix_Data$`Max_stem_diameter_(mm)`, n, replace=TRUE)                                             
  Catkins_Present<-sample(Hjerkinn_Salix_Data$`Catkins_Present_(y=1/n=0)`, n, replace=TRUE)                                          
  Catkin_sex<-sample(Hjerkinn_Salix_Data$Catkin_sex, n, replace=TRUE)                                                         
  Catkin_number<-sample(Hjerkinn_Salix_Data$Catkin_number, n, replace=TRUE)                                                      
  Catkin_Buds<-sample(Hjerkinn_Salix_Data$`Catkin_Buds_(number)`, n, replace=TRUE)                                               
  Catkins_and_buds_present<-sample(Hjerkinn_Salix_Data$`Catkins_and_buds_present?_(y=1,n=0)`, n, replace=TRUE)                                
  Overall_browsing_Score<-sample(Hjerkinn_Salix_Data$Overall_browsing_Score, n, replace=TRUE)                                             
  Mammal_damage<-sample(Hjerkinn_Salix_Data$`Mammal_damage_(0=none/negligable,1= minor,2= significant,3= severe)`, n, replace=TRUE)
  Mammal_species<-sample(Hjerkinn_Salix_Data$Mammal_species, n, replace=TRUE)                                                     
  Insect_damage<-sample(Hjerkinn_Salix_Data$`Insect_damage_(0=none/negligable,1= minor,2= significant,3= severe)`, n, replace=TRUE)
  Galls_present<-sample(Hjerkinn_Salix_Data$`Galls_present_(y=1/n=0)`, n, replace=TRUE)                                            
  Dung_or_signs_present_in_Vacinity<-sample(Hjerkinn_Salix_Data$`Dung_or_signs_present_in_Vacinity_(y=1/n=0)`, n, replace=TRUE)                        
  Dung_signs_species<-sample(Hjerkinn_Salix_Data$`Dung/signs_species`, n, replace=TRUE)                                                 
  Winter_dieback<-sample(Hjerkinn_Salix_Data$`Winter_dieback?_(y=1,n=0)`, n, replace=TRUE)                                          
  Rust_present<-sample(Hjerkinn_Salix_Data$`Rust_present?_(y=1/n=0)`, n, replace=TRUE)                                            
  Early_senescence<-sample(Hjerkinn_Salix_Data$`Early_senescence?_(y=1/n=0)`, n, replace=TRUE)                                        
  
  sim_data<-tibble(Planting_Site_No, Unit_No, Year_planted, Salix_number, Species,
                   Health_Status, Max_diameter, Perpendicular_Diameter, Max_Height,
                   Max_Canopy_volume,No_of_main_branches,Max_stem_diameter,
                   Catkins_Present, Catkin_sex, Catkin_number, Catkin_Buds,
                   Catkins_and_buds_present, Overall_browsing_Score,
                   Mammal_damage, Mammal_species, Insect_damage, Galls_present,
                   Dung_or_signs_present_in_Vacinity, Dung_signs_species,
                   Winter_dieback, Rust_present, Early_senescence)
  return(sim_data)
  
}


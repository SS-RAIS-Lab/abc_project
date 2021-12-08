# Campaign Donation Wrangling and Plot Generator
# Hugo Sant'Anna
# December 2021

library(tidyverse)
library(fst)
library(bbplot)

#---- Some basic data preparation ----


# 2012 File

cand2012_dirty <- read.table("./prestacao_final_2012/receitas_candidatos_2012_brasil.txt", sep = ";", header=TRUE, colClasses = "character")

cand2012 <- cand2012_dirty %>% 
  transmute(
    year = 2012,
    state = UF,
    muni_code = Numero.UE,
    municipality = Municipio,
    party = Sigla..Partido,
    position = Cargo,
    donor_cnpj = factor(CPF.CNPJ.do.doador),
    econ_code = factor(Cod.setor.econômico.do.doador),
    econ_sector = factor(Setor.econômico.do.doador),
    value = Valor.receita
  )



# 2016 File

cand2016_dirty <- read.table("./prestacao_final_2016/receitas_candidatos_2016_brasil.txt", sep = ";", header=TRUE, colClasses = "character")

cand2016 <- cand2016_dirty %>% 
  transmute(
    year = 2016,
    state = UF,
    muni_code = Sigla.da.UE,
    municipality = Nome.da.UE,
    party = Sigla..Partido,
    position = Cargo,
    donor_cnpj = factor(CPF.CNPJ.do.doador),
    econ_code = factor(Cod.setor.econômico.do.doador),
    econ_sector = factor(Setor.econômico.do.doador),
    value = Valor.receita
  )


donation_data <- bind_rows(cand2012, cand2016) %>% 
  mutate(
    econ_code = str_extract(econ_code, "\\d{2}"),
    econ_code = case_when(
      is.na(econ_code) ~ "00",
      TRUE ~ econ_code
     ),
    econ_sector = case_when(
      econ_code == "00" ~ "Physical Person",
      econ_code == "01" ~ "Extraction: Agriculture, Livestock and Related",
      econ_code == "02" ~ "Extraction: Silviculture",
      econ_code == "03" ~ "Extraction: Fishing",
      econ_code == "05" ~ "Extraction: Coal",
      econ_code == "06" ~ "Extraction: Oil and Natural Gas",
      econ_code == "07" ~ "Extraction: Metal",
      econ_code == "08" ~ "Extraction: Non-Metal",
      econ_code == "09" ~ "Extraction: Mining Support Activities",
      econ_code == "10" ~ "Transformation: Food",
      econ_code == "11" ~ "Transformation: Beverage Industry",
      econ_code == "12" ~ "Transformation: Tobacco Industry",
      econ_code == "13" ~ "Transformation: Textile Industry",
      econ_code == "14" ~ "Transformation: Tailoring Industry",
      econ_code == "15" ~ "Transformation: Leatherworking Industry",
      econ_code == "16" ~ "Transformation: Woodwork Industry",
      econ_code == "17" ~ "Transformation: Paper",
      econ_code == "18" ~ "Transformation: Printing",
      econ_code == "19" ~ "Transformation: Oil and Biofuel",
      econ_code == "20" ~ "Transformation: Chemical",
      econ_code == "21" ~ "Transformation: Pharmaceutical",
      econ_code == "22" ~ "Transformation: Rubber and Plastic",
      econ_code == "23" ~ "Transformation: Non-Mineral",
      econ_code == "24" ~ "Transformation: Metallurgy",
      econ_code == "25" ~ "Transformation: Non-Machinery Metal ",
      econ_code == "26" ~ "Transformation: Optical and Electronic Equipment",
      econ_code == "27" ~ "Transformation: Electrical Machinery",
      econ_code == "28" ~ "Transformation: Other Machinery",
      econ_code == "29" ~ "Transformation: Automobile",
      econ_code == "30" ~ "Transformation: Non-automobile Transport",
      econ_code == "31" ~ "Transformation: Furniture",
      econ_code == "32" ~ "Transformation: Other Products",
      econ_code == "33" ~ "Transformation: Maintenance and Repair of Machinery",
      econ_code == "35" ~ "Utilities: Gas and Electricity",
      econ_code == "36" ~ "Utilities: Water",
      econ_code == "37" ~ "Utilities: Sewage",
      econ_code == "38" ~ "Utilities: Waste Collection, Treatment and Recycling",
      econ_code == "39" ~ "Utilities: Waste Decontamination",
      econ_code == "41" ~ "Construction: Buildings",
      econ_code == "42" ~ "Construction: Infrastructure",
      econ_code == "43" ~ "Construction: Support Services",
      econ_code == "45" ~ "Commerce and Services: Automobile Related",
      econ_code == "46" ~ "Commerce and Services: Wholesail Non-automobile" ,
      econ_code == "47" ~ "Commerce and Services: Retail",
      econ_code == "49" ~ "Commerce and Services: Land Transportation",
      econ_code == "50" ~ "Commerce and Services: Water Transportation",
      econ_code == "51" ~ "Commerce and Services: Air Transportation",
      econ_code == "52" ~ "Commerce and Services: Storage",
      econ_code == "53" ~ "Commerce and Services: Shipping",
      econ_code == "55" ~ "Commerce and Services: Lodging",
      econ_code == "56" ~ "Commerce and Services: Food",
      econ_code == "58" ~ "Commerce and Services: Photo Edition",
      econ_code == "59" ~ "Commerce and Services: Movie, Film and Sound Activities",
      econ_code == "60" ~ "Commerce and Services: Radio and Television Activities",
      econ_code == "61" ~ "Commerce and Services: TelecomunicaÃ§Ãµes",
      econ_code == "62" ~ "Commerce and Services: Information Technology Services",
      econ_code == "63" ~ "Commerce and Services: Information Technology Support",
      econ_code == "64" ~ "Banking and Insurance: Financial Services",
      econ_code == "65" ~ "Banking and Insurance: Insurance",
      econ_code == "66" ~ "Banking and Insurance: Support Activities for Banking and Finance",
      econ_code == "68" ~ "Real State",
      econ_code == "69" ~ "Commerce and Services:Accounting, Auditing and Law",
      econ_code == "70" ~ "Commerce and Services: Corporate and Consulting",
      econ_code == "71" ~ "Commerce and Services: Architecture and Engineering",
      econ_code == "72" ~ "Commerce and Services: Scientific Research",
      econ_code == "73" ~ "Commerce and Services: Marketing and Advertisement",
      econ_code == "74" ~ "Commerce and Services: Other",
      econ_code == "75" ~ "Commerce and Services: Veterinary",
      econ_code == "77" ~ "Commerce and Services: Non-real State Renting",
      econ_code == "78" ~ "Commerce and Services: Human Resources",
      econ_code == "79" ~ "Commerce and Services: Travel Agencies",
      econ_code == "80" ~ "Commerce and Services: Security and Watch",
      econ_code == "81" ~ "Commerce and Services: Landscaping",
      econ_code == "82" ~ "Commerce and Services: Office Services",
      econ_code == "84" ~ "Public: Administration, Defense and Insurance",
      econ_code == "85" ~ "Education",
      econ_code == "86" ~ "Healthcare",
      econ_code == "87" ~ "Healthcare",
      econ_code == "88" ~ "Healthcare",
      econ_code == "90" ~ "Art and Sports: Culture",
      econ_code == "91" ~ "Art and Sports: Culture",
      econ_code == "92" ~ "Art and Sports: Gambling",
      econ_code == "93" ~ "Art and Sports: Sports",
      econ_code == "94" ~ "Political Organizations and Unions",
      econ_code == "95" ~ "Commerce and Services: Repair of Other Goods",
      econ_code == "95" ~ "Commerce and Services: Repair of Other Goods",
      econ_code == "96" ~ "Commerce and Services: Personal Care"
    )
  )

donation_data <- donation_data %>% 
  mutate(
    spectrum = case_when(
      party %in% c("DC", "PSDC", "DEM", "PRP", "PMB", "PHS", "PTC") ~ "Center Right",
      party %in% c("NOVO") ~ "Economically Right",
      party %in% c("PATRI", "PRP", "PEN", "PODE", "PTN", "PRTB", "PSC") ~ "Nationalist Right",
      party %in% c("PMDB", "MDB", "PP", "PR", "PSD", "PTB", "PRB", "PSC", "PROS", "SD", "PEN", "PTN", "PHS" , "PSL", "AVANTE", "PT do B") ~ "Great Center",
      party %in% c("PMN", "PSDB", "PV", "PSB") ~ "Center Left",
      party %in% c("PC do B", "PT", "PCO", "PCB", "PDT", "PPL", "PPS", "PSOL", "REDE", "PSTU") ~ "Left",
      (party == "PRB" & year == 2012) ~ "Great Center",
      (party == "PRB" & year == 2016) ~ "Nationalist Right",
      (party == "PSL" & year == 2012) ~ "Center Left",
      (party == "PSL" & year == 2016) ~ "Center Right"
    )
  )


donation_data$value <- as.numeric(sub(",", ".", donation_data$value, fixed = TRUE))
  
fst::write_fst(donation_data, "donation_data.fst")


#---- Political Inclination ----

plot_br_donation <- donation_data %>%
  mutate(year = factor(year)) %>% 
  filter(state == "SP") %>% 
  #filter(str_detect(econ_sector, "Physical")) %>% 
  group_by(year, spectrum) %>% 
  summarise(n = n())


color <- c("2012" = "dodgerblue",
           "2016" = "darkgreen")

ggplot(plot_br_donation, aes(x = spectrum, y = n, fill = year), xlab = "Political Spectrum") +
  geom_bar(stat = "identity", width = .5, position = "dodge") +
  labs(title = "Political Donation Shift in SÃ£o Paulo",
       subtitle = "2012 and 2016 municipal elections")+
  #geom_tile(data=plot_br_donation, aes(x = spectrum, y = 1000000, fill = spectrum))+
  scale_x_discrete(limits=c("Left",
                            "Center Left", 
                            "Great Center", 
                            "Center Right", 
                            "Economically Right",
                            "Nationalist Right"))+
  bbc_style()+
  scale_fill_manual(values = color)+
  coord_polar("y", start = 0)






#---- Political Inclination ----

plot_br_donation <- donation_data %>%
  mutate(year = factor(year)) %>% 
  filter(year == 2012) %>% 
  #filter(state == "SP") %>% 
  #filter(str_detect(econ_sector, "Physical")) %>% 
  group_by(year, spectrum) %>% 
  summarise(n = n())


color <- c("2012" = "dodgerblue",
           "2016" = "darkgreen")

ggplot(plot_br_donation, aes(x = "", y = n, fill = spectrum), xlab = "Political Spectrum") +
  geom_bar(stat = "identity", width = .5) +
  labs(title = "Political Donation Shift in SÃ£o Paulo",
       subtitle = "2012 and 2016 municipal elections")+
  #geom_tile(data=plot_br_donation, aes(x = spectrum, y = 1000000, fill = spectrum))+
  scale_x_discrete(limits=c("Left",
                            "Center Left", 
                            "Great Center", 
                            "Center Right", 
                            "Economically Right",
                            "Nationalist Right"))+
  #bbc_style()+
  scale_fill_manual(values = color)+
  coord_polar("y", start = 0)+
  theme_void()

  


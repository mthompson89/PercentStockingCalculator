# Gingrich Stocking Line Calculator
This code is used to take an output from FVS and calculate the A line, B line and C line (percent stocking level) used in Gingrich Stocking Charts (Ginrich 1967).
This code also implements and modifies Ducey & Knapp (2010) relative density function
in order to calculate the percent stocking level.

When Citing please cite:

Michael C. Thompson, Percent Stocking Calculator, (2021), Github repository, 
https://doi.org/10.5281/zenodo.4596762

First reading in libraries needed for the following code, as well as the function used to calculate a-line or b-line. I decided to change from three different functions to a single function that you can just change percent stocking to calculate what ever line you want. 

```{r setup, include=TRUE, message=FALSE}
library(tidyverse) #needed for read_csv()
library(dplyr)     #needed for using the %>% (pipe) opperators

X_lineBA <- function(DBH, SG, x){
  a = 0.00015# constants from Ducey&knapp 2010 relative density measure
  b = 0.00218 # constants from Ducey&knapp 2010 relative density measure
  c = x # percent stocking line
  k = pi/40000 #metric constant for area
  
  BA <- (((25^1.6*c*k)/(a+b*SG))*(DBH*2.54)^0.4)*4.356
}
```

Read in the FVS output file and specific Gravity reference file using `read_csv()` so that it imports as a tibble and is much faster. `col_types = ` is defining the classes of the columns. The only column that needs to be read in as a factor is StandID, because the ID's are long number strings, if you don't read them in as a factor they will be converted to scientific notation. The `FVS_SampleOutput` file is a shortened FVS example output for you to use and play with if you need/want to.

```{r}
FVS_SampleOutput <- read_csv("F:/AFRI Grant/DegCalc/FVS_SampleOutput.csv", col_types = "ffifnnnnnnnni")
SG_ref <- read_csv("F:/AFRI Grant/DegCalc/SG_ref.csv", col_types = "ffnn")
```

Removes any leading zeros from the specific gravity reference species list so that they match each othe for a joining later
```{r}
SG_ref$splist <- sub("^0+", "", SG_ref$splist)
```

build the first dataframe creating columns for Basal Area and Basal area per tree. 
because of the pipe opperator I can then join the SG reference dataframe and then select the important columns I need
```{r}
df1 <- FVS_SampleOutput%>%
  group_by(StandID, Year,Species)%>%  #group by stand and year and species
  mutate(BA = (DBH^2)* 0.005454,
         BA_per_Tree = BA*TPA,)%>%
  full_join(SG_ref, by = c("Species" = "splist"))%>%          # join with SG reference file
  select(StandID, Species, Gen_sp, sg, Year, DBH, TPA, BA, BA_per_Tree) #select the desired columns
df1
```

Lastly I build the final dataframe that has the Aline Bline and Cline for each stand and year. If you only have one year you don't need to group by it and can just group by StandID.
```{r}
df2 <- df1%>%
  group_by(StandID, Year)%>%
  mutate(Stand_mean_DBH = mean(DBH),
         Stand_mean_SG = mean(sg),
         Total_BA_per_stand = sum(BA_per_Tree),
         Cline_BA = X_lineBA(Stand_mean_DBH,Stand_mean_SG, 0.4),
         Bline_BA = X_lineBA(Stand_mean_DBH,Stand_mean_SG, 0.6),
         Aline_BA = X_lineBA(Stand_mean_DBH,Stand_mean_SG, 1.0)
         )
df2
```
<p>references:</p>
<p>Ducey, M. J., & Knapp, R. A. (2010). Rapid assessment of relative density in mixed-species stands of the northeastern United States. International Journal of Forestry Research, 2010.</p>
<p>Gingrich, S. F. (1967). En Measuring and evaluating stocking and density in upland hardwood forest in the Central States (págs. 69-80). For. Sci.</p>



require(tidyverse)

test_read <- read_csv("SampleFiles/Flumes/V1V2/HoboU20OutsideWell/S1090418.csv",
                      skip = 1, guess_max=5000)
test_read

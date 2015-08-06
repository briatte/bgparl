# hi Bulgaria

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos.zip"))
  unzip("photos.zip")

dir.create("photos", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw"             , showWarnings = FALSE)
dir.create("raw/bill-lists"  , showWarnings = FALSE)
dir.create("raw/bill-pages"  , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Bulgaria",
  "lang" = "en", # Wikipedia language for chamber and constituencies
  "ch" = "Narodno Sabranie",
  "type" = "Unicameral",
  "ipu" = 2045,
  "seats" = 240
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

# have a nice day

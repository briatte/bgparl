# hi Bulgaria

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"   , showWarnings = FALSE)
dir.create("photos" , showWarnings = FALSE)
dir.create("plots"  , showWarnings = FALSE)

dir.create("raw"             , showWarnings = FALSE)
dir.create("raw/bill-lists"  , showWarnings = FALSE)
dir.create("raw/bill-pages"  , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
meta = c("Bulgaria", "Narodno Sabranie")
mode = "fruchtermanreingold"

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

# have a nice day

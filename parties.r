# party colors

colors = c(
  "KB" = "#E41A1C",   # red
  "DPS" = "#80B1D3",  # light blue
  "A" = "#A65628",    # brown
  "NMS" = "#FFFF33",  # yellow
  "BNS" = "#FF7F00",  # orange
  "ODS" = "#BEBADA",  # light purple
  "GERB" = "#984EA3", # purple
  "SK" = "#377EB8",   # blue
  "DSB" = "#053061",  # dark blue
  "RZS" = "#FDB462"   # light orange
)

# party names
groups = c(
  "KB" = "Coalition for Bulgaria", # Коалиция за България (led by Socialists)
  "DPS" = "Movement for Rights and Freedoms", # Движение за права и свободи
  "A" = "Ataka", # Атака
  "NMS" = "National Movement Simeon the Second", # Национално движение Симеон Втори
  "BNS" = "Bulgarian People's Union", # Български Народен Съюз
  "ODS" = "United Democratic Forces", # Обединени Демократични Сили
  "GERB" = "Citizens for European Development of Bulgaria", # ГЕРБ
  "SK" = "Blue Coalition", # Синята коалиция
  "DSB" = "Democrats for Strong Bulgaria", # Демократи за Силна България
  "RZS" = "Order, Lawfulness, Justice" # Ред, законност и справедливост
)

# ParlGov Left/Right scores

scores = c(
  "KB" = 2.9,
  "DPS" = 4.6,
  "A" = 5.5,
  "NMS" = 5.8,
  "BNS" = 5.8,
  "ODS" = 7,
  "GERB" = 7.4,
  "SK" = 7.4,
  "DSB" = 7.9,
  "RZS" = 8.7
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]

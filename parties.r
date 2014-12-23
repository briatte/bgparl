# party colors

colors = c(
  "KB" = "#E41A1C",   # Коалиция за България (Coalition for Bulgaria, led by Socialists) -- red
  "DPS" = "#80B1D3",  # Движение за права и свободи (Movement for Rights and Freedoms) -- light blue
  "A" = "#A65628",    # Атака (Ataka) -- brown
  "NMS" = "#FFFF33",  # Национално движение Симеон Втори (National Movement Simeon the Second) -- yellow
  "BNS" = "#FF7F00",  # Български Народен Съюз (Bulgarian People's Union) -- orange
  "ODS" = "#BEBADA",  # Обединени Демократични Сили (United Democratic Forces, 2005 election) -- light purple
  "GERB" = "#984EA3", # ГЕРБ (Citizens for European Development of Bulgaria) -- purple
  "SK" = "#377EB8",   # Синята коалиция (Blue Coalition) -- blue
  "DSB" = "#053061",  # Демократи за Силна България (Democrats for Strong Bulgaria) -- dark blue
  "RZS" = "#FDB462"   # Ред, законност и справедливост (Order, Lawfulness, Justice) -- light orange
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

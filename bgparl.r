# hi Bulgaria

library(igraph)
library(GGally)
library(network)
library(sna)
library(qdap)
library(plyr)
library(rgexf)
library(stringr)
library(tnet)
library(XML)

gexf = FALSE

# http://stackoverflow.com/a/6364905/635806

simpleCap <- function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

# colors

colors = c(
  "Coalition for Bulgaria" = "#E41A1C",   # Коалиция за България (KB, led by Socialists), red
  "National Movement Simeon the Second" = "#FFFF33",  # Национално движение Симеон Втори (NMS), yellow
  "Order, Lawfulness, Justice" = "#FDB462",  # Ред, законност и справедливост (RZS), light orange
  "Bulgarian People's Union" = "#FF7F00",  # Български Народен Съюз (BNS), orange
  "Citizens for European Development of Bulgaria" = "#984EA3", # ГЕРБ (GERB), purple
  "United Democratic Forces" = "#BEBADA",  # Обединени Демократични Сили (ODS, 2005 election), light purple
  "Movement for Rights and Freedoms" = "#80B1D3", # Движение за права и свободи (DPS), light blue
  "Blue Coalition" = "#377EB8", # Синята коалиция (SK), blue
  "Democrats for Strong Bulgaria" = "#053061", # Демократи за Силна България (DSB), dark blue
  "Ataka" = "#A65628" # Атака (A), brown
)
order = names(colors)

# bills

data = "data/bills.csv"
if(!file.exists(data)) {

  root = "http://www.parliament.bg"
  file = "data/index.html"
  
  if(!file.exists(file))
    download.file("http://www.parliament.bg/bg/bills", , mode = "wb", quiet = TRUE)
  
  h = htmlParse(file, encoding = "UTF-8")
  h = xpathSApply(h, "//a[contains(@href, 'bills/period')]/@href")
  
  b = data.frame()
  k = c()
  for(i in h) {
    
    mth = gsub("(.*)(\\d{4})", "\\2", i)
    cat("Scraping month", str_pad(mth, 7, "right"), "... ")
    file = paste0("data/index-", mth, ".html")
    
    if(!file.exists(file))
      download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
    
    h = htmlParse(file)
    h = xpathSApply(h, "//a[contains(@href, 'bills/ID')]/@href")
    
    for(j in rev(h)) {
      
      #     cat(sprintf("%3.0f", which(h == j)))
      file = paste0("data/bill-", gsub("\\D", "", j), ".html")
      
      if(!file.exists(file))
        download.file(paste0(root, j), file, mode = "wb", quiet = TRUE)
      
      hh = htmlParse(file)
      jj = xpathSApply(hh, "//ul[@class='frontList'][1]/li/a/@href")
      
      ref = xpathSApply(hh, "//td[@class='h1']/following-sibling::td", xmlValue)
      
      b = rbind(b, data.frame(
        uid = as.character(gsub("\\D", "", j)), ref = ref[2],
        date = as.Date(strptime(ref[3], "%d/%m/%Y")), session = ref[4],
        title = scrubber(ref[1]),
        authors = paste0(gsub("\\D", "", jj), collapse = ";"),
        committee = paste0(gsub("\\D", "",
                                xpathSApply(hh, "//td[@class='h1'][7]/following-sibling::td/ul/li/a/@href")),
                           collapse = ";"),
        stringsAsFactors = FALSE))
      
      if(any(grepl("/MP", jj)))
        k = unique(c(k, jj))
      
    }
    
    cat(sprintf("%2.0f", length(h)), "bill(s)\n")
    
  }
  
  b$authors[ b$authors == "" ] = "GOV"
  
  legislature = NA
  legislature[ is.na(legislature) & b$date < as.Date("2005-07-11") ] = "2001-2005"
  legislature[ is.na(legislature) & b$date < as.Date("2009-07-14") ] = "2005-2009"
  legislature[ is.na(legislature) & b$date < as.Date("2013-05-09") ] = "2009-2013"
  legislature[ is.na(legislature) & b$date > as.Date("2013-05-09") ] = "2013-2017"
  table(legislature, b$authors != "GOV", exclude = NULL)
  
  write.csv(cbind(legislature, b), data, row.names = FALSE)
  
  
  # sponsors
  
  data = "data/sponsors.csv"
  if(!file.exists(data)) {
    
    s = data.frame()
    for(i in rev(gsub("/bg/", "/en/", k))) {
      
      file = paste0("data/mp-", gsub("\\D", "", i), ".html")
      
      if(!file.exists(file))
        download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
      
      h = htmlParse(file)
      cat(sprintf("%4.0f", which(k == i)))
      
      nfo = xpathSApply(h, "//div[@class='MPinfo']/ul/li", xmlValue)
      born = nfo[ grepl("Date of birth|Дата на раждане", nfo) ]
      job = gsub("Profession|Професия|: |;$|N\\.A\\.", "", nfo[ grepl("Profession|Професия", nfo) ])
      
      s = rbind(s, data.frame(
        name = xpathSApply(h, "//img[contains(@src, 'Assembly')]/@alt"),
        born = str_extract(born, "[0-9]{4}"),
        born_bg = as.numeric(grepl("Bulgaria|България", born)), # born in Bulgaria (0/1)
        party = gsub("Political force: |Избран\\(а\\) с политическа сила: |;$", "",
                     nfo[ grepl("Political force|политическа сила", nfo) ]),
        constituency = gsub("Constituency: |Изборен район: |;$", "",
                            nfo[ grepl("Constituency|Изборен район", nfo) ]),
        job = ifelse(length(job), job, NA),
        url = gsub("/en/", "", i),
        # photo = xpathSApply(h, "//img[contains(@src, 'Assembly')]/@src"),
        stringsAsFactors = FALSE))
      
      cat(":", tail(s, 1)$name, "\n")
      
    }
    
    write.csv(s, data, row.names = FALSE)
    
  }
  
}

m = read.csv("data/bills.csv", stringsAsFactors = FALSE)
m = subset(m, authors != "GOV")
m$n_au = 1 + str_count (m$authors, ";")

s = read.csv("data/sponsors.csv", stringsAsFactors = FALSE)

s$constituency = gsub("\\d|-| (GRAD|OKRAG|OBLAST)", "", s$constituency)
s$constituency = sapply(tolower(s$constituency), simpleCap)

# name fixes (Google translations with first name checks)

s$name[ s$url == "MP/171" ] = "Kostadin Stoyanov PASKALEV"
s$name[ s$url == "MP/131" ] = "ATANAS PETROV ATANASOV"
s$name[ s$url == "MP/57" ] = "IVO PŬRVANOV ATANASOV"
s$name[ s$url == "MP/819" ] = "PETAR DIMITROV POPOV"
s$name[ s$url == "MP/815" ] = "HRISTO KIRILOV POPOV"
s$name[ s$url == "MP/24" ] = "BOYKO STEFANOV VELIKOV"
s$name[ s$url == "MP/32" ] = "MIHAIL RAĬKOV MIKOV"
s$name[ s$url == "MP/190" ] = "VOLER NIKOLOV SIDEROV"
s$name[ s$url == "MP/1" ] = "LUBEN ANDONOV KORNEZOV"
s$name[ s$url == "MP/79" ] = "ANGEL PETROV NAĬDENOV"
s$name[ s$url == "MP/790" ] = "DENITSA IVAĬLOVA DIMITROVA"
s$name[ s$url == "MP/792" ] = "ILKO DIMITROV DIMITROV"
s$name[ s$url == "MP/209" ] = "STELA DIMITROVA ANGELOVA-BANKOVA"
s$name[ s$url == "MP/114" ] = "ELEONORA NIKOLAEVA NIKOLOVA"

s$name = sapply(tolower(s$name), simpleCap)
s$uid = paste(s$name, gsub("\\D", "", s$url))

s$party[ grepl("Ataka|Attack|Атака", s$party) ] = 
  "Ataka" # A
s$party[ grepl("Movement for Rights and Freedoms|Движение за права и свободи", s$party) ] = 
  "Movement for Rights and Freedoms" # DPS 
s$party[ grepl("Coalition for Bulgaria|Коалиция за България", s$party) ] = 
  "Coalition for Bulgaria" # KB
s$party[ grepl("GERB|ГЕРБ", s$party) ] = 
  "Citizens for European Development of Bulgaria" # GERB
s$party[ grepl("Democrats for Strong Bulgaria|Демократи за Силна България", s$party) ] = 
  "Democrats for Strong Bulgaria" # DSB
s$party[ grepl("National Movement Simeon the Second|Национално движение Симеон Втори", s$party) ] = 
  "National Movement Simeon the Second" # NMS
s$party[ grepl("Order, Lawfulness Justice|Ред, законност и справедливост", s$party) ] =
  "Order, Lawfulness, Justice" # RZS
s$party[ grepl("Blue Coalition|Синята коалиция", s$party) ] =
  "Blue Coalition" # SK
s$party[ grepl("United Democratic Forces|Обединени Демократични Сили", s$party) ] =
  "United Democratic Forces" # ODS
s$party[ grepl("Bulgarian People's Union|Български Народен Съюз", s$party) ] =
  "Bulgarian People's Union" # BNS

# all sponsors recognized
stopifnot(all(unique(unlist(strsplit(m$authors, ";"))) %in% gsub("\\D", "", s$url)))

for(l in unique(m$legislature)) {
  
  data = subset(m, legislature == l & n_au > 1)
  cat("Legislature", l, ":", nrow(data), "cosponsored bills, ")
  
  rownames(s) = gsub("\\D", "", s$url)
  
  edges = lapply(data$authors, function(i) {
    
    w = unlist(strsplit(i, ";"))
    d = s[ w, "uid" ]
    d = expand.grid(d, d)
    d = subset(d, Var1 != Var2)
    d$uid = apply(d, 1, function(x) paste0(sort(x), collapse = "_"))
    d = unique(d$uid)
    if(length(d)) {
      d = data.frame(i = gsub("(.*)_(.*)", "\\1", d),
                     j = gsub("(.*)_(.*)", "\\2", d),
                     w = length(w))
      return(d)
    } else {
      return(data.frame())
    }
    
  })
  
  edges = rbind.fill(edges)
  edges$uid = apply(edges, 1, function(x) paste0(sort(x[ 1:2 ]), collapse = "_"))
  
  # raw edge counts
  count = table(edges$uid)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ uid, function(x) sum(1 / x), data = edges)
  
  # raw counts
  edges$count = as.vector(count[ edges$uid ])
  
  edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                     j = gsub("(.*)_(.*)", "\\2", edges$uid),
                     w = edges$w, n = edges[, 3])
  
  # network
  
  n = network(edges[, 1:2 ], directed = FALSE)
  cat(network.edgecount(n), "edges,", network.size(n), "nodes\n")
  
  n %n% "title" = paste("Народно събрание", paste0(range(substr(data$date, 1, 4)), collapse = " to "))
  n %n% "n_bills" = nrow(data)
  
  rownames(s) = s$uid
  n %v% "url" = s[ network.vertex.names(n), "url" ]
  n %v% "name" = s[ network.vertex.names(n), "name" ]
  # n %v% "sex" = s[ network.vertex.names(n), "sex" ]
  n %v% "born" = s[ network.vertex.names(n), "born" ]
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  # n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]
  n %v% "constituency" = s[ network.vertex.names(n), "constituency" ]
  # n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  # n %v% "coalition" = ifelse(n %v% "party" %in% c("S", "V", "MP"), "Leftwing", # Rödgröna
  #                            ifelse(party == "SD", NA, "Rightwing")) # Alliansen
  
  network::set.edge.attribute(n, "source", as.character(edges[, 1]))
  network::set.edge.attribute(n, "target", as.character(edges[, 2]))
  
  network::set.edge.attribute(n, "weight", edges[, 3])
  network::set.edge.attribute(n, "count", edges[, 4])
  network::set.edge.attribute(n, "alpha",
                              as.numeric(cut(n %e% "count", c(1:4, Inf),
                                             include.lowest = TRUE)) / 5)
  
  # modularity
  
  nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
  E(nn)$weight = edges[, 3]
  
  i = s[ V(nn)$name, "party" ]
  i[ i %in% c("-") ] = NA # unaffiliateds
  
  nn = nn - which(is.na(i))
  i = as.numeric(factor(i[ !is.na(i) ]))
  
  n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)
  
  walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
  
  # max. partition
  maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
  walktrap = walktrap[[ maxwalks ]]
  
  n %n% "modularity_walktrap" = modularity(walktrap)
  
  louvain = multilevel.community(nn)
  
  n %n% "modularity_louvain" = modularity(louvain)
  
  n %n% "modularity_maximized" = n %n% "modularity" /
    max(c(n %n% "modularity_walktrap", n %n% "modularity_louvain"))
  
  # weighted adjacency matrix to tnet
  tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
  
  # weighted degree and distance
  wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
  dist = distance_w(tnet)
  wdeg$distance = NA
  wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
  wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
  names(wdeg) = c("node", "degree", "distance", "clustering")
  
  n %v% "degree" = wdeg$degree
  n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
  
  n %v% "distance" = wdeg$distance
  n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
  
  n %v% "clustering" = wdeg$clustering    # local
  n %n% "clustering" = clustering_w(tnet) # global
  
  # edge colors
  
  i = colors[ s[ n %e% "source", "party" ] ]
  j = colors[ s[ n %e% "target", "party" ] ]
  
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"
  
  print(table(n %v% "party", exclude = NULL))
  
  n %v% "size" = as.numeric(cut(n %v% "degree", quantile(n %v% "degree"), include.lowest = TRUE))
  g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, # mode = "kamadakawai",
                             segment.color = party) +
                         geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                         geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                         scale_size_continuous(range = c(6, 12)) +
                         scale_color_manual("", values = colors, breaks = order) +
                         theme(legend.key.size = unit(1, "cm"),
                               legend.text = element_text(size = 16)) +
                         guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
  
  print(g)
  
  ggsave(paste0("bgparl", l, ".pdf"), g, width = 14, height = 9)
  ggsave(paste0("bgparl", l, ".png"), g, width = 14, height = 9, dpi = 72)
  
  assign(paste0("net_bg", substr(l, 1, 4)), n)
  
  if(gexf) {
    
    rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
    mode = "fruchtermanreingold"
    meta = list(creator = "rgexf", description = paste0(mode, " placement"),
                keywords = "Parliament, Bulgaria")
    
    node.att = data.frame(url = as.character(gsub("\\D", "", n %v% "url")),
                          party = n %v% "party",
                          constituency = n %v% "constituency",
                          distance = round(n %v% "distance", 1),
                          # photo = n %v% "photo",
                          stringsAsFactors = FALSE)
    
    people = data.frame(id = as.numeric(factor(network.vertex.names(n))),
                        label = network.vertex.names(n),
                        stringsAsFactors = FALSE)
    
    relations = data.frame(
      source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
      target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
      weight = round(n %e% "weight", 2), count = n %e% "count")
    relations = na.omit(relations)
    
    # check all weights are positive after rounding
    stopifnot(all(relations$weight > 0))
    
    nodecolors = lapply(n %v% "party", function(x)
      data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
    nodecolors = as.matrix(rbind.fill(nodecolors))
    
    # node placement
    position = do.call(paste0("gplot.layout.", mode),
                       list(as.matrix.network.adjacency(n), NULL))
    position = as.matrix(cbind(round(position, 1), 1))
    colnames(position) = c("x", "y", "z")
    
    # clean up vertex names from uid number
    people$label = gsub("\\s\\d+", "", people$label)

    # save with compressed floats
    write.gexf(nodes = people, nodesAtt = node.att,
               edges = relations[, 1:2 ], edgesWeight = relations[, 3],
               nodesVizAtt = list(position = position, color = nodecolors,
                                  size = round(n %v% "degree", 1)),
               # edgesVizAtt = list(size = relations[, 4]),
               defaultedgetype = "undirected", meta = meta,
               output = paste0("bgparl", l, ".gexf"))
    
  }
  
}

save(list = ls(pattern = "net_bg"), file = "bgparl.rda")

# have a nice day

meta = c("Bulgaria", "Narodno Sabranie")
mode = "fruchtermanreingold"

for(l in unique(m$legislature)) {
  
  data = subset(m, legislature == l & n_au > 1)
  cat("Legislature", l, ":", nrow(data), "cosponsored documents, ")
  
  rownames(s) = s$url
  
  # selects only MPs from the right legislature
  # print(table(s[ unique(unlist(strsplit(data$authors, ";"))), "legisl" ]))
  
  #
  # directed edge list
  #
  
  # the edge list is built from uids: MP names, followed by the
  # numeric id from their URL; this avoid duplicate row names
  
  edges = bind_rows(lapply(data$authors, function(i) {
    
    w = unlist(strsplit(i, ";"))
    d = s[ w, "uid" ]
    
    d = expand.grid(i = d, j = d[ 1 ], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))
  
  #
  # edge weights
  #
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$date, 1, 4))),
                                        collapse = " to "))
  
  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(m, legislature == l)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  # switch to uids for vertex attributes
  rownames(s) = s$uid
  
  n %v% "url" = as.character(s[ network.vertex.names(n), "url" ])  
  n %v% "sex" = as.character(s[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(substr(s[ network.vertex.names(n), "born" ], 1, 4))
  
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "partyname" = s[ network.vertex.names(n), "partyname" ]
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  
  # unnecessary: all photos present, just use URL to impute on the fly
  n %v% "photo" = as.character(s[ network.vertex.names(n), "photo" ])
  
  # mandate years done up to start year of legislature (imputed before)
  n %v% "nyears" = as.numeric(s[ network.vertex.names(n), "nyears" ])
  
  n %v% "constituency" = as.character(s[ network.vertex.names(n), "constituency" ])
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  #
  # weighted measures
  #
  
  # modularity
  n = get_modularity(n, weights = "raw")
  n = get_modularity(n, weights = "nfw")
  n = get_modularity(n, weights = "gsw")
  
  # centrality
  n = get_centrality(n, weights = "raw")
  n = get_centrality(n, weights = "nfw")
  n = get_centrality(n, weights = "gsw")
  
  #
  # network plot
  #
  
  if(plot) {
    
    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
    
    ggnet_save(n, file = paste0("plots/net_bg", l),
               i = colors[ s[ n %e% "source", "party" ] ],
               j = colors[ s[ n %e% "target", "party" ] ],
               q, colors, order)
    
  }
  
  #
  # save objects
  #
  
  # clean up vertex names before export (stops if finds duplicates)
  network.vertex.names(n) = gsub(" \\d+$", "", network.vertex.names(n))
  stopifnot(!length(network.vertex.names(n)[ duplicated(network.vertex.names(n)) ]))
  set.edge.attribute(n, "source", gsub(" \\d+$", "", n %e% "source"))
  set.edge.attribute(n, "target", gsub(" \\d+$", "", n %e% "target"))
  
  assign(paste0("net_bg", substr(l, 1, 4)), n)
  assign(paste0("edges_bg", substr(l, 1, 4)), edges)
  assign(paste0("bills_bg", substr(l, 1, 4)), data)
  
  #
  # export gexf
  #
    
  if(gexf)
    get_gexf(paste0("net_bg", l), n, meta, mode, colors, extra = "constituency")
  
}

if(gexf)
  zip("net_bg.zip", dir(pattern = "^net_bg\\d{4}-\\d{4}\\.gexf$"))

save(list = ls(pattern = "^(net|edges|bills)_bg\\d{4}$"),
     file = "data/net_bg.rda")

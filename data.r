# http://stackoverflow.com/a/6364905/635806

simpleCap <- function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

# bills

data = "data/bills.csv"
if(!file.exists(data)) {
  
  root = "http://www.parliament.bg"
  file = "raw/index.html"
  
  if(!file.exists(file))
    download.file("http://www.parliament.bg/bg/bills", file, mode = "wb", quiet = TRUE)
  
  h = htmlParse(file, encoding = "UTF-8")
  h = xpathSApply(h, "//a[contains(@href, 'bills/period')]/@href")
  
  b = data.frame()
  k = c()
  for(i in h) {
    
    mth = gsub("(.*)(\\d{4})", "\\2", i)
    cat("Scraping month", str_pad(mth, 7, "right"), "... ")
    file = paste0("raw/index-", mth, ".html")
    
    if(!file.exists(file))
      download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
    
    h = htmlParse(file)
    h = xpathSApply(h, "//a[contains(@href, 'bills/ID')]/@href")
    
    for(j in rev(h)) {
      
      #     cat(sprintf("%3.0f", which(h == j)))
      file = paste0("raw/bill-", gsub("\\D", "", j), ".html")
      
      if(!file.exists(file))
        download.file(paste0(root, j), file, mode = "wb", quiet = TRUE)
      
      hh = htmlParse(file)
      jj = xpathSApply(hh, "//ul[@class='frontList'][1]/li/a/@href")
      
      ref = xpathSApply(hh, "//td[@class='h1']/following-sibling::td", xmlValue)
      
      b = rbind(b, data.frame(
        uid = as.character(gsub("\\D", "", j)), ref = ref[2],
        date = as.Date(strptime(ref[3], "%d/%m/%Y")), session = ref[4],
        title = str_clean(ref[1]),
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
  legislature[ is.na(legislature) & b$date < as.Date("2005-07-11") ] = "2001-2005" # l. 39
  legislature[ is.na(legislature) & b$date < as.Date("2009-07-14") ] = "2005-2009" # l. 40
  legislature[ is.na(legislature) & b$date < as.Date("2013-05-09") ] = "2009-2013" # l. 41
  legislature[ is.na(legislature) & b$date > as.Date("2013-05-09") ] = "2013-2014" # l. 42 (ended Aug., election Oct.)
  table(legislature, b$authors != "GOV", exclude = NULL)
  
  write.csv(cbind(legislature, b), data, row.names = FALSE)

  # sponsors (none found in the bills for legislature 39, 2001-2005)
    
  data = "data/sponsors.csv"
  if(!file.exists(data)) {
    
    s = data.frame()
    for(i in rev(k)) {

      cat(sprintf("%4.0f", which(k == i)), str_pad(i, 12, "right"))

      # Bulgarian (seniority)
      file = paste0("raw/mp-", gsub("\\D", "", i), "-bg.html")
      
      if(!file.exists(file))
        download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
      
      h = htmlParse(file)
      mandates = xpathSApply(h, "//div[@class='MPinfo']/ul/li[contains(text(), 'НС:')]", xmlValue)
      mandates = as.numeric(unlist(str_extract_all(mandates, "[0-9]+")))
      
      legisl = xpathSApply(h, "//a[contains(@href, '/bg/MP/members/')]", xmlValue)
      legisl = as.numeric(unique(substr(legisl, 1, 2)))
      stopifnot(mandates < legisl)

      if(!length(mandates))
        mandates = ""

      # English (rest of details)
      i = gsub("/bg/", "/en/", i)
      file = paste0("raw/mp-", gsub("\\D", "", i), ".html")
      
      if(!file.exists(file))
        download.file(paste0(root, i), file, mode = "wb", quiet = TRUE)
      
      h = htmlParse(file)
      
      nfo = xpathSApply(h, "//div[@class='MPinfo']/ul/li", xmlValue)
      born = nfo[ grepl("Date of birth|Дата на раждане", nfo) ]
      job = gsub("Profession|Професия|: |;$|N\\.A\\.", "", nfo[ grepl("Profession|Професия", nfo) ])
      
      s = rbind(s, data.frame(
        legisl,
        name = xpathSApply(h, "//img[contains(@src, 'Assembly')]/@alt"),
        born = str_extract(born, "[0-9]{4}"),
        born_bg = as.numeric(grepl("Bulgaria|България", born)), # born in Bulgaria (0/1)
        mandates = paste0(mandates, collapse = ";"),
        nyears = 4 * length(mandates[ mandates != "" ]),
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

s$url = gsub("\\D", "", s$url)

s$photo = 1 # all photos found, ignore attribute later

# download photos
for(i in unique(s$url)) {
  photo = paste0("photos/", i, ".png")
  if(!file.exists(photo))
    try(download.file(paste0("http://www.parliament.bg/images/Assembly/", i, ".png"),
                      photo, mode = "wb", quiet = TRUE), silent = TRUE)
  if(!file.exists(photo) | !file.info(photo)$size) {
    file.remove(photo) # will warn if missing
    s$photo[ s$url == i ] = 0
  }
}

# convert constituencies to Wikipedia handles
s$constituency = gsub("\\d|-| (GRAD|OKRAG|OBLAST)", "", s$constituency)
s$constituency = sapply(tolower(s$constituency), simpleCap)
s$constituency = paste(s$constituency, "Province")
s$constituency = gsub("\\s", "_", s$constituency)

# name fixes (Google translations with first name checks)
s$name = str_clean(s$name)
s$name[ s$url == "171" ] = "KOSTADIN STOYANOV PASKALEV"
s$name[ s$url == "131" ] = "ATANAS PETROV ATANASOV"
s$name[ s$url == "57" ] = "IVO PŬRVANOV ATANASOV"
s$name[ s$url == "819" ] = "PETAR DIMITROV POPOV"
s$name[ s$url == "815" ] = "HRISTO KIRILOV POPOV"
s$name[ s$url == "24" ] = "BOYKO STEFANOV VELIKOV"
s$name[ s$url == "32" ] = "MIHAIL RAĬKOV MIKOV"
s$name[ s$url == "190" ] = "VOLER NIKOLOV SIDEROV"
s$name[ s$url == "1" ] = "LUBEN ANDONOV KORNEZOV"
s$name[ s$url == "79" ] = "ANGEL PETROV NAĬDENOV"
s$name[ s$url == "790" ] = "DENITSA IVAĬLOVA DIMITROVA"
s$name[ s$url == "792" ] = "ILKO DIMITROV DIMITROV"
s$name[ s$url == "209" ] = "STELA DIMITROVA ANGELOVA-BANKOVA"
s$name[ s$url == "114" ] = "ELEONORA NIKOLAEVA NIKOLOVA"

s$sex = NA
s$sex[ str_sub(s$name, -2) %in% c("EV", "OV") ] = "M"
s$sex[ str_sub(s$name, -2) == "VA" ] = "F"
s$sex[ grepl("^(A(K)?HMED|ANDREY|ANGEL|ARIF|ATANAS|BELGIN|BORIS(LAV)?|BOYKO|BYUNYAMIN|DAUT|DELYAN|DESISLAVA|DIMCHO|DIMITAR|DOBROMIR|DURHAN|EMIL|ERDINCH|GEORGI|GYUNER|HAMID|HASAN|HRISTO|IVAN|IVAYLO|JORDAN|JUNAL|KAMEN|KASIM|KIRIL|KRASIMIR|LYUBEN|LYUBOMIR|LYUTVI|MARIO|MEHMED|MIHAIL|MITHAT|MUSTAFA|NEDZHMI|NESRIN|NEVIN|NIKOLA(Y)?|PAVEL|PETAR|PLAMEN|RADOSLAV|RAMADAN|REMZI|RU(M|P)EN|RUSHEN|SEMIR|SHENDOAN|STANISLAV|STEFAN|STOYAN|TCHETIN|TODOR|TSVETAN|VALENTIN|VA(S)?SIL|VESELIN|VLADIMIR|YANKO|Y(O)?UNAL|YUKSEL|YUSEIN)\\s", s$name) ] = "M"
s$sex[ s$name == "REYHAH" ] = "M"
s$sex[ grepl("^(ANASTASIA|DANIELA|FATHME|GALINA|KRASTANKA|ILIYA|IRENA|MARGARITA|MARIA(NA)?|NIGYAR|PETYA|TATYANA|TEODORA|TSETSKA|VANYA)\\s", s$name) ] = "F"
# table(s$sex, exclude = NULL)

## old code, ignore (replaced by imputation from Bulgarian archive pages)
## imputed number of years in office (approximate)
# nyears = as.data.frame(table(s$name))
# names(nyears) = c("name", "nyears")
# s = merge(s, nyears, all.x = TRUE)
## n(mandates) also converted to 4 years for ongoing legislature (2013-)
## exclude that legislature from network sample
## s$nyears = 4 * s$nyears

s$name = sapply(tolower(s$name), simpleCap)
s$uid = paste(s$name, s$url)

s$party[ grepl("Ataka|Attack|Атака", s$party) ] = "A"
s$party[ grepl("Movement for Rights and Freedoms|Движение за права и свободи", s$party) ] = "DPS" 
s$party[ grepl("Coalition for Bulgaria|Коалиция за България", s$party) ] = "KB"
s$party[ grepl("GERB|ГЕРБ", s$party) ] = "GERB"
s$party[ grepl("Democrats for Strong Bulgaria|Демократи за Силна България", s$party) ] = "DSB"
s$party[ grepl("National Movement Simeon the Second|Национално движение Симеон Втори", s$party) ] = "NMS"
s$party[ grepl("Order, Lawfulness Justice|Ред, законност и справедливост", s$party) ] = "RZS"
s$party[ grepl("Blue Coalition|Синята коалиция", s$party) ] = "SK"
s$party[ grepl("United Democratic Forces|Обединени Демократични Сили", s$party) ] = "ODS"
s$party[ grepl("Bulgarian People's Union|Български Народен Съюз", s$party) ] = "BNS"

# English party names, the lazy way

s$partyname = NA
s$partyname[ s$party == "A" ] = "Ataka"
s$partyname[ s$party == "DPS" ] = "Movement for Rights and Freedoms"
s$partyname[ s$party == "KB" ] = "Coalition for Bulgaria"
s$partyname[ s$party == "GERB" ] = "Citizens for European Development of Bulgaria"
s$partyname[ s$party == "DSB" ] = "Democrats for Strong Bulgaria"
s$partyname[ s$party == "NMS" ] = "National Movement Simeon the Second"
s$partyname[ s$party == "RZS" ] = "Order, Lawfulness, Justice"
s$partyname[ s$party == "SK" ] = "Blue Coalition"
s$partyname[ s$party == "ODS" ] = "United Democratic Forces"
s$partyname[ s$party == "BNS" ] = "Bulgarian People's Union"

# all sponsors recognized
stopifnot(all(unique(unlist(strsplit(m$authors, ";"))) %in% s$url))

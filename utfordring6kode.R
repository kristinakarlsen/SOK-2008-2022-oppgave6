# pakkene
library(tidyverse)
library(rjstat)
library(tidyverse)
library(httr)
library(PxWebApiData)
library(ggplot2)
library(dplyr)
library(patchwork)


# data for sykefraværet
url_sykefravar <- "https://data.ssb.no/api/v0/no/table/12441/"

data <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99"
        ]
      }
    },
    {
      "code": "Sykefraver2",
      "selection": {
        "filter": "item",
        "values": [
          "Alt"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
data <- POST(url_sykefravar , body = data, encode = "json", verbose())
data <- fromJSONstat(content(data, "text"))


#A data for arbeidsledighet

url_arbeidsledighet <- "https://data.ssb.no/api/v0/no/table/05111/"
data2 <- '{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
data2 <- POST(url_arbeidsledighet , body = data2, encode = "json", verbose())
data2 <- fromJSONstat(content(data2, "text"))

# lage et nytt datasett 
df <- merge(data,data2, by = c("år", "kjønn"))
# for menn
df_menn <- df %>% filter(kjønn == "Menn") 
#for kvinner
df_kvinner <- df %>% filter(kjønn == "Kvinner")

df_menn$år <- as.numeric(as.character(df_menn$år))
df_kvinner$år <- as.numeric(as.character(df_kvinner$år))


#kvinner

# koeffisient 
coeffk = 2

P_kvinner <- df_kvinner%>% 
  ggplot(aes(x=år, y=value.y)) +
  geom_line(color = "purple") +
  geom_line(aes(y = value.x/coeffk), color = "deeppink") +
  scale_y_continuous("Arbeidsledighet i %", sec.axis = sec_axis(~.*coeffk, name = "Sykefravær i %")) + 
  scale_x_continuous("År", breaks = 2005:2019) +
  theme(axis.line.y.right = element_line(color = "deeppink"), 
        axis.ticks.y.right = element_line(color = "deeppink"),
        axis.text.y.right = element_text(color = "deeppink"), 
        axis.title.y.right = element_text(color = "deeppink")) +
  theme(axis.line.y.left = element_line(color = "purple"), 
        axis.ticks.y.left = element_line(color = "purple"),
        axis.text.y.left = element_text(color = "purple"), 
        axis.title.y.left = element_text(color = "purple")) +
  ggtitle("Arbeidsledighet og sykefravær \nfor kvinner") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
P_kvinner

# koeffisient 
coff <- 1.5

# Menn
P_menn <- df_menn %>% 
  ggplot(aes(x=år, y=value.y)) +
  geom_line(color = "purple") +
  geom_line(aes(y = value.x/coff), color = "deeppink") +
  scale_y_continuous("Arbeidsledighet i %", sec.axis = sec_axis(~.*coff, name = "Sykefravær i %")) + 
  scale_x_continuous("År", breaks = 2005:2019) +
  theme(axis.line.y.right = element_line(color = "deeppink"), 
        axis.ticks.y.right = element_line(color = "deeppink"),
        axis.text.y.right = element_text(color = "deeppink"), 
        axis.title.y.right = element_text(color = "deeppink")) +
  theme(axis.line.y.left = element_line(color = "purple"), 
        axis.ticks.y.left = element_line(color = "purple"),
        axis.text.y.left = element_text(color = "purple"), 
        axis.title.y.left = element_text(color = "purple")) +
  ggtitle("Arbeidsledighet og sykefravær \nfor menn") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))
P_menn



# Plotte de sammen
P_kvinner + theme_classic() + P_menn + theme_classic()

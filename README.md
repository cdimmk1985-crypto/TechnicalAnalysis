# TechnicalAnalysis

Repo for my BDA400 (Data Science Tools and Techniques) project at CDI College. The project is split over three assignments, and each one gets its own folder.

## Folders

```
TechnicalAnalysis/
├── A02/                          # Assignment 2 - Preliminary Stage
│   ├── portfolio.txt             # stock symbols, one per line
│   ├── stock_functions.R         # functions for loading data, stats, display
│   ├── technical_analysis.Rmd    # runs everything and shows the output
│   ├── technical_analysis.html   # knitted version of the Rmd
│   ├── output/                   # CSV of each stock (created when you run it)
│   └── MahsalKhan_Marwah_CA_BDA400_A02.docx   # report with screenshots
└── README.md
```

## Assignment 2 - Preliminary Stage

Reads the stock symbols from `portfolio.txt`, downloads a year of daily prices (2025) for each one from Yahoo Finance with `quantmod`, calculates basic stats (mean, median, mode, standard deviation, moving averages), and shows the data as tables and charts.

Portfolio: AAPL, MSFT, NVDA, AMZN, TSLA

### Packages needed

```r
install.packages(c("quantmod", "TTR", "knitr", "rmarkdown"))
```

### How to run

Open `A02/technical_analysis.Rmd` in RStudio and click **Knit**. The working directory has to be the `A02` folder so it can find `portfolio.txt` and `stock_functions.R` (knitting does this automatically).

You need an internet connection since the data is downloaded from Yahoo when it runs.

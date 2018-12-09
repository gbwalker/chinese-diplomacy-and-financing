# Elite Chinese Diplomacy and Financial Flows (2000-2018)

## Introduction

In some places, Chinese aid and investment is welcomed with open arms. In others, it furrows brows. Xi Jinping's Belt and Road Initiative in particular has prompted a [polarizing debate](http://explore.tandfonline.com/page/pgas/1b1r) about the nature and intention of Chinese capital flows. Are concessional loans and large-scale investments in developing nations a path to geopolitical influence? And how do the diplomatic activities of top leaders align with financial flows? This project aims to provide users with a new way to visualize and probe China's growing multilateralism and global reach.

## The App

[Elite Chinese Diplomacy and Financial Flows](https://gbwalker.shinyapps.io/chinese-diplomacy-and-financing/) on Shiny Apps.

## Methodology

A few points to note:

* The scraper (china_vitae_scraper.R) extracts the names of every foreign country (and associated text) mentioned in the activity records of seven Chinese officials: current president Xi Jinping, premier Li Keqiang, and foreign minister Wang Yi, and former president Hu Jintao, premier Wen Jiabao, and foreign ministers Yang Jiechi and Li Zhaoxing. I attempt to be comprehensive but it is unlikely perfect. 
* I count only the engagements officials had while occupying the above-stated positions. For example, I ignore all encounters Xi Jinping had while serving as vice president. Counting interactions by position (president, premier, foreign minister) instead of individual keeps the observations consistent across time.
* I do not double-count diplomatic engagements with one country on one single day. For example, if Xi Jinping traveled to the U.S. on January 1 *and* met with the U.S. president on January 1, it would only count as one "engagement."
* I calculate yearly rates of engagement and yearly number of aid projects/investments by dividing the total by the number of observation years within an observation period. For example, if there were 4 aid projects in 2013-2014, the per-year rate for the 2013-2018 period would be 2, since the AidData observations only cover up to 2014. If there were 20 projects in 2008-12, the per-year rate would be 4, since AidData covers all five years.

## Sources

**Engagements**: I scraped data from the [China Vitae](http://www.chinavitae.com/) project at
the Carnegie Endowment for International Peace.

**Investments**: From the [China Global Investment Tracker](http://www.aei.org/china-global-investment-tracker/), a project
published by the American Enterprise Institute.

**Aid**: From the [AidData research](https://www.aiddata.org/china) lab at William & Mary. Specifically, I use 
"[AidData's Geocoded Global Chinese Official Finance, Version 1.1.1](https://www.aiddata.org/data/geocoded-chinese-global-official-finance-dataset)"
from September 2018.

Many thanks to those who collected and provided the data publicly and free of charge.

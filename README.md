# Economic-Impact-of-Automation-US
I found a great data set linking US job data with the probability of automation in the near future. on data.world and wanted to explore. 

TLDR using these numbers, I estimate automation will displace 75 million US jobs and $25 trillion in annual income. Nevada and the Dakota will be heavily impacted, Washington DC will be least impacted. Automation will disproportionately affect jobs with low income.

Until I figure out how to get Rmd working in github, there's a working version of the code posted here:
https://data.world/jimmyjarnold/the-robots-are-coming-who-where-and-how-much

![JobMap](https://github.com/jimmyjamesarnold/Economic-Impact-of-Automation-US/blob/master/choropleth-Percent-Jobs-Automation-US.png)

Jobs in retail and service sectors will displace the most workers. 

![Workers](https://github.com/jimmyjamesarnold/Economic-Impact-of-Automation-US/blob/master/Jobs%20with%20Highest%20Worker%20Displacement.JPG)

Managers and meds will be the highest paying jobs with lowest risk of automation.

![LowRisk](https://github.com/jimmyjamesarnold/Economic-Impact-of-Automation-US/blob/master/Low%20Risk%20High%20Salary.JPG)

It appears that automation will disproportionately affect jobs with lower median annual incomes.

![LowRisk](https://github.com/jimmyjamesarnold/Economic-Impact-of-Automation-US/blob/master/Automation%20Risk%20by%20Median%20Annual%20Income.JPG)
Risk groups:
low = prob < 0.1
mid = prob >= 0.1 | <= 0.9
high = prob > 0.9

Mann-Whitney U test between high risk and low risk groups yielded W = 3512.5, p-value < 2.2e-16.
There's a lot of ways this could be interpreted, but I think we can expect continued thinning of the 'middle class'. 

I'm making a tableau viz of the state-level impact per occupation to be posted on Labor Day. 

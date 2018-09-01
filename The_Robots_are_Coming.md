The Robots are Coming: Who, Where and How Much?
================
Jimmy James Arnold

For this project I'm using a fascinating dataset from data.world, with analysis and viz in R Studio.

Thanks to @wnedds and @quanticdata for posting this project.

The data sources are: Probability of Automation: <http://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf> - from 2013 Job Numbers: Bureau of Labor Statistics. Note: Jobs where data was not available or there were less than 10 employees were marked as zero. Salary data: <https://www.bls.gov/oes/current/oes_nat.htm>

First, let's get the data:

``` r
library(dplyr) # I love the tidyverse, thank you Hadley Wickham and co.
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(data.world)
```

    ## Loading required package: dwapi

    ## 
    ## Attaching package: 'dwapi'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     sql

``` r
# you'll need your own API token to run this part. It's cool, they're free.
#data.world::set_config(saved_cfg)
sql_stmt <- qry_sql("SELECT --This selects the fields listed below for analysis.
       national_dl.a_median,
       raw_state_automation_data.soc,
       raw_state_automation_data.occupation,
       raw_state_automation_data.probability,
       raw_state_automation_data.alabama,
       raw_state_automation_data.alaska,
       raw_state_automation_data.arizona,
       raw_state_automation_data.arkansas,
       raw_state_automation_data.california,
       raw_state_automation_data.colorado,
       raw_state_automation_data.connecticut,
       raw_state_automation_data.delaware,
       raw_state_automation_data.district_of_columbia,
       raw_state_automation_data.florida,
       raw_state_automation_data.georgia,
       raw_state_automation_data.hawaii,
       raw_state_automation_data.idaho,
       raw_state_automation_data.illinois,
       raw_state_automation_data.indiana,
       raw_state_automation_data.iowa,
       raw_state_automation_data.kansas,
       raw_state_automation_data.kentucky,
       raw_state_automation_data.louisiana,
       raw_state_automation_data.maine,
       raw_state_automation_data.maryland,
       raw_state_automation_data.massachusetts,
       raw_state_automation_data.michigan,
       raw_state_automation_data.minnesota,
       raw_state_automation_data.mississippi,
       raw_state_automation_data.missouri,
       raw_state_automation_data.montana,
       raw_state_automation_data.nebraska,
       raw_state_automation_data.nevada,
       raw_state_automation_data.new_hampshire,
       raw_state_automation_data.new_jersey,
       raw_state_automation_data.new_mexico,
       raw_state_automation_data.new_york,
       raw_state_automation_data.north_carolina,
       raw_state_automation_data.north_dakota,
       raw_state_automation_data.ohio,
       raw_state_automation_data.oklahoma,
       raw_state_automation_data.oregon,
       raw_state_automation_data.pennsylvania,
       raw_state_automation_data.rhode_island,
       raw_state_automation_data.south_carolina,
       raw_state_automation_data.south_dakota,
       raw_state_automation_data.tennessee,
       raw_state_automation_data.texas,
       raw_state_automation_data.utah,
       raw_state_automation_data.vermont,
       raw_state_automation_data.virginia,
       raw_state_automation_data.washington,
       raw_state_automation_data.west_virginia,
       raw_state_automation_data.wisconsin,
       raw_state_automation_data.wyoming
       FROM raw_state_automation_data JOIN national_dl ON national_dl.occ_code = raw_state_automation_data.soc")
dwapi::configure()
df <- data.world::query(sql_stmt, "quanticdata/occupation-and-salary-by-state-and-likelihood-of-automation")
head(df)
```

    ## # A tibble: 6 x 55
    ##   a_median soc   occupation probability alabama alaska arizona arkansas
    ##   <chr>    <chr> <chr>            <dbl>   <int>  <int>   <int>    <int>
    ## 1 59020    13-1~ Training ~       0.014    2670      0    7710     2740
    ## 2 51800    47-2~ Structura~       0.83     1340    180    1460      930
    ## 3 30570    47-3~ Helpers–B~       0.83      340      0     580      230
    ## 4 28810    47-3~ Helpers–C~       0.92      680    250     300      270
    ## 5 29530    47-3~ Helpers–E~       0.74     1950    220     560      300
    ## 6 27310    47-3~ Helpers–P~       0.94      220     60     300      280
    ## # ... with 47 more variables: california <int>, colorado <int>,
    ## #   connecticut <int>, delaware <int>, district_of_columbia <int>,
    ## #   florida <int>, georgia <int>, hawaii <int>, idaho <int>,
    ## #   illinois <int>, indiana <int>, iowa <int>, kansas <int>,
    ## #   kentucky <int>, louisiana <int>, maine <int>, maryland <int>,
    ## #   massachusetts <int>, michigan <int>, minnesota <int>,
    ## #   mississippi <int>, missouri <int>, montana <int>, nebraska <int>,
    ## #   nevada <int>, new_hampshire <int>, new_jersey <int>, new_mexico <int>,
    ## #   new_york <int>, north_carolina <int>, north_dakota <int>, ohio <int>,
    ## #   oklahoma <int>, oregon <int>, pennsylvania <int>, rhode_island <int>,
    ## #   south_carolina <int>, south_dakota <int>, tennessee <int>,
    ## #   texas <int>, utah <int>, vermont <int>, virginia <int>,
    ## #   washington <int>, west_virginia <int>, wisconsin <int>, wyoming <int>

Great, I have df containing median annual income (a\_median), the occupation code (soc) used to join the tables, the occupation name, the probability of a particular occupation getting automated, and the estimated \# of workers in occupation by state. It should be noted at the start that I'm using the median annual income here. Look at the source dataset, there's more detailed quartile values available, which could be great for getting more accurate estimates.

Before we start doing calculations, it's good to some exploratory data analysis (EDA).

First, I want to calculate the total number of workers in a given occupation across the US:

``` r
# oops, before I do that, the a_median is missing some data and got read in as a character vector. I gotta fix that. 
# some of the a_median rows are missing data, so I will replace with 0 for now
df <- transform(df, a_median = as.numeric(a_median))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs
    ## introduced by coercion

``` r
df$a_median[is.na(df$a_median)] <- 0

# sum over all states into new column 'total_US'
df$total_US=rowSums(df[,5:55])

# Let's look at some of the numbers associated with jobs in this data. First, let's do job frequency:
# Define new df for ease of handling
US_jobs = df[,c('occupation','probability','total_US','a_median')]
```

It might be informative to look at the histograms and ecdf curves and get a feel for the data.

``` r
# I want to plot several plots together, and I'll probably do it again later, so it's worth calling the multiplot function:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# histogram
hist(US_jobs$probability,
     main = 'Histogram of Risk of Automation',
     xlab = 'Risk of Automation')
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# Define ecdf plot
ecdf_plot <- function(x, title, xlab, ylab) {
  Fn <- ecdf(x)
  plot(x,Fn(x), xlab = xlab, ylab = ylab, main = title)
}
# call plot
ecdf_plot(US_jobs$probability,'Risk of Automation ECDF','Risk of Automation','ecdf')
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-3-2.png)

The probability is skewed to the extremes, with very little in between. I don't know how realistic that is, so it'll hard to draw concrete conclusions.

``` r
# Get occupations with highest risk of automation, look at the total workers across US, and the median annual income:
high_risk = US_jobs %>%
  arrange(desc(probability)) %>%
  slice(1:10)
high_risk
```

    ##                                                       occupation
    ## 1                                                Watch Repairers
    ## 2                                         Insurance Underwriters
    ## 3                                                   Sewers; Hand
    ## 4                                                  Tax Preparers
    ## 5  Photographic Process Workers and Processing Machine Operators
    ## 6                                       Mathematical Technicians
    ## 7                    Title Examiners; Abstractors; and Searchers
    ## 8                                            Library Technicians
    ## 9                                                  Telemarketers
    ## 10                                           New Accounts Clerks
    ##    probability total_US a_median
    ## 1         0.99     1090    36740
    ## 2         0.99    90310    67680
    ## 3         0.99     5170    24520
    ## 4         0.99    63600    36550
    ## 5         0.99    26200    26470
    ## 6         0.99      220    49660
    ## 7         0.99    51810    45800
    ## 8         0.99    93400    32890
    ## 9         0.99   214570    24300
    ## 10        0.99    41430    34990

Jobs with the highest probability of being automated are technical, but repetitive. I don't see a clear trend with respect to job freqency or income.

``` r
# and jobs with the lowest risk:
low_risk = US_jobs %>%
  arrange(probability) %>%
  slice(1:10)
low_risk
```

    ##                                                        occupation
    ## 1                                         Recreational Therapists
    ## 2  First-Line Supervisors of Mechanics; Installers; and Repairers
    ## 3                                  Emergency Management Directors
    ## 4                Mental Health and Substance Abuse Social Workers
    ## 5                                                    Audiologists
    ## 6                                       Healthcare Social Workers
    ## 7                                         Occupational Therapists
    ## 8                                     Orthotists and Prosthetists
    ## 9                                 Oral and Maxillofacial Surgeons
    ## 10 First-Line Supervisors of Fire Fighting and Prevention Workers
    ##    probability total_US a_median
    ## 1       0.0028    18080    46410
    ## 2       0.0030   453360    63540
    ## 3       0.0030     9540    70500
    ## 4       0.0031   111370    42700
    ## 5       0.0033    11830    75980
    ## 6       0.0035   159340    53760
    ## 7       0.0035   118060    81910
    ## 8       0.0035     6260    65630
    ## 9       0.0036     3450        0
    ## 10      0.0036    56980    74540

A lot of the jobs with lowest probability of being automated are technical and involve human interaction, like social workers and therapists. The salaries in this group tend to be better paid at &gt;50k, while the high risk jobs are mostly &lt;50k.

Let's look at the job freq numbers:

``` r
# Job Frequency:
# histogram
hist(US_jobs$total_US, 
     main = 'Histogram of Job Frequency',
     xlab = 'Job Frequency')
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# call ecdf plot
ecdf_plot(US_jobs$total_US,'Job Frequency ECDF','Median Annual Income','ecdf')
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-6-2.png)

The frequency of job types are strongly skewed to the right. There's a few jobs with &gt;2 million people, but the vast majority look to be around 100,000. Specialization in action.

``` r
# Get occupations with most workers
common_jobs = US_jobs %>%
  arrange(desc(total_US)) %>%
  slice(1:10)
common_jobs
```

    ##                                                                         occupation
    ## 1                                                              Retail Salespersons
    ## 2                                                                         Cashiers
    ## 3               Combined Food Preparation and Serving Workers; Including Fast Food
    ## 4                                                           Office Clerks; General
    ## 5                                                 Customer Service Representatives
    ## 6                           Laborers and Freight; Stock; and Material Movers; Hand
    ## 7                                                           Waiters and Waitresses
    ## 8  Secretaries and Administrative Assistants; Except Legal; Medical; and Executive
    ## 9                                                  General and Operations Managers
    ## 10                   Janitors and Cleaners; Except Maids and Housekeeping Cleaners
    ##    probability total_US a_median
    ## 1         0.92  4528570    22680
    ## 2         0.97  3540980    20180
    ## 3         0.92  3426090    19440
    ## 4         0.96  2955560    30580
    ## 5         0.55  2707030    32300
    ## 6         0.85  2587940    25980
    ## 7         0.94  2564620    19990
    ## 8         0.96  2295480    34820
    ## 9         0.16  2188870    99310
    ## 10        0.66  2161710    24190

A lot of the most common jobs have a very high probability of being automated and low median income. A lot of service jobs.

``` r
# and jobs with the least workers
uncommon_jobs = US_jobs %>%
  arrange(total_US) %>%
  slice(1:10)
uncommon_jobs
```

    ##                                occupation probability total_US a_median
    ## 1            Computer Support Specialists      0.6500        0    52160
    ## 2                  Postsecondary Teachers      0.0320        0    66500
    ## 3                         Prosthodontists      0.0550        0   126050
    ## 4                 Physicians and Surgeons      0.0042        0        0
    ## 5      Miscellaneous Agricultural Workers      0.8700        0    22520
    ## 6                Cooks; Private Household      0.3000       30    32060
    ## 7     Fishers and Related Fishing Workers      0.8300       40    27110
    ## 8  Timing Device Assemblers and Adjusters      0.9800      130    37040
    ## 9                Mathematical Technicians      0.9900      220    49660
    ## 10                     Model Makers; Wood      0.9600      280    40890

It's not clear to me why there are 0 workers for some of these jobs. For the the physicans and surgeons, I think they are paid hourly and thus the BLS doesn't have a median annual income. I don't see a clear trend between job rarity and risk of automation and median salary.

Let's look at median annual incomes:

``` r
# histogram
hist(US_jobs$a_median,
     main = 'Histogram of Median Annual Income',
     xlab = 'Median Annual Income')
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# call ecdf plot
ecdf_plot(US_jobs$a_median,'Median Annual Income ECDF','Median Annual Income','ecdf')
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-9-2.png)

The median incomes look right skewed, which sounds right. Most jobs pay less than $50k, few jobs pay &lt;150k. The ecdf is interesting, you can see the minimum wage effect as there's no median annual between $0-15k or so.

``` r
# Let's find which jobs have the highest median annual income
high_income = US_jobs %>%
  arrange(desc(a_median)) %>%
  slice(1:10)
high_income
```

    ##                                        occupation probability total_US
    ## 1                                Chief Executives      0.0150   223270
    ## 2                               Dentists; General      0.0044   105650
    ## 3       Computer and Information Systems Managers      0.0350   352540
    ## 4          Architectural and Engineering Managers      0.0170   177540
    ## 5                              Marketing Managers      0.0140   205900
    ## 6                             Petroleum Engineers      0.1600    31510
    ## 7  Airline Pilots; Copilots; and Flight Engineers      0.1800    66850
    ## 8                                 Prosthodontists      0.0550        0
    ## 9      Judges; Magistrate Judges; and Magistrates      0.4000    24720
    ## 10                                    Podiatrists      0.0046     9320
    ##    a_median
    ## 1    181210
    ## 2    153900
    ## 3    135800
    ## 4    134730
    ## 5    131180
    ## 6    128230
    ## 7    127820
    ## 8    126050
    ## 9    125880
    ## 10   124830

CEOs, dentists, and STEM managers, sounds about right. Also, most of them have very low risk of being automated.

``` r
# and which jobs have the lowest median annual income
low_income = US_jobs %>%
  arrange(a_median) %>%
  slice(1:10)
low_income
```

    ##                                                            occupation
    ## 1                                                              Actors
    ## 2                                                             Dancers
    ## 3                                               Musicians and Singers
    ## 4                                     Oral and Maxillofacial Surgeons
    ## 5                                                       Orthodontists
    ## 6                                             Physicians and Surgeons
    ## 7                                                      Gaming Dealers
    ## 8  Combined Food Preparation and Serving Workers; Including Fast Food
    ## 9                                                          Shampooers
    ## 10                                                   Cooks; Fast Food
    ##    probability total_US a_median
    ## 1       0.3700    46770        0
    ## 2       0.1300     6840        0
    ## 3       0.0740    38860        0
    ## 4       0.0036     3450        0
    ## 5       0.0230     3800        0
    ## 6       0.0042        0        0
    ## 7       0.9600    83170    19290
    ## 8       0.9200  3426090    19440
    ## 9       0.7900    14390    19700
    ## 10      0.8100   513200    19860

I think artists don't get a regular paycheck, but rather get paid per gig, which explain why the NBL doesn't have an estimate for median annual income. Either that or the median is 0 and there's a bunch of starving artists. Food prep workers are not highly paid, that sounds right. Also, check out the probability of automation for dealers, interesting.

Taken together, it would seems there might be a relationship between automation risk and income, and possibly \# of workers. This can evaluated visually with some simple plots. First, let's look at probability and income.

``` r
# Make scatter plot comparing probability of automation and income:
# using raw data
ggplot(US_jobs, aes(x=probability, y=a_median)) + 
  geom_point() +
  theme_classic() + 
  labs(title = "US Jobs - Automation Risk by Median Annual Income", y = "Median Annual Income ($)", x = "Probability Automation Risk")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-12-1.png)

We knew from before that the probability data is highly bimodal, and the income was right skewed. That being said, it looks like the values on the right are lower than those on the left. I'm going to try plotting the salary by percent rank and see if that gives some clarity:

``` r
# percent rank each variable for scaling
US_jobs = US_jobs %>%
  mutate(p_job_freq = percent_rank(total_US)) %>%
  mutate(p_income = percent_rank(a_median))
  
# using percent ranked income:
ggplot(US_jobs, aes(x=probability, y=p_income)) + 
  geom_point() +
  theme_classic() + 
  labs(title = "US Jobs - Automation Risk by Median Annual Income", y = "Percentile Median Annual Income", x = "Probability Automation Risk")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-13-1.png)

Looking at income as percent ranked, it looks clear that a lot of those with higher salaries also have lower risk of probability. This is probably the high skill/ high pay crowd. I can't help but feel that this chart indicates the divide between 'haves' and 'have nots' will likely widen as automation is adopted.

Those with lower risk of automation appear to have higher median annual incomes, a testable hypothesis. Instead of running a regression, it may be easier to interpret if I divide the samples into groups and compare. I know from the histogram that the data is bimodal, and I really want to compare the high risk and low risk groups, so I'm going to define 3 groups, and compare the high and low risk group respectively.

But first, Let's look at the relationship between risk of automation and job frequency:

``` r
# Make scatter plot comparing probability of automation and income:
# using raw data
ggplot(US_jobs, aes(x=probability, y=total_US)) + 
  geom_point() +
  theme_classic() + 
  labs(title = "US Jobs - Automation Risk by Job Title Frequency", y = "Job Title Frequency", x = "Probability Automation Risk")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-14-1.png)

I think job frequency and risk of automation are independent variables. That being, it does appear that several of the most frequent jobs (&gt;2E6) have a high risk of automation, which could have significant economic displacement.

Relationship between income and job frequency:

``` r
# using raw data
ggplot(US_jobs, aes(x=total_US, y=a_median)) + 
  geom_point() +
  theme_classic() + 
  labs(title = "US Jobs - Job Title Frequency by Median Annual Income", x = "Job Title Frequency", y = "Median Annual Income")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-15-1.png)

I already knew from the histograms that both of these features were right skewed, and that is obvious here.

Now I'm going to add an extra layer of info here by coloring the top and bottom tenths of risk of automation.

``` r
# engineer new features: 
US_jobs$risk_group = 'mid'
US_jobs$risk_group[US_jobs$probability < 0.1] ='low'
US_jobs$risk_group[US_jobs$probability > 0.9] = 'high'
US_jobs$risk_group <- as.factor(US_jobs$risk_group)

# plot with fill
ggplot(US_jobs, aes(x=total_US, y=a_median, fill=risk_group)) + 
  geom_point(aes(color=risk_group)) +
  scale_fill_viridis(discrete=T) +
  theme_classic() + 
  labs(title = "US Jobs - Job Title Frequency by Median Annual Income", x = "Job Title Frequency", y = "Median Annual Income")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-16-1.png)

Hmm, the custom color isn't working, but even so it looks like the low risk of automation jobs tend to have higher salaries. A boxplot would visualize this better.

``` r
# using raw data
ggplot(US_jobs, aes(x=reorder(risk_group, probability), y=a_median, fill=risk_group)) + 
  geom_boxplot() +
  scale_fill_viridis(discrete=T) +
  theme_classic() + 
  labs(title = "US Jobs - Automation Risk Group by Median Annual Income", x = "Automation Risk Group", y = "Median Annual Income ($)")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-17-1.png)

Although there are outliers, it appears that jobs with greater than 90% risk of automation (high) have lower median annual incomes than those with less than 10% risk of automation (low). Because the data isn't normally distributed, we can test the hypothesis using the non-parametric Mann-Whitney U test.

``` r
test = US_jobs[which(US_jobs$risk_group!='mid'),] # drop mid group
wilcox.test(test$a_median~test$risk_group)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  test$a_median by test$risk_group
    ## W = 3512.5, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

Boom, reject the null hypothesis! Those with jobs least affected by automation are earning more than those who will be displaced. It's interesting, and indicates there are some objectively wise career choices one should consider. High pay and non-automatable, those are jobs to pursue at the present moment. I'll figure out exactly what those are in a little bit.

But first, It's not all doom and gloom for those getting displaced. It's reasonable to assume that as automation takes over low-end jobs, new opportunities will emerge. However, there's likely to be some 'turbulance' as displaced workers figure out the new economy.

I want to estimate the scale of economic disruption. How many US jobs are at risk, all told? And how much current income will be displaced by automation? Let's find out:

``` r
# First, I'm going to calculate a few new columns to answer these questions
# 1) the # of US jobs at risk, by profession
df$jobs_at_risk = df$probability * df$total_US
df$current_income = df$total_US * df$a_median
df$income_at_risk = df$jobs_at_risk * df$a_median

# save and print the sums
total_jobs = sum(df$total_US)
print(c('Current number of jobs:',total_jobs))
```

    ## [1] "Current number of jobs:" "125099130"

``` r
total_jobs_at_risk = sum(df$jobs_at_risk)
print(c('Estimated number of automatable jobs:',total_jobs_at_risk))
```

    ## [1] "Estimated number of automatable jobs:"
    ## [2] "75757157.488"

``` r
print(c('% Current jobs displaced:',total_jobs_at_risk*100/total_jobs))
```

    ## [1] "% Current jobs displaced:" "60.5577013109524"

Based on these estimates 75,757,158 US jobs can be automated (~60% of all current jobs). This seems really high.

Let's look at how much money will be displaced:

``` r
# Next, calculate the total income and income at risk
total_income = sum(df$current_income)
print(c('Current income:',total_income))
```

    ## [1] "Current income:" "5457014122300"

``` r
total_income_at_risk = sum(df$income_at_risk)
print(c('Estimated income displaced by automation:',total_income_at_risk))
```

    ## [1] "Estimated income displaced by automation:"
    ## [2] "2572185783759.71"

``` r
print(c('% of current annual income displaced by automation:',total_income_at_risk*100/total_income))
```

    ## [1] "% of current annual income displaced by automation:"
    ## [2] "47.135406398318"

Based on these estimates $25 Trillion (~47% of cumulative current annual incomes) will be displaced. That's almost half. That's huge.

That's going to have a big impact on taxes, as well as the economy at large. If people don't have money to spend, how will there be demand? If people aren't working, they can't payback taxes and debt. That's going to cause a lot of turbulance.

Yeesh, let's keep digging. I want to revist the question about which jobs are going to continue to do well, as well as the jobs that are going to get hit hardest.

Because the data is broken by state and occupation, I'd like to look at: 1) which jobs are going be most disrupted, and which will remain stable? 2) which states are going to be affected most and least by automation? 3) which jobs in each state are going to get hardest hit?

1.  which jobs are going be most disrupted, and which will remain stable? There's a couple ways to ask this. For now, I want to focus on number of jobs displaced.

<!-- -->

1.  Across total US, which jobs will displace most workers?
2.  And how much money will that displace?
3.  which will be most least affected? What will be the highest paying among those?

First, I'll make use of the jobs\_at\_risk variable I made up above.

``` r
risky_jobs = df %>%
  mutate(job_p = percent_rank(jobs_at_risk)) %>%
  arrange(desc(job_p)) %>%
  slice(1:10)

# plot top jobs and associated income
jobs <- ggplot(risky_jobs, aes(x = reorder(occupation, jobs_at_risk), y = jobs_at_risk, fill = jobs_at_risk)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Most Jobs Displaced", y = "Jobs at Risk", x = "Occupation")

income <- ggplot(risky_jobs, aes(x = reorder(occupation, jobs_at_risk), y = income_at_risk, fill = jobs_at_risk)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Associated Income", y = "Annual Income Displaced ($)", x = "Occupation")

multiplot(jobs, income)
```

    ## Loading required package: grid

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
# x axes should be improved (scale, add M or B respectively)
```

How does it change when we look for jobs that are going to displace the most income? And how many jobs will that displace?

``` r
risky_income = df %>%
  mutate(income_p = percent_rank(income_at_risk)) %>%
  arrange(desc(income_p)) %>%
  slice(1:10)

# plot top incomes and associated jobs
income <- ggplot(risky_income, aes(x = reorder(occupation, income_at_risk), y = income_at_risk, fill = income_at_risk)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Top Income Displacement", y = "Annual Income Displaced ($)", x = "Occupation")

jobs <- ggplot(risky_income, aes(x = reorder(occupation, income_at_risk), y = jobs_at_risk, fill = income_at_risk)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Associated Jobs", y = "Jobs at Risk", x = "Occupation")

multiplot(income, jobs)
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-22-1.png)

1.  Taken together, this suggests point-of-sales jobs like retail and cashiers will be most disrupted. This sounds reasonable, brick and mortar stores are dying and Amazon and Alibaba are thriving.

I think it's interesting that when we look at it by income, some 'white collar' jobs show up, like Accountants and Auditors. That's not surprising, software will make it easier to track process data.

Next, I'd like to look at which jobs are predicted to have low risk of automation and high pay:

``` r
stable_jobs = df %>%
  filter(probability < 0.1 & total_US > 10000) %>% #added a filter to remove exceedingly rare jobs
  arrange(desc(a_median)) %>%
  slice(1:10)

# plot top jobs and associated income
jobs <- ggplot(stable_jobs, aes(x = reorder(occupation, a_median), y = jobs_at_risk, fill = a_median)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Low Automation, High-Salary Jobs", y = "Jobs at Risk", x = "Occupation")

income <- ggplot(stable_jobs, aes(x = reorder(occupation, a_median), y = a_median, fill = a_median)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Median Annual Income", y = "Median Annual Income", x = "Occupation")

multiplot(jobs, income)
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-23-1.png)

Nearly every entry on this list requires a 4-year degree, if not a graduate degree. TL;DR - Management or Medicine, if you're planning on going to college then that's what you want to aim for. That's probably been true for at least 20 years, so not that surprising. I would have thought more tech jobs would be up there.

1.  which states are going to be affected most and least by automation?

I want to see how the displacement of jobs and income will be distributed across the US, by state:

``` r
# get jobs per state
st_jobs = df[,c(5:55)]
# calculate income per state
st_income = st_jobs * df$a_median
# calculate jobs at risk per state
st_jobs_at_risk = st_jobs * df$probability
# calculate income at risk per state
st_income_at_risk = st_jobs_at_risk * df$a_median

# label function
stateLabels = function(x){
  rownames(x) = df$occupation
  # transpose for mapping and add state code
  x = data.frame(t(x))
  x = cbind(code = c( "AL",
        "AK",
        "AZ",
        "AR",
        "CA",
        "CO",
        "CT",
        "DE",
        "DC",
        "FL",
        "GA",
        "HI",
        "ID",
        "IL",
        "IN",
        "IA",
        "KS",
        "KY",
        "LA",
        "ME",
        "MD",
        "MA",
        "MI",
        "MN",
        "MS",
        "MO",
        "MT",
        "NE",
        "NV",
        "NH",
        "NJ",
        "NM",
        "NY",
        "NC",
        "ND",
        "OH",
        "OK",
        "OR",
        "PA",
        "RI",
        "SC",
        "SD",
        "TN",
        "TX",
        "UT",
        "VT",
        "VA",
        "WA",
        "WV",
        "WI",
        "WY"),x)
  x
}

# label dfs
st_jobs = stateLabels(st_jobs)
st_income = stateLabels(st_income)
st_jobs_at_risk = stateLabels(st_jobs_at_risk)
st_income_at_risk = stateLabels(st_income_at_risk)

# calculate total jobs by state
st_jobs$total <- rowSums(st_jobs[,2:689])
# calculate total jobs at risk by state
st_jobs_at_risk$total <- rowSums(st_jobs_at_risk[,2:689])
# calculate total income by state
st_income$total <- rowSums(st_income[,2:689])
# calculate total income at risk by state
st_income_at_risk$total <- rowSums(st_income_at_risk[,2:689])
# calculate scaled job risk (jobs at risk/total jobs per state), do element-wise division. This yields the % of state workforce at risk, indicating which jobs will be most displaced by state, and which states will have most displacement by job
st_jobs_risk_scaled = cbind(st_jobs$code, 100*st_jobs_at_risk[,2:690]/ st_jobs$total)
# calculate scaled income risk (income at risk/total jobs per state), do element-wise division. This yields the median annual income/current income at risk, indicating the relative economic impact of automation.
st_income_risk_scaled = cbind(st_jobs$code, 100*st_income_at_risk[,2:690]/ st_income$total)

total = data.frame(cbind(st_jobs$code,st_jobs$total,st_jobs_at_risk$total,st_income$total,st_income_at_risk$total))
colnames(total) = c('current_jobs', 'jobs_at_risk', 'current_income', 'income_at_risk')
rownames(total) = rownames(st_jobs)

# saving dfs for tableau work later
write.csv(st_jobs, "current state jobs.csv")
write.csv(st_income, "current state income.csv")
write.csv(st_jobs_at_risk, "state jobs at risk.csv")
write.csv(st_income_at_risk, "state income at risk.csv")
write.csv(st_jobs_risk_scaled, "state jobs at risk - scaled to current state jobs.csv")
write.csv(st_income_risk_scaled, "state income at risk - scaled to current state jobs.csv")
write.csv(total, "state totals.csv")
```

OK, now I've made my data, it's time for EDA. I'm going to start with the jobs at risk, and I'm going to use the scaled data. But first, let's get some summary stats.

``` r
# Now let's do some stats on the relative impact:
print(c('Median % of US Workforce Displacement:',median(st_jobs_risk_scaled$total)))
```

    ## [1] "Median % of US Workforce Displacement:"
    ## [2] "61.3095593010991"

``` r
print(c('Minimum % of Workforce Displaced:',min(st_jobs_risk_scaled$total)))
```

    ## [1] "Minimum % of Workforce Displaced:" "44.5093591967443"

``` r
print(c('Place with lowest workforce displacement:',rownames(st_jobs_risk_scaled)[st_jobs_risk_scaled$total == min(st_jobs_risk_scaled$total)]))
```

    ## [1] "Place with lowest workforce displacement:"
    ## [2] "district_of_columbia"

``` r
print(c('Maximum % of US Workforce Displaced:',max(st_jobs_risk_scaled$total)))
```

    ## [1] "Maximum % of US Workforce Displaced:"
    ## [2] "66.4547518066372"

``` r
print(c('Place with highest workforce displacement:',rownames(st_jobs_risk_scaled)[st_jobs_risk_scaled$total == max(st_jobs_risk_scaled$total)]))
```

    ## [1] "Place with highest workforce displacement:"
    ## [2] "nevada"

61% sounds really high. I remember reading a McKinnsey study saying which estimated around 47%.

Also, notice the place least affected is DC at 44%. What is this, the Hunger Games? Rationally speaking, government officials and lobbyists tend to perform highly specialized work which depends on personal connections. That being said, my first impression is that it seems ironic that the people with the most power already will be least affected.

NV is the hardest hit, with 66% of its workforce. I would hazard a guess that a lot of the service industry will be automated, meaning a lot of casino, hotel, and restaurant workers will be displaced.

Let's get an idea of how this is distributed. A simple bar plot might help get an idea of how the data looks.

``` r
# plot workforce displacement by state code
ggplot(st_jobs_risk_scaled, aes(x = reorder(st_jobs$code, total), y = total, fill = total)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Estimated % Workforce Displacement by Location", y = "% Workforce Displaced", x = "Location")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-26-1.png)

It lOoks like NV and SD will have the largest % of their workforce displaced, and DC is a bit of an outlier.

I can get the geographic distribution by mapping it with plotly.

``` r
# load plotly user name and apikey if saving to plotly.
# call choropleth, plot scaled % jobs at risk of automation
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))

p <- plot_geo(st_jobs_risk_scaled, locationmode = 'USA-states') %>% 
  add_trace(
    z = ~total,
    locations = ~st_jobs$code,
    color = ~total,
    colors = viridis_pal(alpha = 1, begin = 0, end = 1, direction = -1,
  option = "B")(20)
    ) %>%
  colorbar(title = "% Workforce") %>%
  layout(
    title = 'Estimated % Workforce At Risk of Automation',
    geo = g
  )
p
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-73c154cbfd5a89fd5362">{"x":{"visdat":{"38881c195299":["function () ","plotlyVisDat"]},"cur_data":"38881c195299","attrs":{"38881c195299":{"locationmode":"USA-states","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"z":{},"locations":{},"color":{},"colors":["#FCFFA4FF","#F1ED6FFF","#F7D340FF","#FBB91FFF","#FCA007FF","#F8870EFF","#F17020FF","#E55C30FF","#D64B40FF","#C43C4EFF","#B1325AFF","#9C2964FF","#87216BFF","#711A6EFF","#5C126EFF","#460B6AFF","#300A5BFF","#190C3EFF","#08051EFF","#000004FF"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"mapType":"geo","scene":{"zaxis":{"title":"total"}},"geo":{"domain":{"x":[0,1],"y":[0,1]},"scope":"usa","projection":{"type":"albers usa"},"showlakes":true,"lakecolor":"rgba(255,255,255,1)"},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5},"title":"Estimated % Workforce At Risk of Automation"},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"colorbar":{"title":"% Workforce","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","rgba(252,255,164,1)"],["0.529059900637807","rgba(176,50,91,1)"],["0.613235419737461","rgba(142,36,105,1)"],["0.655107143260773","rgba(125,30,108,1)"],["0.665864108363882","rgba(121,28,109,1)"],["0.677262513762278","rgba(116,27,110,1)"],["0.685714918638014","rgba(112,26,110,1)"],["0.707025757869597","rgba(104,22,110,1)"],["0.710889084521343","rgba(103,22,110,1)"],["0.734926995987123","rgba(93,18,110,1)"],["0.741597840650324","rgba(90,17,110,1)"],["0.750399361507536","rgba(86,16,109,1)"],["0.765545661588272","rgba(80,14,108,1)"],["0.774659106502599","rgba(76,13,107,1)"],["0.783272082420512","rgba(73,12,106,1)"],["0.79473475402451","rgba(68,11,104,1)"],["0.808516663866901","rgba(62,11,101,1)"],["0.813840133012869","rgba(60,11,99,1)"],["0.820428380870627","rgba(57,11,97,1)"],["0.831237629216475","rgba(53,10,94,1)"],["0.839803731427123","rgba(49,10,92,1)"],["0.851001997964746","rgba(44,11,86,1)"],["0.862504642519338","rgba(39,12,80,1)"],["0.885174341031885","rgba(29,12,67,1)"],["1","rgba(0,0,4,1)"]],"showscale":true,"locationmode":"USA-states","z":[63.9527378892231,60.1066144882774,59.6521567786534,62.0405881088959,59.3249770071362,58.8553957634551,56.7698941304053,60.7899652356698,44.5093591967443,63.271023819826,60.7542636324377,61.2852185209741,61.4297236734971,59.4631278307791,63.7381748852527,62.2694576087275,61.6935689914578,63.4706298816586,62.9967931364864,60.9772361266365,56.0606857303907,54.5280029584353,61.4963468708336,59.2648331933949,63.2476451596831,62.5607410454114,62.9292336211401,62.4944539517866,66.4547518066372,60.6690193618097,59.0505961227415,59.9689878118587,57.9517941569493,61.7235904376961,62.2441334210526,60.9764161978805,61.3095593010991,60.5434245677346,61.6548735541027,60.0655522003861,62.9441558866783,65.5850439859456,62.4233474769194,61.9199711564198,60.1118416438401,59.4381944969386,58.0433366863055,58.9775833382241,62.88723409102,62.3308668377418,62.5335102947113],"locations":["AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"],"type":"choropleth","marker":{"line":{"colorbar":{"title":"","ticklen":2},"cmin":44.5093591967443,"cmax":66.4547518066372,"colorscale":[["0","rgba(252,255,164,1)"],["0.529059900637807","rgba(176,50,91,1)"],["0.613235419737461","rgba(142,36,105,1)"],["0.655107143260773","rgba(125,30,108,1)"],["0.665864108363882","rgba(121,28,109,1)"],["0.677262513762278","rgba(116,27,110,1)"],["0.685714918638014","rgba(112,26,110,1)"],["0.707025757869597","rgba(104,22,110,1)"],["0.710889084521343","rgba(103,22,110,1)"],["0.734926995987123","rgba(93,18,110,1)"],["0.741597840650324","rgba(90,17,110,1)"],["0.750399361507536","rgba(86,16,109,1)"],["0.765545661588272","rgba(80,14,108,1)"],["0.774659106502599","rgba(76,13,107,1)"],["0.783272082420512","rgba(73,12,106,1)"],["0.79473475402451","rgba(68,11,104,1)"],["0.808516663866901","rgba(62,11,101,1)"],["0.813840133012869","rgba(60,11,99,1)"],["0.820428380870627","rgba(57,11,97,1)"],["0.831237629216475","rgba(53,10,94,1)"],["0.839803731427123","rgba(49,10,92,1)"],["0.851001997964746","rgba(44,11,86,1)"],["0.862504642519338","rgba(39,12,80,1)"],["0.885174341031885","rgba(29,12,67,1)"],["1","rgba(0,0,4,1)"]],"showscale":false,"color":[63.9527378892231,60.1066144882774,59.6521567786534,62.0405881088959,59.3249770071362,58.8553957634551,56.7698941304053,60.7899652356698,44.5093591967443,63.271023819826,60.7542636324377,61.2852185209741,61.4297236734971,59.4631278307791,63.7381748852527,62.2694576087275,61.6935689914578,63.4706298816586,62.9967931364864,60.9772361266365,56.0606857303907,54.5280029584353,61.4963468708336,59.2648331933949,63.2476451596831,62.5607410454114,62.9292336211401,62.4944539517866,66.4547518066372,60.6690193618097,59.0505961227415,59.9689878118587,57.9517941569493,61.7235904376961,62.2441334210526,60.9764161978805,61.3095593010991,60.5434245677346,61.6548735541027,60.0655522003861,62.9441558866783,65.5850439859456,62.4233474769194,61.9199711564198,60.1118416438401,59.4381944969386,58.0433366863055,58.9775833382241,62.88723409102,62.3308668377418,62.5335102947113]}},"geo":"geo","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
# lame, it doesn't render in the notebook.
# render online:
chart_link = api_create(p, filename="choropleth-Percent-Jobs-Automation-US")
```

    ## Found a grid already named: 'choropleth-Percent-Jobs-Automation-US Grid'. Since fileopt='overwrite', I'll try to update it

    ## Found a plot already named: 'choropleth-Percent-Jobs-Automation-US'. Since fileopt='overwrite', I'll try to update it

``` r
chart_link
```

<iframe src="https://plot.ly/~jimmyjamesarnold/7.embed" width="800" height="600" id="igraph" scrolling="no" seamless="seamless" frameBorder="0">
</iframe>
``` r
# see the plot here: https://plot.ly/~jimmyjamesarnold/7
```

Now let's explore the scaled income data:

``` r
# Now let's do some stats on the relative impact:
print(c('Median Annual Income Displaced per Worker:',median(st_income_risk_scaled$total)))
```

    ## [1] "Median Annual Income Displaced per Worker:"
    ## [2] "48.1333671597322"

``` r
print(c('Minimum Annual Income Displaced per Worker:',min(st_income_risk_scaled$total)))
```

    ## [1] "Minimum Annual Income Displaced per Worker:"
    ## [2] "30.1592475590963"

``` r
print(c('Place with lowest income displacement:',rownames(st_income_risk_scaled)[st_income_risk_scaled$total == min(st_income_risk_scaled$total)]))
```

    ## [1] "Place with lowest income displacement:"
    ## [2] "district_of_columbia"

``` r
print(c('Maximum Annual Income Displaced per Worker:',max(st_income_risk_scaled$total)))
```

    ## [1] "Maximum Annual Income Displaced per Worker:"
    ## [2] "55.1636147275903"

``` r
print(c('Place with highest income displacement:',rownames(st_income_risk_scaled)[st_income_risk_scaled$total == max(st_income_risk_scaled$total)]))
```

    ## [1] "Place with highest income displacement:"
    ## [2] "south_dakota"

Hey, those look familiar! SD has the 2nd most job displacement, just after NV. And DC had the lowest job displacement, so it would seem that DC is going to weather automation fairly well.

Let's see the barplot:

``` r
# plot workforce displacement by state code
ggplot(st_income_risk_scaled, aes(x = reorder(st_jobs$code, total), y = total, fill = total)) + 
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(title = "Estimated Annual Income Displaced by Automation", y = "% Annual Income Displaced", x = "Location")
```

![](The_Robots_are_Coming_files/figure-markdown_github/unnamed-chunk-29-1.png)

Interesting, it looks like DC, MA, MD, and CT are going to be the least affected, whereas the ND, SD, and WY are going to be hit hardest.

Let's get the map with plotly.

``` r
# call choropleth, plot scaled % jobs at risk of automation
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))

p <- plot_geo(st_income_risk_scaled, locationmode = 'USA-states') %>% 
  add_trace(
    z = ~total,
    locations = ~st_jobs$code,
    color = ~total,
    colors = viridis_pal(alpha = 1, begin = 0, end = 1, direction = -1,
  option = "B")(20)
    ) %>%
  colorbar(title = "% Income") %>%
  layout(
    title = 'Estimated % Annual Income Displaced by Automation',
    geo = g
  )
p
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-172bf7bf5579c7cf23ad">{"x":{"visdat":{"38882ebb641a":["function () ","plotlyVisDat"]},"cur_data":"38882ebb641a","attrs":{"38882ebb641a":{"locationmode":"USA-states","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"z":{},"locations":{},"color":{},"colors":["#FCFFA4FF","#F1ED6FFF","#F7D340FF","#FBB91FFF","#FCA007FF","#F8870EFF","#F17020FF","#E55C30FF","#D64B40FF","#C43C4EFF","#B1325AFF","#9C2964FF","#87216BFF","#711A6EFF","#5C126EFF","#460B6AFF","#300A5BFF","#190C3EFF","#08051EFF","#000004FF"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"mapType":"geo","scene":{"zaxis":{"title":"total"}},"geo":{"domain":{"x":[0,1],"y":[0,1]},"scope":"usa","projection":{"type":"albers usa"},"showlakes":true,"lakecolor":"rgba(255,255,255,1)"},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5},"title":"Estimated % Annual Income Displaced by Automation"},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"colorbar":{"title":"% Income","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","rgba(252,255,164,1)"],["0.47299540180871","rgba(196,60,78,1)"],["0.54898030673978","rgba(168,46,94,1)"],["0.589765338373194","rgba(152,39,101,1)"],["0.604743990173684","rgba(146,37,103,1)"],["0.614425553678514","rgba(142,36,105,1)"],["0.635251506490477","rgba(133,32,107,1)"],["0.656486158979428","rgba(125,30,108,1)"],["0.667368062058421","rgba(120,28,109,1)"],["0.679737126176742","rgba(115,27,110,1)"],["0.692242389471583","rgba(110,25,110,1)"],["0.705888792220063","rgba(105,23,110,1)"],["0.718839212347019","rgba(99,21,110,1)"],["0.734788585192105","rgba(93,18,110,1)"],["0.752168887377463","rgba(86,16,109,1)"],["0.76707596395654","rgba(80,14,108,1)"],["0.772138308030635","rgba(77,13,107,1)"],["0.783789184638005","rgba(72,12,106,1)"],["0.801897495091828","rgba(65,11,102,1)"],["0.824921414011087","rgba(55,10,96,1)"],["0.830259871975852","rgba(53,10,94,1)"],["0.835068249610484","rgba(51,10,93,1)"],["0.843257446281944","rgba(47,10,90,1)"],["0.867562600104605","rgba(37,12,77,1)"],["1","rgba(0,0,4,1)"]],"showscale":true,"locationmode":"USA-states","z":[51.3509483767023,47.5024225258176,45.7082360734177,49.4534170281446,44.9686734452843,45.1680055927814,42.9630254721086,47.2036409009926,30.1592475590963,50.6673414605942,47.0116542573282,47.7023952879269,48.6376843390782,45.5054538114549,50.9257008675689,49.4913978975728,48.9135963191035,50.934485858676,51.0746389579193,48.1333671597322,41.8973957806417,39.9628571403151,47.8192914464338,45.5367211683061,50.8705597798889,49.8545135649432,51.2547708274431,50.3598939523998,53.1887957908016,46.3783828700285,45.5124615100312,46.7390764643401,44.8850510156129,48.522579024947,50.9067089946042,48.1317668269294,48.3633446958957,46.5036474964713,49.2325459552823,46.6247121069529,50.0604799633361,55.1636147275903,49.3422263758052,49.3385866853877,46.9000071968269,47.2978699811484,43.7129149092905,44.7523417560886,51.1923440437644,49.6880333641483,51.8976607153164],"locations":["AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"],"type":"choropleth","marker":{"line":{"colorbar":{"title":"","ticklen":2},"cmin":30.1592475590963,"cmax":55.1636147275903,"colorscale":[["0","rgba(252,255,164,1)"],["0.47299540180871","rgba(196,60,78,1)"],["0.54898030673978","rgba(168,46,94,1)"],["0.589765338373194","rgba(152,39,101,1)"],["0.604743990173684","rgba(146,37,103,1)"],["0.614425553678514","rgba(142,36,105,1)"],["0.635251506490477","rgba(133,32,107,1)"],["0.656486158979428","rgba(125,30,108,1)"],["0.667368062058421","rgba(120,28,109,1)"],["0.679737126176742","rgba(115,27,110,1)"],["0.692242389471583","rgba(110,25,110,1)"],["0.705888792220063","rgba(105,23,110,1)"],["0.718839212347019","rgba(99,21,110,1)"],["0.734788585192105","rgba(93,18,110,1)"],["0.752168887377463","rgba(86,16,109,1)"],["0.76707596395654","rgba(80,14,108,1)"],["0.772138308030635","rgba(77,13,107,1)"],["0.783789184638005","rgba(72,12,106,1)"],["0.801897495091828","rgba(65,11,102,1)"],["0.824921414011087","rgba(55,10,96,1)"],["0.830259871975852","rgba(53,10,94,1)"],["0.835068249610484","rgba(51,10,93,1)"],["0.843257446281944","rgba(47,10,90,1)"],["0.867562600104605","rgba(37,12,77,1)"],["1","rgba(0,0,4,1)"]],"showscale":false,"color":[51.3509483767023,47.5024225258176,45.7082360734177,49.4534170281446,44.9686734452843,45.1680055927814,42.9630254721086,47.2036409009926,30.1592475590963,50.6673414605942,47.0116542573282,47.7023952879269,48.6376843390782,45.5054538114549,50.9257008675689,49.4913978975728,48.9135963191035,50.934485858676,51.0746389579193,48.1333671597322,41.8973957806417,39.9628571403151,47.8192914464338,45.5367211683061,50.8705597798889,49.8545135649432,51.2547708274431,50.3598939523998,53.1887957908016,46.3783828700285,45.5124615100312,46.7390764643401,44.8850510156129,48.522579024947,50.9067089946042,48.1317668269294,48.3633446958957,46.5036474964713,49.2325459552823,46.6247121069529,50.0604799633361,55.1636147275903,49.3422263758052,49.3385866853877,46.9000071968269,47.2978699811484,43.7129149092905,44.7523417560886,51.1923440437644,49.6880333641483,51.8976607153164]}},"geo":"geo","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
# lame, it doesn't render in the notebook.
# render online:
chart_link = api_create(p, filename="choropleth-Income-Displaced-per-Worker-Automation-US")
```

    ## Found a grid already named: 'choropleth-Income-Displaced-per-Worker-Automation-US Grid'. Since fileopt='overwrite', I'll try to update it

    ## Found a plot already named: 'choropleth-Income-Displaced-per-Worker-Automation-US'. Since fileopt='overwrite', I'll try to update it

``` r
chart_link
```

<iframe src="https://plot.ly/~jimmyjamesarnold/9.embed" width="800" height="600" id="igraph" scrolling="no" seamless="seamless" frameBorder="0">
</iframe>
``` r
# see the plot here: https://plot.ly/~jimmyjamesarnold/9
```

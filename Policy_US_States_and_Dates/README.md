# COVID-19 Lockdown Policies Per State

This folder includes: 

1. The raw data named `states_and_dates.csv`
2. The (semi) cleaned data named `states_and_dates_clean.csv`
3. The **R Markdown** file with the code to plot

## Data Dictionary

| Column | Type | Description |  
|---|---| --- |
| STATE | string | Full name of U.S. State |
| STATE_ABB | string | Two-letter state abbreviation |
| PARTY | string | `Democrat` or `Republican` |
| HOME | date | Start date where full lockdown was enforced |
| PARTIAL | date | Start date where restrictions were eased |
| REOPEN | date | Start date where states can reopen, i.e. close to normal operations |
| COLOR | string | `Red` or `Blue`. Same as `PARTY` |
| EXTENDED | string | `Yes` or `No`. Did states extend lockdown or not |
| SHELTER-IN-PLACE | string | `Yes` or `Never Sheltered`. Did states enforce a lockdown policy |

## Sample Plot -- Subject to Change

![](https://github.com/neooooo28/STATS141_Grp4/blob/master/Policy_US_States_and_Dates/plot_policy_per_state.png)

## Sources

- https://www.nytimes.com/elections/2016/results/president
- https://www.businessinsider.com/us-map-stay-at-home-orders-lockdowns-2020-3
- https://www.nbcnews.com/news/us-news/reopening-america-see-what-states-across-u-s-are-starting-n1195676
- https://abcnews.go.com/US/list-states-stay-home-order-lifts/story?id=70317035

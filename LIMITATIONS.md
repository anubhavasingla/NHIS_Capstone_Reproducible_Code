# A note on the synthetic data exercise in the paper titled "Improving India’s National Household Income Survey"&#32;

Oct 7, 2026 · @Anu

## What I set out to do

I wanted to test whether the NHIS 2026 Block 7c module, which annualises last month's profit by multiplying it by months operated, would misstate yearly income for India's non-agricultural self-employed. Since NHIS is the first survey of its kind, there was no real data to check it against, so I built a synthetic population of 75,000 households and applied the Block 7c rule to it.

The broad structure of that population comes from published PLFS 2024-25 tables. Many of the finer assumptions, especially how income moves through the year, are drawn from my reading, from my own experience of living in India as well as sources I do mention in the paper itself. They have not yet been worked through using data yet. I completed this within the time set for the capstone, and I see it as a work in progress rather than a finished estimate.

## What the exercise does

- Builds about 100,000 businesses across 20 states, matched to PLFS on the share of households in non-agricultural self-employment (13% rural, 18% urban), state industry mix and state mean earnings.
- Gives each business a 12-month ledger of revenue, expenses and closures, across 11 industries and five household types, from year-round to seasonal and sequential.
- Applies the Block 7c rule exactly as written, with the NHIS four-panel rotation, and compares the result to the known truth.
- Runs two passes on the same data: one with perfect reporting, and one where revenue is under-reported and expenses over-reported by 5%.
- Shows that the rule records zero income for seasonal businesses surveyed in an off-month. This follows from the formula itself, not from my assumptions.
- Illustrates how small reporting errors in revenue or expenses become large errors in profit.
- Is fully reproducible from one input file and one random seed, with every assumption listed in Appendix 2 of the paper.

## Limitations

- **Seasonality is assumed.** The monthly revenue patterns for each industry reflect what I understand of festivals, the monsoon and the school calendar, not estimates from data. The roughly 10% design-only bias depends heavily on these, especially on the June trough being deeper than the December peak.
- **Seasonality does not vary by state.** I applied the same monthly patterns everywhere, so state differences in the results come only from industry mix. They should not be read as differences in seasonal exposure.
- **Industry categories are broad.** I used 11 categories, each with a single seasonal pattern, so very different businesses within one category are treated alike.
- **Profit margins are assumed.** I set margins between 8% and 80% by industry without fitting them to data. This matters most for the reporting-bias result.
- **The 40% figure is arithmetic, not a simulation finding.** The second pass reuses the first pass's data and only scales revenue and expenses. Because profit is a small gap between two large numbers, the effect depends almost entirely on margins. In the paper I wrongly credited it to the months-of-operation multiplier, which scales truth and estimate equally.
- **Reporting bias is uniform.** Everyone misreports by exactly 5%, and months of operation are always reported correctly.
- **Industry mix comes from all workers.** PLFS Table 27 covers all workers, not only the self-employed.
- **Some inputs reappear as conclusions.** I set the share of seasonal and sequential businesses at 1.5% from NSS data, and the same figure supports my recommendation to keep last-month recall. The simulation does not test it.
- **I tested random seeds, not assumptions.** Results barely change across four seeds. I have not tested how they change if seasonality, margins or bias rates change, which is the more important check.
- **Other choices are stylised too.** Expense timing, closure rates, links between a household's businesses and the shape of the top tail were all assumed.
- **The model covers one year.** It has no shocks, no year-to-year variation, and no businesses opening or closing.

## Scope

I would read the exact percentages as illustrative and the mechanisms as the main contribution. The pipeline was built so any assumption can be replaced and the whole analysis rerun. Next, I would like to:

- Estimate monthly patterns by industry, and by state where possible, from PLFS unit-level microdata.
- Take margins from ASUSE 2022-23 enterprise data on output and value added by industry.
- Report results across a range of margins and reporting errors instead of a single figure.
- Let reporting error vary by income and industry.
- Split broad categories where seasonality clearly differs, and re-derive the industry mix for the self-employed only.

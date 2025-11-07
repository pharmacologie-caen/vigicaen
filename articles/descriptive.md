# Descriptive

## Introduction

### Objectives

- Compute descriptive statistics on a (vigibase) dataset

- Understand the structure of the `link` table

### Prerequisite

- Be familiar with data management on drugs and adrs, with the `get_*`
  and `add_*` functions (see
  [`vignette("basic_workflow")`](https://pharmacologie-caen.github.io/vigicaen/articles/basic_workflow.md))

## Demo data: counts of drugs, adrs, case characteristics

### Step 0: Load packages

``` r
library(vigicaen)
library(rlang)
library(dplyr)
```

### Step 1: Load datasets and add drug and adr columns

This vignette uses the preloaded datasets (and a spurious suspdup
table).

``` r
demo     <- demo_
adr      <- adr_
drug     <- drug_
link     <- link_
out      <- out_
followup <- followup_

srce     <- srce_

thg      <- thg_
mp       <- mp_
meddra   <- meddra_
smq_list <- smq_list_
smq_content <- smq_content_

suspdup <- 
  data.table::data.table(
    UMCReportId = 1,
    SuspectedduplicateReportId = NA
  )
```

And preloaded drug and adr dictionaries.

``` r
d_drecno <- ex_$d_drecno

a_llt <- ex_$a_llt
```

``` r
demo <-
  demo |>
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `demo` table.

demo <-
  demo |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )
#> ℹ `.data` detected as `demo` table.
```

As we aim to describe drug and adr counts, but also other variables
(age, sex, type of reporter), they will be added too.

You can still refer to

``` r
# Age, sex

demo <-
  demo |>
  mutate(
    age = cut(as.integer(AgeGroup),
              breaks = c(0,4,5,6,7,8),
              include.lowest = TRUE, right = TRUE,
              labels = c("<18", "18-45","45-64", "65-74", "75+")),

    sex = case_when(Gender == "1" ~ 1,
                    Gender == "2" ~ 2,
                    Gender %in% c("-","0","9") ~ NA_real_,
                    TRUE ~ NA_real_)
  )

# Death + outcome availability

demo <- 
  demo |> 
  mutate(death = 
           ifelse(UMCReportId %in% out$UMCReportId,
                  UMCReportId %in% 
                    (out |> 
                    filter(Seriousness == "1") |> 
                    pull(UMCReportId)
                    ),
                  NA)
         )

# follow-up, seriousness

demo <-
  demo |>
  mutate(
    fup = ifelse(UMCReportId %in% followup$UMCReportId, 1, 0),
    serious = 
      ifelse(
        UMCReportId %in% out$UMCReportId,
        UMCReportId %in% 
          (out |> 
          filter(Serious == "Y") |> 
          pull(UMCReportId)
          ),
        NA)
  )

# year

demo <- 
  demo |> 
  mutate(
    year = as.numeric(substr(FirstDateDatabase, start = 1, stop = 4))
    )

# type of reporter

demo <-
  demo |>
  left_join(
    srce |> transmute(UMCReportId, type_reporter = Type),
    by = "UMCReportId")
```

### `desc_facvar()`

[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md)
generates a summary of categorical variables with 2 or more levels.

Its `.data` argument is a dataset to describe. Described variables
should be passed to `vf`, as a character vector.

#### Multi-level variables

Let’s take the `demo` dataset as an example, with variable “age”.

``` r
desc_facvar(
  .data = demo,
  vf = "age"
)
#> # A tibble: 5 × 4
#>   var   level value           n_avail
#>   <chr> <chr> <chr>             <int>
#> 1 age   <18   " 1/499 (0%) "      499
#> 2 age   18-45 "43/499 (9%) "      499
#> 3 age   45-64 "173/499 (35%)"     499
#> 4 age   65-74 "174/499 (35%)"     499
#> 5 age   75+   "108/499 (22%)"     499
```

The output format is a `data.frame`, of class `tibble`.

The first column, `var`, contains the name of the variable of interest.
The second column, `level`, contains the level of the variable.

In this example, the first line shows the number of patients whose age
variable (`var`) is “\<18”, i.e. patients under 18 years old.

The percentage appears in the `value` column, after the count of cases
and the total number of reports for which the information is available.

This number of reports with available information is recalled in the
`n_avail` column.

#### Binary variables

What happens when the variable has only two levels, for example 1 and 0,
as is often the case for the drug and adr variables?

``` r
desc_facvar(
  .data = demo,
  vf = "nivolumab"
)
#> # A tibble: 2 × 4
#>   var       level value         n_avail
#>   <chr>     <chr> <chr>           <int>
#> 1 nivolumab 0     525/750 (70%)     750
#> 2 nivolumab 1     225/750 (30%)     750
```

The output format is unchanged, with a data.frame as output.

The reading is unchanged: we get the count of cases of the variable
nivolumab, by its two levels. There are thus 225 patients exposed to
nivolumab, out of 750 reports in total, which represents 30% of
patients.

Conversely, 525 reports do not mention nivolumab.

In general, when presenting the results, the level 0 of binary variables
provides little information and can be omitted.

#### Logical variables

Let’s continue with another example on the “seriousness” status.

``` r
desc_facvar(
  .data = demo,
  vf = "serious"
)
#> # A tibble: 2 × 4
#>   var     level value         n_avail
#>   <chr>   <chr> <chr>           <int>
#> 1 serious FALSE 181/747 (24%)     747
#> 2 serious TRUE  566/747 (76%)     747
```

The “serious” variable takes the values TRUE/FALSE, and not 1/0, but it
is interpreted in the same way (it is only an artifact of construction).

Thus, 566 cases are considered serious, out of 747 where the information
is available.

#### Exporting raw values

You can export to run plotting or other formatting functions, with
argument `export_raw_values`.

``` r
desc_facvar(
  .data = demo,
  vf = "nivolumab",
  export_raw_values = TRUE
)
#> # A tibble: 2 × 6
#>   var       level value         n_avail     n    pc
#>   <chr>     <chr> <chr>           <int> <int> <dbl>
#> 1 nivolumab 0     525/750 (70%)     750   525    70
#> 2 nivolumab 1     225/750 (30%)     750   225    30
```

### Grouping several levels of a variable

What if the available categories do not match our final needs?

In the example on age, there is only one patient under 18 years old, and
few patients under 45 years old. We would like to group all this data
into a single line for a summary.

The solution is to create the variable with the desired levels upstream,
in a data management step.

``` r
demo <-
  demo |>
  mutate(
    age2 = cut(as.integer(AgeGroup),
              breaks = c(0, 6, 7, 8),
              include.lowest = TRUE, right = TRUE,
              labels = c("<64", "65-74", "75+"))
  )


desc_facvar(
  demo,
  vf = "age2"
)
#> # A tibble: 3 × 4
#>   var   level value         n_avail
#>   <chr> <chr> <chr>           <int>
#> 1 age2  <64   217/499 (43%)     499
#> 2 age2  65-74 174/499 (35%)     499
#> 3 age2  75+   108/499 (22%)     499
```

The same is true for columns like “year”.

When studying the “year” column, it is common to get an error message

``` r
desc_facvar(
  .data = demo,
  vf = "year"
)
#> # A tibble: 13 × 4
#>    var   level value           n_avail
#>    <chr> <chr> <chr>             <int>
#>  1 year  2011  " 1/750 (0%) "      750
#>  2 year  2012  " 1/750 (0%) "      750
#>  3 year  2013  " 2/750 (0%) "      750
#>  4 year  2014  "10/750 (1%) "      750
#>  5 year  2015  " 8/750 (1%) "      750
#>  6 year  2016  "15/750 (2%) "      750
#>  7 year  2017  "116/750 (15%)"     750
#>  8 year  2018  "150/750 (20%)"     750
#>  9 year  2019  "116/750 (15%)"     750
#> 10 year  2020  "72/750 (10%)"      750
#> 11 year  2021  "99/750 (13%)"      750
#> 12 year  2022  "119/750 (16%)"     750
#> 13 year  2023  "41/750 (5%) "      750
```

The error message “Too many levels detected in year” is intentional, to
avoid passing continuous variables in the `vf` argument.

The maximum number of categories that can be taken by a variable treated
by `desc_facvar` is controlled by the `ncat_max` argument.

If a variable has more than `ncat_max` different levels, the function
stops.

We can therefore solve this problem by adjusting the value of this
parameter.

``` r
desc_facvar(
  .data = demo,
  vf = "year",
  ncat_max = 20
)
#> # A tibble: 13 × 4
#>    var   level value           n_avail
#>    <chr> <chr> <chr>             <int>
#>  1 year  2011  " 1/750 (0%) "      750
#>  2 year  2012  " 1/750 (0%) "      750
#>  3 year  2013  " 2/750 (0%) "      750
#>  4 year  2014  "10/750 (1%) "      750
#>  5 year  2015  " 8/750 (1%) "      750
#>  6 year  2016  "15/750 (2%) "      750
#>  7 year  2017  "116/750 (15%)"     750
#>  8 year  2018  "150/750 (20%)"     750
#>  9 year  2019  "116/750 (15%)"     750
#> 10 year  2020  "72/750 (10%)"      750
#> 11 year  2021  "99/750 (13%)"      750
#> 12 year  2022  "119/750 (16%)"     750
#> 13 year  2023  "41/750 (5%) "      750
```

This allows to review the main years, but will be less transposable in a
final table of a manuscript. A categorization of the reporting years may
be more informative.

### Explicit categorical variables

Levels of some variables are indicated by numbers.

``` r
desc_facvar(
  .data = demo,
  vf = "Region"
)
#> # A tibble: 6 × 4
#>   var    level value           n_avail
#>   <chr>  <chr> <chr>             <int>
#> 1 Region 1     " 1/750 (0%) "      750
#> 2 Region 2     "389/750 (52%)"     750
#> 3 Region 3     "17/750 (2%) "      750
#> 4 Region 4     "276/750 (37%)"     750
#> 5 Region 5     " 6/750 (1%) "      750
#> 6 Region 6     "61/750 (8%) "      750
```

We know that 389 cases come from Region “2”, without being able to say
which geographical area this region belongs to.

To obtain the correspondence, there are external tables, such as this
one for the Region: (they can be found in the subsidiary tables of
vigibase).

| Code | Label                        |
|------|------------------------------|
| 1    | African Region               |
| 2    | Region of the Americas       |
| 3    | South-East Asia Region       |
| 4    | European Region              |
| 5    | Eastern Mediterranean Region |
| 6    | Western Pacific Region       |

Several options are possible to bring the information back directly into
demo, the simplest is to use factors

``` r
demo <-
  demo |> 
  mutate(
    Region = factor(Region, levels = c("1", "2", "3", "4", "5", "6"))
  )

levels(demo$Region) <-
  c("African Region",                                    
    "Region of the Americas",                            
    "South-East Asia Region",                            
    "European Region",                                   
    "Eastern Mediterranean Region",                      
    "Western Pacific Region"  
  )
```

Note the transformation in two steps. The first to sort the levels of
the variable, the second to assign the labels to its levels. This
sequence is necessary to avoid a random sorting of levels.

This transformation has the effect of modifying the result of
[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md)

``` r
desc_facvar(
  .data = demo,
  vf = "Region"
)
#> # A tibble: 6 × 4
#>   var    level                        value           n_avail
#>   <chr>  <chr>                        <chr>             <int>
#> 1 Region African Region               " 1/750 (0%) "      750
#> 2 Region Region of the Americas       "389/750 (52%)"     750
#> 3 Region South-East Asia Region       "17/750 (2%) "      750
#> 4 Region European Region              "276/750 (37%)"     750
#> 5 Region Eastern Mediterranean Region " 6/750 (1%) "      750
#> 6 Region Western Pacific Region       "61/750 (8%) "      750
```

The two other variables mainly affected by this phenomenon are `Type`
and `type_reporter`. The transformation code is found in
`vignette("template_main.R")`

### Other arguments of `desc_facvar()`

Three other arguments allow to control the output format of the results.

1.  `format` is a character string that must necessarily contain the
    values `n`, `N` and `pc`.

This argument allows to customize the way the result is displayed. For
example, if you want to put the percentage in brackets instead of
parentheses

``` r
desc_facvar(
  .data = demo,
  vf = "nivolumab",
  format = "n_/N_ [pc_%]"
)
#> # A tibble: 2 × 4
#>   var       level value         n_avail
#>   <chr>     <chr> <chr>           <int>
#> 1 nivolumab 0     525/750 [70%]     750
#> 2 nivolumab 1     225/750 [30%]     750
```

You can also change all other elements of this argument.

2.  `pad_width` allows to center the results in the middle of a
    character string. If you have particularly high numbers, you can
    increase the value of this parameter, so that your results remain
    well centered.

3.  `digits` controls the number of digits after the decimal point for
    the percentage. **Warning**, it is not guaranteed that the sum will
    be exactly 100%.

``` r
desc_facvar(
  .data = demo,
  vf = "nivolumab",
  digits = 1
)
#> # A tibble: 2 × 4
#>   var       level value           n_avail
#>   <chr>     <chr> <chr>             <int>
#> 1 nivolumab 0     525/750 (70.0%)     750
#> 2 nivolumab 1     225/750 (30.0%)     750
```

## Drug data: drug screening

## Adr data: adr screening and evolution of adverse events

[`screen_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/screen_drug.md)
let you screen the most drugs reported in a `drug` dataset, sorted by
frequency.

``` r
screen_drug(drug, mp_data = mp, top_n = 5)
#> # A tibble: 5 × 4
#>   `Drug name`      DrecNo     N percentage
#>   <chr>             <dbl> <int>      <dbl>
#> 1 pembrolizumab  20116296   298      39.7 
#> 2 nivolumab     111841511   225      30   
#> 3 ipilimumab    133138448    86      11.5 
#> 4 atezolizumab  112765189    69       9.2 
#> 5 durvalumab    125456180    68       9.07
```

Most of the time, you will have filtered the `drug` data upstream, with
some `add_*` function, allowing to focus on a subset of cases (of a
specific drug, adr, or any set of these)

For example, identify colitis cases and screen drugs under this
reaction.

``` r
drug |> 
  add_adr(
    a_llt,
    adr_data = adr
  ) |> 
  filter(a_colitis == 1) |> 
  screen_drug(
    mp_data = mp, top_n = 5
  )
#> ℹ `.data` detected as `drug` table.
#> # A tibble: 5 × 4
#>   `Drug name`      DrecNo     N percentage
#>   <chr>             <dbl> <int>      <dbl>
#> 1 nivolumab     111841511    44       42.3
#> 2 pembrolizumab  20116296    40       38.5
#> 3 ipilimumab    133138448    20       19.2
#> 4 NA             73636724    14       13.5
#> 5 NA             34178924    13       12.5
```

### Adr screening

[`screen_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/screen_adr.md)
let you screen the most frequent reactions reported in an `adr` dataset,
sorted by frequency.

``` r
screen_adr(adr_, meddra = meddra_)
#>                                               term     n percentage
#>                                             <char> <int>      <num>
#> 1:                                            <NA>   678 90.4000000
#> 2: Respiratory, thoracic and mediastinal disorders   110 14.6666667
#> 3:                      Gastrointestinal disorders   104 13.8666667
#> 4:                              Vascular disorders     9  1.2000000
#> 5:                         Immune system disorders     6  0.8000000
#> 6:                         Hepatobiliary disorders     5  0.6666667
#> 7:          Skin and subcutaneous tissue disorders     1  0.1333333
```

Different term levels can be used, according to meddra, with argument
`term_level`.

Most of the time, you will have filtered the `adr` data upstream, with
some `add_*` function, allowing to focus on a subset of cases (of a
specific drug, adr, or any set of these).

### Outcome

The adr table contains information on the evolution of adverse events.

The possible outcomes (column `Outcome`) are

- Recovered/resolved
- Recovering/resolving
- Recovered/resolved with sequelae
- Not recovered/not resolved
- Fatal
- Died- unrelated to reaction
- Died- reaction may be contributory

The adr structure is as follows

| UMCReportId | Adr_Id | Outcome |
|-------------|--------|---------|
| 1           | a_1    | 1       |
| 1           | a_2    | 2       |
| 2           | a_3    | 3       |
| 2           | a_4    | 1       |

A case, identified by its UMCReportId, may have several adverse events
(Adr_Id) with different outcomes. Summarizing this information requires
prioritization.

The logic is as follows: take the ” worst evolution” possible for each
event of each case, in order to count each event only once for each
case.

In order to filter cases according to a drug exposition, it is necessary
to join the drug data to the adr table.

### Step 1: Data management of adr with add_drug and add_adr

[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
and
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)
can be used on `adr` data.

``` r

adr <-
  adr |>
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `adr` table.

adr <-
  adr |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )
#> ℹ `.data` detected as `adr` table.
```

This allows to identify drugs and adverse events of interest in the adr
table.

Drugs are identified at the case level in this table.

### Step 2: `desc_outcome()` function

The `desc_outcome` function prioritizes data according to the rule:

> Take the “worst evolution” possible for each event of each case, in
> order to count each event only once for each case.

``` r
adr |> 
  desc_outcome(
    drug_s = "nivolumab",
    adr_s = "a_colitis"
  )
#> # A tibble: 5 × 4
#>   drug_s    adr_s     n_cas out_label                 
#>   <chr>     <chr>     <int> <chr>                     
#> 1 nivolumab a_colitis    10 Unknown                   
#> 2 nivolumab a_colitis    25 Recovered/resolved        
#> 3 nivolumab a_colitis     6 Recovering/resolving      
#> 4 nivolumab a_colitis     1 Not recovered/not resolved
#> 5 nivolumab a_colitis     2 Fatal
```

In the case where `adr` was previously filtered to contain only data of
a specific adverse drug reaction (for example, with
[`tb_subset()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_subset.md)),
it is still preferable to recreate the drug column with `add_drug` (it
will take the value 1 for all cases).

## Link data: time to onset, dechallenge, rechallenge

The link table, as created with
[`tb_vigibase()`](https://pharmacologie-caen.github.io/vigicaen/reference/tb_vigibase.md),
contains additional information than the original link table.

It is augmented with

- `UMCReportId` the case id
- `tto_mean` the average of TimeToOnsetMin, and TimeToOnsetMax, in days
- `range` the half-difference between TimeToOnsetMin and TimeToOnsetMax,
  in days

These additional variables are useful to compute the time from drug
initiation to adverse drug reaction onset, and also to compute
dechallenge and rechallenge data at case level.

### Step 1: Load the datasets

``` r
link <- 
  link_
```

### Step 2: Add drug and adr columns

The link table studies the relationship of each drug - adverse event
pair, within the reports. There are therefore several lines in link for
each line (case) in demo.

`demo` table example

| UMCReportId | Other data (age, sexe…) |
|-------------|-------------------------|
| 1           | 65-74, Man              |
| 2           | 65-74, Woman            |
| 3           | 45-64, Woman            |

The corresponding `link` table would be

| UMCReportId | Drug_Id | Adr_Id | Time to onset |
|-------------|---------|--------|---------------|
| 1           | 1_1     | 1_a    | 60            |
| 1           | 1_2     | 1_a    | 30            |
| 1           | 1_1     | 1_b    | 45            |
| 1           | 1_2     | 1_b    | 15            |
| 2           |         |        |               |
| 2           |         |        |               |
| 3           |         |        |               |
| 3           |         |        |               |
| 3           |         |        |               |

### Learn the content of link

Let’s take a while to read data related to the case no 1, in the
previous example.

- It contains two different Drug_Id `1_1` and `1_2`: this means that
  this case has two different drugs. Most of the time, it is two
  different drugs (let’s say, paracetamol and ibuprofen for this
  example). It can also be the same drug, with different administration
  modalities (paracetamol with two dosages, or at two different times).

- It contains two different Adr_Id `1_a` and `1_b`: this means that this
  case has two different adverse events. Mostly, it refers to two
  different events (e.g. hepatitis and hemorrhage).

- Information are available for each combination. The time to onset,
  i.e. the delay between drug initiation and event onset is displayed
  for each combination

The reading is as follows:

- The hepatitis (`1_a`) occurred 60 days after the introduction of
  paracetamol (`1_1`), and 30 days after the introduction of ibuprofen
  (`1_2`).

- The hemorrhage (`1_b`) occurred 45 days after the introduction of
  paracetamol (`1_1`), and 15 days after the introduction of ibuprofen
  (`1_2`).

In this relatively simple example, everything is coherent: we observe
that paracetamol and ibuprofen were introduced 30 days apart from each
other.

The reality is often more complex: as previously announced, there may be
several lines in `link`for the same drug, with different time to onset.

In this case, it is important to decide how to handle this multiple
information.

For example, we could have a time to onset at 30 days for paracetamol
taken at 500mg daily, and a time to onset at 15 days for paracetamol
taken at 1000mg daily.

### Identify drugs and adverse events

As for the `demo` and `adr` tables, the `link` table must be completed
with drug and adr columns, using the `add_*` family functions.

``` r
link <-
  link |> 
   add_drug(
    d_code = d_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `link` table.

link <-
  link |>
  add_adr(
    a_code = a_llt,
    adr_data = adr
  )
#> ℹ `.data` detected as `link` table.
```

Counts check

``` r
link |> 
   check_dm(
     cols = c(names(d_drecno), names(a_llt))
     )
#>               [,1]
#> ipilimumab     267
#> atezolizumab   189
#> durvalumab     166
#> nivolumab     1347
#> pembrolizumab 1819
#> avelumab        83
#> cemiplimab      38
#> tremelimumab    35
#> a_embolism      38
#> a_colitis      314
#> a_pneumonitis  238
```

**!! Warning!!**, counts correspond to the number of lines for each drug
and each effect. It is not the number of reports containing each drug or
each effect. If you want to obtain this information, you must query the
`demo` table.

### Time to onset

The time to onset information is contained in two variables in the
`link` table: `TimeToOnsetMin` and `TimeToOnsetMax`. These two variables
reflect the minimum and maximum delay of the adverse event occurrence
compared to the drug intake, taking into account the uncertainty of the
input data.

| UMCReportId | Drug_Id | Adr_Id | TimeToOnsetMin | TimeToOnsetMax |
|-------------|---------|--------|----------------|----------------|
| 1           | 1_1     | 1_a    | 45             | 75             |

Here, hepatitis occurred between 45 and 75 days after first paracetamol
intake.

This structure is inherited of the incertitude from the source reporter
or the case. This case would correspond to data like: “Hepatitis
occurred 2months after paracetamol introduction”.

This sentence contains an imprecision on the exact delay of occurrence:
what was the exact day of the month? Was it 1 month and 15 days? Or 2
months and 15 days? More? It is impossible to decide.

By convention, we consider that the true time to onset is +/- 15 days
from the indicated date (here, between 60 - 15 = 45 days, and 60 + 15 =
75 days).

Two parameters are derived from this information: the mean time to onset
`tto_mean` and the `range`. The calculation is as follows:

``` r
tto_mean = (TimeToOnsetMax + TimeToOnsetMin) / 2

range = (TimeToOnsetMax + TimeToOnsetMin) / 2 - TimeToOnsetMin
```

| UMCReportId | Drug_Id | Adr_Id | TimeToOnsetMin | TimeToOnsetMax | tto_mean | range |
|-------------|---------|--------|----------------|----------------|----------|-------|
| 1           | 1_1     | 1_a    | 45             | 75             | 60       | 30    |

The `tto_mean` is intuitive: it is the average delay between the two
available values. In our example, we find 60 days, which is the delay
indicated by the reporter.

The `range` gives the uncertainty: 30 days in our example, meaning that
we cannot be more precise than 30 days.

> The Uppsala Monitoring Centre recommendation is to use only the time
> to onset whose range is \<= 1, i.e. the cases where the date is known
> to the day.

Note: the information on hours and minutes is also present in the time
to onset, if known.

If we keep on the example of hepatitis, we could have a time to onset at
30 days for paracetamol taken at 500mg daily, and a time to onset at 15
days for paracetamol taken at 1000mg daily.

In this case, it is important to decide how to handle this multiple
information. Otherwise, we would have two different `tto_mean` values
for the paracetamol - hepatitis pair.

There is a need for an arbitrary rule to synthetize these data. Our
habit is to **take the longest delay** between the drug introduction and
the event occurrence (i.e. the delay between the first drug intake and
the event). Admittedly, this may not meet all needs.

This information, that we call `tto_max`, is obtained with
[`extract_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/extract_tto.md).

``` r
extract_tto(
  .data = link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)
#>    UMCReportId tto_max     adr_s    drug_s
#> 1    108846594     205 a_colitis nivolumab
#> 2     73027866     175 a_colitis nivolumab
#> 3     87966085      36 a_colitis nivolumab
#> 4     88371745     740 a_colitis nivolumab
#> 5    138643678     379 a_colitis nivolumab
#> 6     39936658      99 a_colitis nivolumab
#> 7    140765885       5 a_colitis nivolumab
#> 8     11372968      62 a_colitis nivolumab
#> 9     63102481      57 a_colitis nivolumab
#> 10    34209616       8 a_colitis nivolumab
#> 11    21293261      59 a_colitis nivolumab
#> 12    58329610     541 a_colitis nivolumab
#> 13    38269558     393 a_colitis nivolumab
#> 14    85691581     490 a_colitis nivolumab
#> 15   112663221       7 a_colitis nivolumab
#> 16   109716692      37 a_colitis nivolumab
#> 17    56986472      47 a_colitis nivolumab
#> 18    76401465     390 a_colitis nivolumab
#> 19   125056616      18 a_colitis nivolumab
#> 20    33942691      36 a_colitis nivolumab
#> 21   133088642      23 a_colitis nivolumab
#> 22   143881598     113 a_colitis nivolumab
#> 23    79620779      85 a_colitis nivolumab
#> 24    95759941      59 a_colitis nivolumab
```

The `tto_max` is the longest delay between the drug introduction and the
event occurrence. There is only one line for each drug - adr pair.

This information can be used for a graphical representation, or to
derive an average, a range… The second option is possible in many ways,
notably with
[`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md).

``` r
desc_tto(
  .data = link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)
#>      drug_s     adr_s     var level                         value n_avail
#> 1 nivolumab a_colitis tto_max  <NA> 60.5 (36.0-248.5) [5.0-740.0]      24
```

Several drugs and reactions can be queried in these two functions.

``` r
desc_tto(
  .data = link,
  drug_s = c("nivolumab", "pembrolizumab"),
  adr_s  = c("a_colitis", "a_pneumonitis")
)
#>          drug_s         adr_s     var level                           value
#> 1     nivolumab     a_colitis tto_max  <NA>   60.5 (36.0-248.5) [5.0-740.0]
#> 2 pembrolizumab     a_colitis tto_max  <NA> 44.0 (18.5-112.5) [1.0-1,207.0]
#> 3     nivolumab a_pneumonitis tto_max  <NA>  75.0 (49.8-167.0) [18.0-602.0]
#> 4 pembrolizumab a_pneumonitis tto_max  <NA> 52.0 (21.0-101.0) [0.0-1,050.0]
#>   n_avail
#> 1      24
#> 2      15
#> 3      22
#> 4      17
```

### Dechallenge

[`desc_dch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_dch.md)
synthesizes the number of positive dechallenges:

> A positive dechallenge occurs when the drug has been stopped or its
> dosage has been reduced, and the reaction has abatted.

``` r
desc_dch(
  link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)
#>      drug_s     adr_s pos_dch
#> 1 nivolumab a_colitis      27
```

``` r
desc_dch(
  link,
  drug_s = c("nivolumab", "pembrolizumab"),
  adr_s  = c("a_colitis", "a_pneumonitis")
)
#>          drug_s         adr_s pos_dch
#> 1     nivolumab     a_colitis      27
#> 2 pembrolizumab     a_colitis      19
#> 3     nivolumab a_pneumonitis      22
#> 4 pembrolizumab a_pneumonitis      26
```

### Rechallenge

Description span from rechallenge cases to informative rechallenge cases
(those cases where the outcome is known). Drug and Adr identifiers refer
to DrecNo and MedDRA_Id, respectively. Terminology

- `overall` as opposed to `rch` for rechallenged (`rch` + `no_rch` =
  `overall`).

- Among `rch`, `inf` (informative) as opposed to `non_inf` (`inf` +
  `non_inf` = `rch`)

- Among `inf`, `rec` (recurring) as opposed to `non_rec` (`rec` +
  `non_rec` = `inf`)

``` r
desc_rch(
  link,
  drug_s = "nivolumab",
  adr_s  = "a_colitis"
)
#>       drug_s     adr_s n_overall n_rch n_inf n_rec
#>       <char>    <char>     <int> <int> <int> <int>
#> 1: nivolumab a_colitis        44    26    19    12
```

**The number of cases is counted at the case level.**

As with
[`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md)
and
[`desc_dch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_dch.md),
you can query several drug - adr pairs at once.

> Columns passed to arguments `drug_s` and `adr_s` can correspond to
> sets of drugs or events, or even identify all cases present in your
> dataset.

Let’s say we want to know the number of positive rechallenge cases for
our entire dataset

We must create a variable that takes the value 1 for all cases.

``` r
link <-
  link |> 
  mutate(
    all_cases = 1
  )
```

We a particular syntax, we can access the information

``` r
desc_rch(
  link,
  drug_s = "all_cases",
  adr_s  = "all_cases"
)
#>       drug_s     adr_s n_overall n_rch n_inf n_rec
#>       <char>    <char>     <int> <int> <int> <int>
#> 1: all_cases all_cases       707   297   106    28
```

# Basic Workflow

## Introduction

### Objectives

- Collect drug and adverse drug reaction **IDs**

- Perform standard data management in VigiBase ECL

- Conduct disproportionality analysis (both univariate and multivariate)

If you want to access script templates for these steps, see

- [`vignette("template_dictionary")`](https://pharmacologie-caen.github.io/vigicaen/articles/template_dictionary.md)
  and

- [`vignette("template_main")`](https://pharmacologie-caen.github.io/vigicaen/articles/template_main.md)

### Reminder of Database Structure

Each table has a unique identifying key and other keys to perform joins.

| Table  | Key           | Other keys    |
|--------|---------------|---------------|
| `demo` | `UMCReportId` |               |
| `drug` | `Drug_Id`     | `UMCReportId` |
| `adr`  | `Adr_Id`      | `UMCReportId` |

> Goal of the tutorial: perform a disproportionality analysis between
> colitis and nivolumab among checkpoint inhibitor cases.

### Step 0: Load Packages

``` r
library(vigicaen)
library(rlang)
library(dplyr)
```

## Build tables from source files

This process should be done **once per database version**.

You don’t have to do it to follow this tutorial, since we will use the
package built-in example tables.

However, when working on real analyses, you will need to process this
step first.

See the vignette here
[`vignette("getting_started")`](https://pharmacologie-caen.github.io/vigicaen/articles/getting_started.md).

## Collecting IDs

The whole package relies on defining a dictionary of drugs and adrs of
interest.

Those collection of terms should be stored in **named lists**.

``` r
# drug selection
d_sel <-
  list(
    nivolumab   = "nivolumab",
    ipilimumab  = "ipilimumab",
    nivo_or_ipi = c("nivolumab", "ipilimumab")
  )

# adverse drug reaction selection

a_sel <- 
  list(
    colitis     = "Colitis",
    pneumonitis = "Pneumonitis"
  )
```

As we see, the `d_sel` list contains three named vectors: nivolumab,
ipilimumab, and nivo_or_ipi. Each of these vectors can contain one or
more names of drugs.

> The names of the vectors don’t have to be the same as the drug names.
> They will be used to created columns later in the process.

We will pass these named list selections to *ID collector* functions:
The **`get_*`** family.

- [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
  for drugs,
  [`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)
  for ATC classes

- [`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
  or
  [`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)
  for adverse drug reactions

These functions will allow to collect IDs (e.g. codes) matching our
drugs and adrs in a specific dictionary.

- For drugs, we will use the WHODrug dictionary, and collect Drug Record
  Numbers (DrecNos) most of the time, or Medicinal Product Ids
  (MedicinalProd_Ids) in some specific scenarii.

- For adrs, we will use the Medical Dictionary for Regulatory Activities
  (MedDRA), and collect term codes (low-level term codes). Here, it is
  important to note that we can work with other terms (like Preferred
  Terms, High Level terms, etc.). The ID collector will just collect
  low-level term codes of all higher level terms, resulting in a pretty
  long list of codes, because VigiBase data is structured on low-level
  term codes.

## Data management

### Drugs

#### Principle

1.  Load the `demo`, `drug`, and `mp` tables.
2.  Select one or more medications of interest.
3.  Identify the drug codes (e.g., the `DrecNo(s)`) associated with
    these drug using the `mp` table.
4.  Search for cases exposed to these medications using the codes in the
    `drug` table.
5.  Update the `demo` table: code 1 if the case reports the medication
    of interest, 0 otherwise.
6.  Check your data management

Step 1 is performed with
[`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md)
if you have [your own tables](#building_tables), or using the built-in
example tables for this tutorial.

Step 2 and 3 can be referred as “dictionary” steps.

Step 3 is performed with
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
or
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md).

Steps 4 and 5 are performed with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md).

step 6 is performed with
[`check_dm()`](https://pharmacologie-caen.github.io/vigicaen/reference/check_dm.md).

#### Step 1: Load the tables

``` r
demo <- demo_
drug <- drug_

mp <- mp_
```

> Note: if you are working with your own tables, you will need to load
> them here, with
> [`dt_parquet()`](https://pharmacologie-caen.github.io/vigicaen/reference/dt_parquet.md).

#### Step 2: Choose drugs of Interest

This is probably the most interesting part of the process, from a
scientific point of view. But for now, it’s pretty trivial.

Once you’ve decided which drugs you would like to study (e.g. nivolumab
in this tutorial), you need to create a **named list** of character
vectors.

``` r
d_sel <- # drug selection
  list2(
    nivolumab = "nivolumab"
  )

d_sel
#> $nivolumab
#> [1] "nivolumab"
```

> Remember to use lower case names in d_sel (no capital letters: Good:
> “nivolumab”, Wrong: “Nivolumab” or “NIVOLUMAB”)

The ideal way of picking drugs is by using their **WHO name**.

WHO name is an international non-proprietary name (INN) for the drug. A
few drugs have more than one INN (e.g. paracetamol and acetaminophen),
but they still have a unique WHO name (most of the time, one of the
INN).

The ID collector
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
will let you know if you missed the WHO name.

Alternatively, you can work with Anatomical and Therapeutical
Classification (ATC) codes to investigate a set of drug. This is
explained in the [ATC section](#atc_classes).

#### Step 3: Identify drug codes

The function
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
allows you to query the `mp` table with a selection of drug names.

It takes several arguments, two of which must be filled in: the
selection of drugs and the table containing the correspondence between
the name and the code (here, `mp`).

You should **always** look carefully at the printed message.

1.  Use
    [`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
    with argument `verbose = TRUE` (default).

``` r
get_drecno(
  d_sel = d_sel,
  mp = mp_,
  verbose = TRUE
  )
#> 
#> ── get_drecno() ────────────────────────────────────────────────────────────────
#> 
#> ── `d_sel`: Matching drugs ──
#> 
#> ── ✔ Matched drugs
#> 
#> → `nivolumab`: "nivolumab" and "ipilimumab;nivolumab"
#> 
#> 
#> ℹ Set `verbose` to FALSE to suppress this section.
#> 
#> 
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> $nivolumab
#> [1] 111841511  98742214
```

We see that
[`get_drecno()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_drecno.md)
finds two entries containing the drug “nivolumab” in the `mp` table:

- The entry for nivolumab alone

- The entry for the ipilimumab;nivolumab combination

This second entry was identified because the `allow_combination`
argument is set to `TRUE` by default. This allows for a broader
identification of all specialties containing nivolumab. In this
situation, this behavior is desirable because we want to be sure to
identify all cases reporting the drug.

2.  Since these are actually the codes we were looking for, we can
    (optionally) set the `verbose` argument to FALSE and keep the result
    in an R object called `d_drecno`.

``` r
d_drecno <-
  get_drecno(
    d_sel = d_sel,
    mp = mp_,
    verbose = FALSE
    )
```

#### Steps 4 and 5: add_drug() function

To identify cases reporting the drug of interest and add the
corresponding column to `demo`, we use the
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
function.

The
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
function takes 3 mandatory arguments:

- The dataset on which to add the drug variable(s) (here, `demo`)

- A named list containing the codes of the drug(s)

- The `drug` table linking drug intake to each case.

``` r
demo <- 
  add_drug(
    .data = demo,
    d_code = d_drecno,
    drug_data = drug)
#> ℹ `.data` detected as `demo` table.
demo
#>      UMCReportId AgeGroup Gender DateDatabase   Type Region FirstDateDatabase
#>            <int>   <char> <char>       <char> <char> <char>            <char>
#>   1:   141557914        7      2     20210927      2      4          20200703
#>   2:    48453218        9      -     20170809      1      2          20170809
#>   3:   127169804        6      2     20150814      1      2          20150814
#>   4:    35746144        6      1     20120221      1      2          20111114
#>   5:   143784377        9      1     20190418      1      2          20190418
#>  ---                                                                         
#> 746:    48354189        6      1     20150814      2      2          20150814
#> 747:    20398003        7      1     20220107      1      3          20211001
#> 748:   109565701        8      1     20220107      1      3          20220107
#> 749:    95759941        9      1     20200301      2      4          20180810
#> 750:    76017998        7      1     20210301      2      4          20201216
#>      nivolumab
#>          <num>
#>   1:         0
#>   2:         0
#>   3:         0
#>   4:         0
#>   5:         1
#>  ---          
#> 746:         0
#> 747:         0
#> 748:         0
#> 749:         1
#> 750:         0
```

Or, in tidyverse syntax

``` r
demo <- 
  demo |> 
  add_drug(
    d_code = d_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `demo` table.
```

#### Step 6: Check your data management

This may seem trivial, but it is an **essential** step in the
construction of a dataset.

There are many ways to check that the code has worked. Here, the
[`check_dm()`](https://pharmacologie-caen.github.io/vigicaen/reference/check_dm.md)
function will count the number of rows in the dataset where the desired
column is equal to 1.

``` r
check_dm(demo, "nivolumab")
#>           [,1]
#> nivolumab  225
```

It shows how many rows in `demo` have the value 1 in the `nivolumab`
column (e.g. how many cases where identified as reporting on nivolumab
reactions).

Here, we see that 225 cases report nivolumab.

#### Step 2 and 3 variant: ATC classes

The correspondence between ATC (Anatomical and Therapeutical
Classification) classes and drug codes is found in the `thg` table. In
this table, drug codes are stored as `MedicinalProd_Id`. It is therefore
necessary to make a second correspondence with `mp` to find `DrecNo`.

This can be done with the
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)
function.

As with drugs, we first need to identify the ATC class of interest
(here, “L03”).

``` r
atc_sel <-
  list2(l03 = "L03")

atc_drecno <- 
  get_atc_code(
    atc_sel = atc_sel,
    mp = mp,
    thg_data = thg_
    )
#> ℹ vigilyze set to TRUE, extracting DrecNos (?get_atc_code for details)
```

The
[`get_atc_code()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_atc_code.md)
function requires the `mp` and `thg` tables, as well as the selection of
ATC classes.

``` r
str(atc_drecno)
#> List of 1
#>  $ l03: num [1:13] 2.20e+07 1.41e+08 1.01e+08 1.25e+08 3.93e+07 ...
```

By default, this function retrieves DrecNos associated with an ATC
class. It is possible to retrieve MedicinalProd_Ids instead by setting
the `vigilyze` argument to `FALSE`.

The interest of using MedicinalProd_Ids instead of DrecNos is to
restrict the drug panel only to packages corresponding to a specific ATC
class (e.g., you might not want to find all packages of corticosteroids
if you work with the ATC class “S01BA”, which corresponds to ophtalmic
steroids).

Once DrecNos are identified, we can add them to the `demo` table, with
the
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
function.

``` r
demo |> 
  add_drug(
    d_code = atc_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `demo` table.
#>      UMCReportId AgeGroup Gender DateDatabase   Type Region FirstDateDatabase
#>            <int>   <char> <char>       <char> <char> <char>            <char>
#>   1:   141557914        7      2     20210927      2      4          20200703
#>   2:    48453218        9      -     20170809      1      2          20170809
#>   3:   127169804        6      2     20150814      1      2          20150814
#>   4:    35746144        6      1     20120221      1      2          20111114
#>   5:   143784377        9      1     20190418      1      2          20190418
#>  ---                                                                         
#> 746:    48354189        6      1     20150814      2      2          20150814
#> 747:    20398003        7      1     20220107      1      3          20211001
#> 748:   109565701        8      1     20220107      1      3          20220107
#> 749:    95759941        9      1     20200301      2      4          20180810
#> 750:    76017998        7      1     20210301      2      4          20201216
#>      nivolumab   l03
#>          <num> <num>
#>   1:         0     0
#>   2:         0     0
#>   3:         0     0
#>   4:         0     0
#>   5:         1     0
#>  ---                
#> 746:         0     0
#> 747:         0     0
#> 748:         0     0
#> 749:         1     0
#> 750:         0     0
```

#### Step 4 and 5 variant: Suspect, concomitant, interacting

We can choose to work with drugs according to their “reputation basis”.

This information is stored in the `Basis` column of the `drug` table.

- 1 suspect

- 2 concomitant

- 3 interacting

By using the
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
function, we can specify which type of status we are interested in, in
the `repbasis` argument. By default, the value `"sci"` indicates that we
consider the drug whether it is suspect, concomitant, or interacting. We
can change the selection.

``` r
demo |> 
  add_drug(
    d_code = d_drecno,
    drug_data = drug,
    repbasis = "sci"
  ) |> 
  check_dm("nivolumab")
#> ℹ `.data` detected as `demo` table.
#>           [,1]
#> nivolumab  225

# suspected only

demo |> 
  add_drug(
    d_code = d_drecno,
    drug_data = drug,
    repbasis = "s"
  ) |> 
  check_dm("nivolumab")
#> ℹ `.data` detected as `demo` table.
#>           [,1]
#> nivolumab  214
```

#### Create multiple drug columns

To work with multiple drugs, you need to update the initial `d_sel`
list.

``` r
d_sel <- 
  list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
  )

d_drecno <-
  d_sel |> 
  get_drecno(mp = mp)
#> 
#> ── get_drecno() ────────────────────────────────────────────────────────────────
#> 
#> ── `d_sel`: Matching drugs ──
#> 
#> ── ✔ Matched drugs
#> 
#> → `nivolumab`: "nivolumab" and "ipilimumab;nivolumab"
#> → `pembrolizumab`: "pembrolizumab"
#> 
#> 
#> ℹ Set `verbose` to FALSE to suppress this section.
#> 
#> 
#> 
#> ────────────────────────────────────────────────────────────────────────────────

demo <- 
  demo |> 
  add_drug(
    d_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `demo` table.

demo |> 
  check_dm(c("nivolumab", "pembrolizumab"))
#>               [,1]
#> nivolumab      225
#> pembrolizumab  298
```

#### Drug groups

If you want to work at the level of a group of drugs, but the ATC
classes do not match your needs perfectly, you can group them in the
`d_sel` list.

``` r
d_sel <- 
  list2(
    analgesics = c("paracetamol", "tramadol"),
    ici = c("nivolumab", "pembrolizumab")
  )

d_drecno <-
  d_sel |> 
  get_drecno(mp = mp,
             allow_combination = FALSE)
#> 
#> ── get_drecno() ────────────────────────────────────────────────────────────────
#> 
#> ── `d_sel`: Matching drugs ──
#> 
#> ── ✔ Matched drugs
#> 
#> → `analgesics`: "paracetamol" and "tramadol"
#> → `ici`: "nivolumab" and "pembrolizumab"
#> 
#> 
#> ℹ Set `verbose` to FALSE to suppress this section.
#> 
#> 
#> 
#> ────────────────────────────────────────────────────────────────────────────────

demo <- 
  demo |> 
  add_drug(
    d_drecno,
    drug_data = drug
  )
#> ℹ `.data` detected as `demo` table.

demo |> 
  check_dm(names(d_sel))
#>            [,1]
#> analgesics   68
#> ici         519
```

### Adverse drug reactions

#### Principles

1.  Load the `demo`, `adr`, and `meddra` tables.

2.  Choose the adverse event(s) of interest.

3.  Identify the event codes (these are low-level terms according to the
    MedDRA classification). They can be found in the `meddra` table or
    in the `smq` tables.

4.  Search for cases that have presented this event, using the codes

5.  Update the `demo` table: code 1 if the case reports the event of
    interest, 0 otherwise.

6.  Check your data management

Similarly to the [drug workflow](#drug_workflow), steps 2 and 3 can be
referred to as “dictionary” steps.

Step 3 uses
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
or
[`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md).

#### Step 1: Load the tables

``` r
adr <- adr_
meddra <- meddra_
```

demo was loaded during the [drug workflow](#drug_workflow).

#### Step 2: Choose events of interest

``` r
a_sel_pt <-
  list2(
    a_colitis = c(
      "Colitis",
      "Autoimmune colitis",
      "Colitis microscopic",
      "Diarrhoea",
      "Diarrhoea haemorrhagic",
      "Duodenitis",
      "Enteritis",
      "Enterocolitis",
      "Enterocolitis haemorrhagic",
      "Ulcerative gastritis"
    )
  )
```

We start with a list of adverse events of interest, grouped altogether
under the name “a_colitis”.

> MedDRA terms always start with a capital letter, be sure to provide
> the exact case, e.g. Good : “**C**olitis”, Wrong : “colitis” or
> “COLITIS”.

Be sure all selected terms belong to the same hierarchical level
(preferred term, high level term…) in MedDRA. Here, we use Preferred
Terms.

#### Step 3: Identify event codes

The
[`get_llt_soc()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_soc.md)
function allows you to query the `meddra`.

``` r
a_llt <- 
  get_llt_soc(
    term_sel = a_sel_pt,
    term_level = "pt",
    meddra = meddra_
    )
#> 
#> ── get_llt_soc() ───────────────────────────────────────────────────────────────
#> 
#> ── ✔ Matched reactions at `pt` level (number of codes) ──
#> 
#> → `a_colitis`: "Autoimmune colitis (1)", "Colitis (25)", "Colitis microscopic
#>   (3)", "Diarrhoea (53)", "Diarrhoea haemorrhagic (8)", "Duodenitis (5)",
#>   "Enteritis (8)", "Enterocolitis (4)", "Enterocolitis haemorrhagic (10)", and
#>   "Ulcerative gastritis (1)"
#> 
#> 
#> ℹ Set `verbose` to FALSE to suppress this section.

a_llt
#> $a_colitis
#>   [1] 146319904  72535511 145048103  83961164  60502763  86999793  31902962
#>   [8]  74358151  93223730  42212111  47872019   9787331  97183180  76243579
#>  [15]  57421483  83801839  35994947 145921181 126960392 139876147   7485419
#>  [22]  56928874 106618606 136185109 134273207  69329666  71614386  39085290
#>  [29]  44792724  60161897 117694644  33268936  52457844   3588697  50677287
#>  [36]  86522225  52090591 115452495  57673555  36505933  97974705 116341322
#>  [43] 100087548  92141110  76778618 129074932  74911722  30822686 106700292
#>  [50]  40062048  94663343  99951844  10796290  16785547 100720721  26400336
#>  [57]  71857357   9581412 142354851 119711285  14275999 116525173  46785130
#>  [64]  69158837  92034682  11303229  32580170 122765038  94746500  97136680
#>  [71]  92371714 104194644  80678406 130746421   6544507  54366037  11722654
#>  [78]  23855288    996046 104846192  35477411  75848043   5029037  50188094
#>  [85]  84444296 136071015   5144633  62896477  85459432 142989340  22136645
#>  [92]  33346088  89785585  72565521  53464576 138014054  76736595 121450460
#>  [99]  52582197  39107722  31994263  68617678 128230716 114942138  24470833
#> [106]  27038937  54746042  71417752  12508751  74249211 109131610 121557512
#> [113]  18989134 118819900  11580419 137272208 137544759  76219908
```

An alternative is to use the
[`get_llt_smq()`](https://pharmacologie-caen.github.io/vigicaen/reference/get_llt_smq.md)
function, which allows you to query the `smq` tables.

Notice that you collect low level term codes, even if you work with
higher level terms, like preferred terms, or high level terms. This is
intentional: this list collects all low level term codes composing the
higher level term. See [Collecting ID section](#collecting_ids).

#### Steps 4 and 5: add_adr() function

The
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md)
function allows you to identify cases reporting the adverse event of
interest and add the corresponding column to `demo`.

``` r
demo <- 
  add_adr(
    .data = demo,
    a_code = a_llt,
    adr_data = adr)
#> ℹ `.data` detected as `demo` table.
```

#### Step 6: Check your data management

[`check_dm()`](https://pharmacologie-caen.github.io/vigicaen/reference/check_dm.md)
also works for adr.

``` r
demo |> 
  check_dm("a_colitis")
#>           [,1]
#> a_colitis  104
```

### Other variables

We may need to create other variables to perform our analysis, for
example age and sex in a multivariable analysis.

#### Indications

The `ind` table contains the indication data at the drug level.
Indications are encoded using either MedDRA or International
Classification of Diseases (ICD) codes. The exact ICD version depends on
the date of the report.

> If you wish to perform a full search of indications, you must enclose
> terms from both MedDRA and ICD.

You can add the indication columns to `demo` or other tables (`drug`,
`adr`, `link`, or even `ind`) with
[`add_ind()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_ind.md).

The process is simpler than for drugs and reactions, since there is no
`get_` step.

First, load the tables and pick the indication terms of interest.

``` r
# You need ind and drug, plus the table you will be working on, here demo.
ind <- ind_

i_list <-
  list2(
    melanoma = c("Malignant melanoma", "Metastatic malignant melanoma")
  )
```

Then, run
[`add_ind()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_ind.md).

``` r
demo <- 
  add_ind(
    .data = demo,
    i_list = i_list,
    drug_data = drug,
    ind_data  = ind
    )
#> ℹ `.data` detected as `demo` table.
```

As all drugs from all reports do not appear in the ind table, there
might be missing data in the columns created with
[`add_ind()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_ind.md)
(which couldn’t happen with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
or
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md).

Thus, you can’t use
[`check_dm()`](https://pharmacologie-caen.github.io/vigicaen/reference/check_dm.md),
which doesn’t work in case of missing data.
[`desc_facvar()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_facvar.md)
will do the job instead.

``` r
demo |> 
  desc_facvar("melanoma")
#> # A tibble: 2 × 4
#>   var      level value         n_avail
#>   <chr>    <chr> <chr>           <int>
#> 1 melanoma 0     574/660 (87%)     660
#> 2 melanoma 1     86/660 (13%)      660
```

#### Age

The `demo` table contains the `AgeGroup` column, which groups ages into
categories. You may want to recode it to match you research question

``` r
demo <-
  demo |>
  mutate(
    age = cut(as.integer(AgeGroup),
              breaks = c(0,4,5,6,7,8),
              include.lowest = TRUE, right = TRUE,
              labels = c("<18", "18-45","45-64", "65-74", "75+"))
  )
```

#### Sex

The `demo` table contains the `Gender` column, from which you can also
create a new sex column (with values 1 for men, 2 for women, and NA
otherwise)

``` r
demo <-
  demo |> 
  mutate(
    sex = ifelse(Gender == "1", 1,
                 ifelse(Gender == "2", 2, NA_real_)
                 )
    )
```

#### Using case_when()

The
[`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
function from the `dplyr` package allows you to manage multiple options
in a single function, with a slightly different syntax.

``` r
demo <- 
  demo |> 
  mutate(
    sex = case_when(Gender == "1" ~ 1,
                    Gender == "2" ~ 2,
                    TRUE ~ NA_real_)
    )
```

More documentation on
[`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
can be found in the `dplyr` package documentation.

You should just remember here that options are evaluated sequentially,
from top to bottom.

#### Seriousness, death

The `out` table contains the `Seriousness` column, which indicates
whether the case was serious or not, and whether the patient experienced
a fatal issue during his/her follow-up.

``` r
# ---- Serious ---- ####

out <- out_

demo <- 
  demo |> 
  mutate(
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

# ---- Death + outcome availability ---- ####

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
```

- The `serious` and `death` columns are coded with TRUE/FALSE values in
  this example. There is no particular reason to prefer it over 1/0
  codes. It is just a matter of preference.

- The `Seriousness` can have several levels, level 1 being death. (see
  subsidiary files)

## Disproportionality

Our `demo` dataset now has a drug column for nivolumab, and an adr
column for colitis.

We can perform a disproportionality analysis between these two
variables.

### Univariate analysis

#### Disproportionality metrics

Reporting Odds-Ratio (ROR) and Information Component essentially measure
the same thing: the disproportionality.

[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)
computes both of these.

- `or` and `or_ci` are the reporting Odds-Ratio and its confidence
  interval (default: 95%CI).

- `ic` and `ic_tail` are the Information Component, and its lower end of
  credibility interval (default: IC025).

``` r
demo |> 
  compute_dispro(
    y = "a_colitis",
    x = "nivolumab"
    )
#> # A tibble: 1 × 9
#>   y         x         n_obs n_exp or    or_ci          ic ic_tail ci_level
#>   <chr>     <chr>     <dbl> <dbl> <chr> <chr>       <dbl>   <dbl> <chr>   
#> 1 a_colitis nivolumab    44  31.2 1.88  (1.23-2.88) 0.489  0.0314 95%
```

### Advanced modelling, multivariate analysis

From this point, it is also possible to run any statistical model
including drug and adr parameters, but also potential other variables
such as age and sex. For example, one could wish to perform a
multivariate logistic regression on the reporting of colitis and
nivolumab, adjusted on age groups and sex.

The [`glm()`](https://rdrr.io/r/stats/glm.html) function from the
`stats` package can be used for this purpose.

``` r
mod <- glm(a_colitis ~ nivolumab, 
           data = demo, family = "binomial")

summary(mod)
#> 
#> Call:
#> glm(formula = a_colitis ~ nivolumab, family = "binomial", data = demo)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -2.0477     0.1372 -14.928  < 2e-16 ***
#> nivolumab     0.6334     0.2170   2.919  0.00351 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 603.80  on 749  degrees of freedom
#> Residual deviance: 595.53  on 748  degrees of freedom
#> AIC: 599.53
#> 
#> Number of Fisher Scoring iterations: 4
```

In a logistic regression models, estimates lead to (reporting) OR by the
exponential.

``` r
summary(mod)$coefficients
#>               Estimate Std. Error    z value     Pr(>|z|)
#> (Intercept) -2.0476928  0.1371736 -14.927752 2.174746e-50
#> nivolumab    0.6333854  0.2169532   2.919456 3.506429e-03

exp(summary(mod)$coefficients[2, 1])
#> [1] 1.883978
```

Adding covariates is straightforward

``` r
mod2 <- glm(a_colitis ~ nivolumab + sex + age,
            data = demo,
            family = "binomial")

summary(mod2)
#> 
#> Call:
#> glm(formula = a_colitis ~ nivolumab + sex + age, family = "binomial", 
#>     data = demo)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)  
#> (Intercept) -14.8881   882.7435  -0.017   0.9865  
#> nivolumab     0.4613     0.2679   1.722   0.0851 .
#> sex           0.1610     0.2517   0.640   0.5223  
#> age18-45     12.7789   882.7435   0.014   0.9884  
#> age45-64     12.7362   882.7434   0.014   0.9885  
#> age65-74     13.1715   882.7434   0.015   0.9881  
#> age75+       12.6702   882.7434   0.014   0.9885  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 436.14  on 489  degrees of freedom
#> Residual deviance: 429.24  on 483  degrees of freedom
#>   (260 observations deleted due to missingness)
#> AIC: 443.24
#> 
#> Number of Fisher Scoring iterations: 13
```

#### Extract Odds-Ratio with compute_or_mod()

There are several packages that can extract the OR from a model. The
[`compute_or_mod()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_or_mod.md)
function is just one of many ways to do it.

``` r
mod_or <- 
  compute_or_mod(
    summary(mod2)$coefficients,
    estimate = Estimate,
    std_er = Std..Error
    )

mod_or
#>             rn    Estimate  Std..Error     z.value   Pr...z..           or
#>         <char>       <num>       <num>       <num>      <num>        <num>
#> 1: (Intercept) -14.8881304 882.7435188 -0.01686575 0.98654372 3.421111e-07
#> 2:   nivolumab   0.4613151   0.2679460  1.72167184 0.08512898 1.586159e+00
#> 3:         sex   0.1610313   0.2516929  0.63979269 0.52230739 1.174722e+00
#> 4:    age18-45  12.7789374 882.7434892  0.01447639 0.98844992 3.546680e+05
#> 5:    age45-64  12.7361773 882.7434133  0.01442795 0.98848856 3.398220e+05
#> 6:    age65-74  13.1714980 882.7434134  0.01492109 0.98809513 5.251809e+05
#> 7:      age75+  12.6701788 882.7434409  0.01435318 0.98854821 3.181184e+05
#>       low_ci    up_ci        orl          ci ci_level signif_ror
#>        <num>    <num>     <char>      <char>   <char>      <num>
#> 1: 0.0000000      Inf       0.00  (0.00-Inf)      95%          0
#> 2: 0.9381463 2.681777       1.59 (0.94-2.68)      95%          0
#> 3: 0.7172881 1.923873       1.17 (0.72-1.92)      95%          0
#> 4: 0.0000000      Inf 354,668.00  (0.00-Inf)      95%          0
#> 5: 0.0000000      Inf 339,822.03  (0.00-Inf)      95%          0
#> 6: 0.0000000      Inf 525,180.88  (0.00-Inf)      95%          0
#> 7: 0.0000000      Inf 318,118.36  (0.00-Inf)      95%          0
```

## Conclusion

You’re now all set to create drugs and adrs columns into a `demo`
dataset. This is the first step to many modelling possibilities!

Where do you want to go next?

- Dive into descriptive features, such as time to onset, dechallenge,
  rechallenge, screening of drugs and adrs.
  [`vignette("descriptive")`](https://pharmacologie-caen.github.io/vigicaen/articles/descriptive.md)

- Learn on interactions in pharmacovigilance database
  [`vignette("interactions")`](https://pharmacologie-caen.github.io/vigicaen/articles/interactions.md)

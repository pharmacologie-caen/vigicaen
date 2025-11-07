# Time to onset extraction

**\[stable\]** `extract_tto()` collects all available time to onsets for
a set of drug-adr pairs.

## Usage

``` r
extract_tto(.data, adr_s, drug_s, tto_time_range = 1)
```

## Arguments

- .data:

  A `link` data.table. See
  [`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md).

- adr_s:

  A character string. The name of the adr column. (see details)

- drug_s:

  A character string. The name of the drug column. (see details)

- tto_time_range:

  Incertitude range of Time to onset, in days. Defaults to 1 as
  recommended by umc

## Value

A data.frame with

- All available time to onsets for this combination (column `tto_max`).

- `adr_s` and `drug_s`, same as input.

- `UMCReportId`, the unique identifier of the case.

## Details

Extraction of (maximum available) time between drug initiation and event
onset. This runs at the drug-adr pair level. You will need a `link`
data.table, see
[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
on which you have added drugs and adrs with
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md)
and
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md).
Uppsala Monitoring Centre recommends to use only cases where the
incertitude on time to onset is less than **1 day**. You can change this
with `tto_time_range`. You might want to use
[`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md)
to obtain summary statistics of time to onset, but `extract_tto()` is
useful to get the raw data and plot it, for instance with `ggplot2`.

## See also

[`link_`](https://pharmacologie-caen.github.io/vigicaen/reference/demo_.md),
[`desc_tto()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_tto.md),
[`add_drug()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_drug.md),
[`add_adr()`](https://pharmacologie-caen.github.io/vigicaen/reference/add_adr.md),
[`desc_dch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_dch.md),
[`desc_rch()`](https://pharmacologie-caen.github.io/vigicaen/reference/desc_rch.md)

## Examples

``` r
link_ <-
  link_ |>
  add_drug(
    d_code = ex_$d_groups_drecno,
    drug_data = drug_
  ) |>
  add_adr(
    a_code = ex_$a_llt,
    adr_data = adr_
  )
#> ℹ `.data` detected as `link` table.
#> ℹ `.data` detected as `link` table.

extract_tto(.data = link_,
         adr_s = "a_colitis",
         drug_s = "pd1")
#>    UMCReportId tto_max     adr_s drug_s
#> 1    108846594     205 a_colitis    pd1
#> 2     73027866     175 a_colitis    pd1
#> 3     87966085      36 a_colitis    pd1
#> 4     88371745     740 a_colitis    pd1
#> 5    138643678     379 a_colitis    pd1
#> 6     39936658      99 a_colitis    pd1
#> 7    142848064    1207 a_colitis    pd1
#> 8    140765885       5 a_colitis    pd1
#> 9     11372968      62 a_colitis    pd1
#> 10    24328271      88 a_colitis    pd1
#> 11    63102481      57 a_colitis    pd1
#> 12    34209616       8 a_colitis    pd1
#> 13    21293261      59 a_colitis    pd1
#> 14    81010378      27 a_colitis    pd1
#> 15    67492542      78 a_colitis    pd1
#> 16    58329610     541 a_colitis    pd1
#> 17   109965210       1 a_colitis    pd1
#> 18    84968360       4 a_colitis    pd1
#> 19    37122185      11 a_colitis    pd1
#> 20    77761414       3 a_colitis    pd1
#> 21    26302027     137 a_colitis    pd1
#> 22    38269558     393 a_colitis    pd1
#> 23    85691581     490 a_colitis    pd1
#> 24   112663221       7 a_colitis    pd1
#> 25      610256      52 a_colitis    pd1
#> 26   109716692      37 a_colitis    pd1
#> 27    56986472      47 a_colitis    pd1
#> 28     6112110      44 a_colitis    pd1
#> 29    76401465     390 a_colitis    pd1
#> 30   125056616      18 a_colitis    pd1
#> 31    33942691      36 a_colitis    pd1
#> 32    63782466      41 a_colitis    pd1
#> 33     1261650     286 a_colitis    pd1
#> 34    85774959     254 a_colitis    pd1
#> 35   133088642      23 a_colitis    pd1
#> 36   143881598     113 a_colitis    pd1
#> 37    79620779      85 a_colitis    pd1
#> 38    33962643      26 a_colitis    pd1
#> 39    95759941      59 a_colitis    pd1
extract_tto(.data = link_,
         adr_s = c("a_colitis", "a_pneumonitis"),
         drug_s = c("pd1", "ctla4"))
#>    UMCReportId tto_max         adr_s drug_s
#> 1    108846594     205     a_colitis    pd1
#> 2     73027866     175     a_colitis    pd1
#> 3     87966085      36     a_colitis    pd1
#> 4     88371745     740     a_colitis    pd1
#> 5    138643678     379     a_colitis    pd1
#> 6     39936658      99     a_colitis    pd1
#> 7    142848064    1207     a_colitis    pd1
#> 8    140765885       5     a_colitis    pd1
#> 9     11372968      62     a_colitis    pd1
#> 10    24328271      88     a_colitis    pd1
#> 11    63102481      57     a_colitis    pd1
#> 12    34209616       8     a_colitis    pd1
#> 13    21293261      59     a_colitis    pd1
#> 14    81010378      27     a_colitis    pd1
#> 15    67492542      78     a_colitis    pd1
#> 16    58329610     541     a_colitis    pd1
#> 17   109965210       1     a_colitis    pd1
#> 18    84968360       4     a_colitis    pd1
#> 19    37122185      11     a_colitis    pd1
#> 20    77761414       3     a_colitis    pd1
#> 21    26302027     137     a_colitis    pd1
#> 22    38269558     393     a_colitis    pd1
#> 23    85691581     490     a_colitis    pd1
#> 24   112663221       7     a_colitis    pd1
#> 25      610256      52     a_colitis    pd1
#> 26   109716692      37     a_colitis    pd1
#> 27    56986472      47     a_colitis    pd1
#> 28     6112110      44     a_colitis    pd1
#> 29    76401465     390     a_colitis    pd1
#> 30   125056616      18     a_colitis    pd1
#> 31    33942691      36     a_colitis    pd1
#> 32    63782466      41     a_colitis    pd1
#> 33     1261650     286     a_colitis    pd1
#> 34    85774959     254     a_colitis    pd1
#> 35   133088642      23     a_colitis    pd1
#> 36   143881598     113     a_colitis    pd1
#> 37    79620779      85     a_colitis    pd1
#> 38    33962643      26     a_colitis    pd1
#> 39    95759941      59     a_colitis    pd1
#> 40     2626997     107     a_colitis  ctla4
#> 41   140765885       5     a_colitis  ctla4
#> 42    11372968      15     a_colitis  ctla4
#> 43    12728402      76     a_colitis  ctla4
#> 44   112663221       7     a_colitis  ctla4
#> 45   109716692      37     a_colitis  ctla4
#> 46    56986472      47     a_colitis  ctla4
#> 47   125056616      18     a_colitis  ctla4
#> 48   143881598     113     a_colitis  ctla4
#> 49    95759941      59     a_colitis  ctla4
#> 50    51868324     602 a_pneumonitis    pd1
#> 51   141780031      43 a_pneumonitis    pd1
#> 52    38542967     105 a_pneumonitis    pd1
#> 53    46205163      90 a_pneumonitis    pd1
#> 54   143915039    1050 a_pneumonitis    pd1
#> 55     8037093      58 a_pneumonitis    pd1
#> 56    51289899       6 a_pneumonitis    pd1
#> 57     5827498     150 a_pneumonitis    pd1
#> 58   109804882      32 a_pneumonitis    pd1
#> 59    60410606      52 a_pneumonitis    pd1
#> 60    28278690     470 a_pneumonitis    pd1
#> 61    59468437      33 a_pneumonitis    pd1
#> 62    68574533      49 a_pneumonitis    pd1
#> 63   105702324      29 a_pneumonitis    pd1
#> 64    55788241     125 a_pneumonitis    pd1
#> 65   120048321       8 a_pneumonitis    pd1
#> 66    91145758      21 a_pneumonitis    pd1
#> 67    42289125     136 a_pneumonitis    pd1
#> 68    49019818      77 a_pneumonitis    pd1
#> 69   119065329      34 a_pneumonitis    pd1
#> 70    11932941      58 a_pneumonitis    pd1
#> 71   106102063      65 a_pneumonitis    pd1
#> 72    82344156      52 a_pneumonitis    pd1
#> 73    40884461      23 a_pneumonitis    pd1
#> 74    63322168     523 a_pneumonitis    pd1
#> 75   100871963      43 a_pneumonitis    pd1
#> 76   140323054      18 a_pneumonitis    pd1
#> 77   147304194     117 a_pneumonitis    pd1
#> 78    12314086      69 a_pneumonitis    pd1
#> 79   143881598     181 a_pneumonitis    pd1
#> 80     3460652     138 a_pneumonitis    pd1
#> 81    27273063      25 a_pneumonitis    pd1
#> 82    84451753       1 a_pneumonitis    pd1
#> 83    79620779     511 a_pneumonitis    pd1
#> 84   103773756      52 a_pneumonitis    pd1
#> 85   123322212       0 a_pneumonitis    pd1
#> 86   135108556     183 a_pneumonitis    pd1
#> 87    90067921     101 a_pneumonitis    pd1
#> 88    95759941      73 a_pneumonitis    pd1
#> 89    76017998      72 a_pneumonitis    pd1
#> 90    38542967     106 a_pneumonitis  ctla4
#> 91    46205163      90 a_pneumonitis  ctla4
#> 92    82344156      52 a_pneumonitis  ctla4
#> 93    40884461      23 a_pneumonitis  ctla4
#> 94    12314086      69 a_pneumonitis  ctla4
#> 95   143881598     181 a_pneumonitis  ctla4
#> 96   103773756      52 a_pneumonitis  ctla4
#> 97    95759941      73 a_pneumonitis  ctla4
```

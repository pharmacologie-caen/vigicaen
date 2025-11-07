# Credibility interval limits for the information component

**\[stable\]** Compute the Information Component credibility interval,
typically the lower end of the 95% CI, also known as the IC025.

## Usage

``` r
ic_tail(n_obs, n_exp, p = 0.025)
```

## Arguments

- n_obs:

  Number of observed cases

- n_exp:

  Number of expected cases (see Details)

- p:

  End of chosen credibility interval

## Value

A numeric vector. The lower end of the credibility interval

## Details

The ends of the credibility interval of the information component are
estimated with the gamma distribution. `n_exp` is defined as
`n_drug * n_event / n_total` for the basic IC (formula is different for
interactions) Do not add `+.5` to `n_obs` and `n_exp` as it is
automatically done in the function. By default, IC025 is computed.
Change `p` for different ends. It may be easier to use
[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md),
which internally calls this function.

## See also

[`compute_dispro()`](https://pharmacologie-caen.github.io/vigicaen/reference/compute_dispro.md)

## Examples

``` r
ic_tail(n_obs = 12,
        n_exp = 5)
#> [1] 0.2542334
```

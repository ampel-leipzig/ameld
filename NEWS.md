# ameld 0.0

## Changes in development

- Rename `meld_plus7` into `pmeld_plus7`.

## Changes in 0.0.16

- Add `main` to `plot.arcv.glmnet`.
- Add `plot_surv_roc`.
- Add `rjlegend`.

## Changes in 0.0.15

- Add `.prediction_error`, `.cutpoints` and `.cut`.
- Add `bootstrap` and `plot.boot.glmnet`.
- Add `which.min.error`.

## Changes in 0.0.14

- Add `groupmean`.

## Changes in 0.0.13

- Add `arcv.glmnet`.
- Integrate `rcv.glmnet` from https://github.com/ampel-leipzig/glmnettools.
- Use `future` backend for parallelisation.

## Changes in 0.0.12

- Allow vectorized `cause` argument in `meld` (again).

## Changes in 0.0.11

- Use lower case `"type"` argument in `meld_na` ("unos" instead of "UNOS").
- Restrict `meld(..., cause)` argument to
  `c("other", "unos", "ethyltoxic", "cholestatic")`.

## Changes in 0.0.10

- Add `enum` function.

## Changes in 0.0.9

- `observed_events` gains a `cumulative` argument to calculate cumulative
  events over time periods.

## Changes in 0.0.8

- Rename "calcium, total" and "bilirubin, total" to
  "total calcium" and "total bilirubin" in `eldr$LongDescription`

## Changes in 0.0.7

- Add `observed_vs_expected_mortality`.

## Changes in 0.0.6

- Add `observed_survival`, `observed_mortality`, and `observed_events`.

## Changes in 0.0.5

- Add `plot_dots`.

## Changes in 0.0.4

- Add MELD score calculation: `meld`, `meld_na`, `meld_plus7`
- Add `as_metric`, `as_si` to convert between different units.

## Changes in 0.0.3

- Add `plot_table`.

## Changes in 0.0.2

- Add `plot_surv`.
- Increment minimal R dependency to R 4.1.

## Changes in 0.0.1

- Initial version with `eldd` (data) and `eldr` (reference) dataset.

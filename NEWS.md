# ameld 0.0

## Changes in development

## Changes in 0.0.32

- Internal `.plot.glmnet`: code cleanup, no user-visible changes.
- Internal `arcv.glmnet`, `rcv.glmnet`, `plot.glmnet`: replace `seq(..., along)`
  with `seq_along` (avoid warning about partial argument matching).

## Changes in 0.0.31

- Add `confidencebands`.
- Add `legend` argument to `plot_surv_roc`.

## Changes in 0.0.30

- Print confidence intervals in `plot_surv_roc` legend.
- Plot confidence bands in `plot_surv_roc_trend`.

## Changes in 0.0.29

- Fix upper creatinine bound in `meld3`.

## Changes in 0.0.28

- Add `pmeld3`.

## Changes in 0.0.27

- Add `meld3`.

## Changes in 0.0.26

- Add CITATION file.

## Changes in 0.0.25

- Add `plot.rcvglmnet(..., what = "path")` to plot lambda path
  (extends `glmnet::plot.glmnet`).

## Changes in 0.0.24

- Pass `main` argument to `.plot.cal`, affected function: `plot.boot.glmnet`.

## Changes in 0.0.23

- Add `"times"` argument to `basehaz`.

## Changes in 0.0.22

- Add `basehaz` for `cv.glmnet` class.
- Add `legend` argument to `plot.boot.glmnet`.

## Changes in 0.0.21

- Fix `bootstrap`: adapt to exported `cutpoint` function
  (error was missing `.cutpoint`).

## Changes in 0.0.20

- Add grouping variable `f` to bootstrap output.
- Export `cutpoints`.

## Changes in 0.0.19

- Fix *ylab* position and *lty* order in legend in `plot_surv_roc`.
- Add `plot_surv_roc_trend`.

## Changes in 0.0.18

- Return a survival instead of a mortality probability for `pmeld_plus7`.

## Changes in 0.0.17

- Rename `meld_plus7` into `pmeld_plus7`.
- Add `pmeld`.

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

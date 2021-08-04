# ameld 0.0

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

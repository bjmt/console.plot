# console.plot 0.0.6 #

## Bug fixes ##

- For plots where `y = NULL`, don't ignore `ylab` if specified. (PR #1 by
  @rkmittal)

## Misc ##

- Updated LICENSE

# console.plot 0.0.5 #

## User-facing ##

- added README
- more tweaks to xaxis tick labels

# console.plot 0.0.4 #

## User-facing ##

- plots with `plot.height = 5` now display correctly
- xaxis tick labels now better align with ticks

## Internal ##

- got rid of hex codes
- added stats import
- fixed approx errors from duplicate x values

# console.plot 0.0.3 #

## User-facing ##

- new option: `options(ascii = FALSE)`

## Internal ##

- fixed bug where `substr` reported `character(0)`
- code re-organisation: no longer have to write code for both ASCII and
  non-ASCII symbols separately

# console.plot 0.0.2 #

## User-facing ##

- finished adding ASCII-only plots
- added `horizontal` param (does nothing as of yet)
- improved appearance of abline with non-ASCII characters

## Internal ##

- fixed bug where abline would appear twice
- fixed data not respecting user-set xlim, ylim

# console.plot 0.0.1 #

- Initial commit

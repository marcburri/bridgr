## Test environments

* local macOS 26.5, R 4.4.0
* win-builder, R-devel and R-release
* macOS builder (aarch64), R 4.6.1 Patched -- OK, no notes
* R-hub: r-devel-linux-x86_64-debian-clang, r-release-linux-x86_64, noLD,
  noSuggests, vnu

## R CMD check results

0 errors | 0 warnings | 2 notes locally.

Both local notes are artefacts of the check machine (macOS, R 4.4.0):

* "checking for future file timestamps ... unable to verify current time",
  which is the time server being unreachable.
* "checking HTML version of manual", from an HTML Tidy build dating to 2006
  that does not recognise the `<main>` element emitted by current R. The
  R-hub 'vnu' HTML validation platform reports no problems.

win-builder additionally reports one note with two parts:

* Possibly misspelled words in DESCRIPTION: Andreou, Ghysels, Kourtellos,
  Sinko, Valkanov. These are the surnames of the authors of the papers cited
  in the Description field and are spelled correctly.
* A possibly invalid ORCID iD, 0000-0001-8974-9090. The iD is correct: its
  ISO 7064 MOD 11-2 check digit is valid, the iD resolves at
  <https://orcid.org/0000-0001-8974-9090>, and the check passes here. The
  note appears to come from a transient failure to reach orcid.org from the
  check machine.

## Submission Summary

This is the first stable release of 'bridgr', following a substantial API
redesign since 0.1.2. The model-construction entry point and the fitted-model
class were renamed from `bridge` to `mf_model`, and `bridge()` remains as a
deprecated compatibility wrapper that warns and forwards, so existing code
keeps working.

It also contains two user-visible breaking changes, both correcting methods
that did not honour their documented contract:

* `summary()` on an `"mf_model"` object now returns a `"summary.mf_model"`
  object, following the convention of `summary.lm()`, instead of printing and
  returning the model unchanged. The printed report is unchanged. The returned
  object exposes the summary quantities programmatically, including a standard
  `coefficients` matrix, so `coef(summary(model))` works as it does for `lm()`.

* Objects returned by `forecast()` no longer inherit from the `forecast`
  package's `"forecast"` class. The inheritance was not honoured: `plot()` and
  `autoplot()` failed on the result and `accuracy()` returned misleading
  values, because target frequencies supported by this package include daily
  and weekly series that `stats::ts()` cannot represent exactly. `plot()` and
  `autoplot()` methods are now provided directly for the returned class, and
  the new `as.forecast()` converts to a genuine `"forecast"` object where the
  target frequency has an exact `ts` representation.

The release also adds accessor methods (`indicators()`, `weights()`,
`aggregation_parameters()`, `variable.names()`, `model.frame()`) so that
results can be extracted without reaching into the object's internal
structure, and speeds up the full-system block bootstrap.

Reverse dependencies: none.

# sked / drudg

Source repository for **sked** (the IVS geodetic-VLBI scheduling program) and its
companion tools **drudg**, and the supporting libraries (`skdrut`, `skdrincl`,
`skdrlnfch`, `matrix`, `curses`, `vex`, …).

## Repository history

The git history in this repository was **reconstructed** from the public snapshot
archive published by the IVS Analysis Coordinator:

> https://ivscc.gsfc.nasa.gov/IVS_AC/sked_cat/  (the `skedall_*.tgz` releases)

One commit was created per published snapshot, in chronological order, spanning
**2020-01-06 → 2025-09-18** (26 snapshots). From that point on the history is a
normal, live development history.

### Honesty caveats

These apply to every commit **dated 2025-09-18 or earlier** (the reconstructed
prefix):

- **Dates are snapshot-nominal, not per-change.** Each commit is dated from the
  release's filename/publication date. A single commit therefore collapses
  everything that changed between two published `skedall` snapshots — sometimes
  weeks or months of edits.
- **`git blame` / `git log` granularity is coarse** for the reconstructed prefix:
  a whole release is attributed to one commit and one author.
- **Authorship** on the reconstructed commits is set to the sked maintainer
  (John Gipson); it does not distinguish individual contributors within a release.
- **Only source, configuration and scripts are tracked.** Each snapshot was
  filtered before committing — the following were stripped:
  - build artifacts: `*.o`, `*.a`, `*.mod`, compiled binaries (`sked`, `drudg`,
    `*.lnx`, `*.hpux`, …)
  - editor backups and old/temp source variants: `*~`, `*.f0`, `*.fn`, `*.fold`,
    `*.old`, `*.bak`, `*.orig`
  - nested backup / abandoned-experimental copy trees bundled inside some
    tarballs: `*_old` (`drudg_old`, `sked_old`, `skdrut_old`), `sked_new`,
    `sked2`, and the dated `sked_2020Apr20`
  - packaging archives (`*.tgz`/`*.tar`/`*.zip`)

  `drudg2` (the second-generation drudg, which appears in the 2025 releases and
  persists to HEAD) is a **real component** and is kept.

The file-count curve of the reconstructed history matches the known evolution of
the codebase: ~750–830 tracked source files through 2020–2023, rising to ~910 in
2025 when `drudg2` was added.

## Building

See `make_sked` and the `set_misc*` environment scripts at the repository root.

## License

GNU General Public License v3.0 — see [`LICENSE`](LICENSE). `drudg` is part of the
VLBI Field System (https://github.com/nvi-inc/fs) and its sources carry the standard
`Copyright (c) NVI, Inc.` GPLv3 headers; the whole tree is distributed under the same
terms, consistent with the other `nvi-inc` Field System repositories.

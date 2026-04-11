# Parity benchmark — elm-spa-example

**Date:** 2026-04-11
**Fixture:** `rtfeldman/elm-spa-example` src (34 `.elm` files, ~224 KB) with
a fresh modern review config (`jfmengels/elm-review 2.16.4`) enabling 6
rules: `NoUnused.CustomTypeConstructors`, `NoUnused.CustomTypeConstructorArgs`,
`NoUnused.Exports`, `NoUnused.Parameters`, `NoUnused.Patterns`,
`NoUnused.Variables`.

**Commit (our runner):** post-Phase-5-cleanup (`9b7cdb7`).

## Headline numbers

Both runners agree on correctness: **187 errors across 28 files**.

| Runner                        | Cold wall | Warm wall | Cold internal | Warm internal |
|-------------------------------|---------:|---------:|---------------:|---------------:|
| Our review-runner (bundled)   | **53.33 s** | **0.91 s** | 46.27 s        | 0.63 s         |
| elm-review CLI 2.13.5 (full cold, no elm-stuff) | **1.82 s** | — | —  | —              |
| elm-review CLI (config-warm, analysis-cold)     | 0.49 s   | —        | —              | —              |
| elm-review CLI (fully warm)   | —        | **0.37 s** | —             | —              |

## Parity ratios

- **Cold:** 53.3 s ÷ 1.82 s ≈ **29× slower** vs full-cold CLI
  - Against the more apples-to-apples comparison (CLI with its review-app
    already compiled, result-cache empty), the gap is ~109×. This is the
    cost of interpreting Elm-authored rules through the Elm interpreter
    instead of running elm-review-CLI's pre-JIT'd JavaScript.
- **Warm:** 0.91 s ÷ 0.37 s ≈ **2.5× slower**
  - This is where the new review-runner architecture actually shines:
    the rule-cache + content-addressed reuse layer brings warm latency
    within a small constant factor of the JS-native CLI. The bulk of
    warm time is project-rule skip-check + cache lookup, not interpreted
    rule bodies.

## Our runner's cold breakdown (major stages, ms)

| Stage                 | ms    | % of 46273 |
|-----------------------|------:|-----------:|
| project_rule_eval     | 32932 | 71%        |
| module_rule_eval      | 11081 | 24%        |
| load_review_project   |  1341 |  3%        |
| prepare_config        |   629 |  1%        |
| get_rule_info         |   169 |  0.4%      |

Same shape as `small-12` after Phase 5 — project-rule eval continues to
dominate cold. The "what's left" headroom is still the Phase 5
persistent-value-injection / Salsa-style cache (documented as a
follow-on in `phase5-fallback-thunk.md`).

## Fixture setup

Not committed to the repo — recreate with:

```bash
FIXTURE=/tmp/elm-spa-parity
rm -rf "$FIXTURE" && mkdir -p "$FIXTURE/review/src"
cp -r ~/src/github.com/rtfeldman/elm-spa-example/src "$FIXTURE/"
cp    ~/src/github.com/rtfeldman/elm-spa-example/elm.json "$FIXTURE/"
# Write review/elm.json pinned to jfmengels/elm-review 2.16.4.
# Write review/src/ReviewConfig.elm importing the 6 rules above.
```

## Notes

- **Why not elm-spa-example as-is?** Its `review/elm.json` pins
  `jfmengels/elm-review 2.13.1`. Our bundled runner's `Review.Rule`
  implementation expects the 2.16.x internals and silently returned 0
  errors on the older package version. We created a fresh review/ with
  2.16.4 to get matching semantics across both runners.
- **Simplify + NoDeprecated + NoMissingTypeAnnotation* not included.**
  The 2.13-era `review/src/ReviewConfig.elm` in elm-spa-example had these
  disabled for our runner. We kept the rule set to ones both runners
  support identically so the 187-error correctness check is meaningful.

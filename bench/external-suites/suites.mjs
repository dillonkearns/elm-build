export const externalSuites = [
  {
    id: "elm-review",
    repo: "https://github.com/jfmengels/elm-review.git",
    branch: "main",
    description: "Primary elm-review package test suite",
    tags: ["starter", "elm-review"],
    timeoutSecs: 600,
  },
  {
    id: "elm-review-common",
    repo: "https://github.com/jfmengels/elm-review-common.git",
    branch: "main",
    description: "Common elm-review rule package",
    tags: ["starter", "elm-review-rule"],
    timeoutSecs: 300,
  },
  {
    id: "elm-review-unused",
    repo: "https://github.com/jfmengels/elm-review-unused.git",
    branch: "main",
    description: "Unused-code elm-review rules",
    tags: ["starter", "elm-review-rule"],
    timeoutSecs: 300,
  },
  {
    id: "elm-review-simplify",
    repo: "https://github.com/jfmengels/elm-review-simplify.git",
    branch: "main",
    description: "Large simplification-focused elm-review rule suite",
    tags: ["starter", "elm-review-rule"],
    timeoutSecs: 600,
  },
];

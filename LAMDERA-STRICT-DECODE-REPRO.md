# Lamdera Strict Decode Repro

This repo now has a minimal diagnostic script at:

- [/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/LamderaStrictDecodeProgression.elm](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/LamderaStrictDecodeProgression.elm)

Run it with:

```bash
npx elm-pages run src/LamderaStrictDecodeProgression.elm
```

Current observed output:

```text
Lamdera strict decode progression
  first strict failure: primitive:int

  - primitive:int bytes=1 lenient=true strict=false
```

It also logs the kernel-side exception from `bytesDecodeStrict`:

```text
bytesDecodeStrict unexpected error: TypeError: fun is not a function
```

What this means:

- `Lamdera.Wire3.bytesDecode` works for the primitive `Int` case.
- `Lamdera.Wire3.bytesDecodeStrict` fails before any AST-specific logic.
- The first reproducible failing checkpoint is the primitive case:
  - `W.encodeInt 42`
  - `W.decodeInt`

This is a better starting point than the AST repro, because it removes `AstWireCodec` from the equation entirely.

Related files:

- [/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/AstWireCodec.elm](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/AstWireCodec.elm)
- [/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/AstWireCodecTests.elm](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/AstWireCodecTests.elm)
- [/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/AstWireCodecSingleModuleRepro.elm](/Users/dillonkearns/src/github.com/dillonkearns/elm-build2/src/AstWireCodecSingleModuleRepro.elm)

Additional finding from this debugging pass:

- `AstWireCodec` had a real framing bug where it used `encodeSequence` instead of `encodeSequenceWithoutLength`.
- After fixing that, normal `bytesDecode` roundtrips now pass the local regression suite.
- The remaining strict failure is independent of the AST codec.

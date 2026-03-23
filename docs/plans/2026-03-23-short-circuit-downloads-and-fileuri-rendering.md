# Short-Circuit Downloads and FileURI Rendering Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate redundant fallback download requests in `Interface.API` and remove hot-path `FileURI -> String -> Text/ByteString` conversions.

**Architecture:** This refactor has two parts. First, replace eager `asum <$> traverse ...` download selection with a monadic short-circuit loop that stops after the first successful URL. Second, introduce canonical `FileURI` render helpers for `ByteString` and `Text`, keep `Show` only for debugging, and migrate hot-path storage and RPC code to the new renderers. The plan preserves behavior and public interfaces except for adding explicit rendering helpers.

**Tech Stack:** Haskell, Polysemy, Servant, strict `ByteString`, `Text`, existing `hspec` test suite, Stack.

---

## Constraints and Preconditions

1. `stack test --fast` is currently red for unrelated reasons in `test/Integration.hs`, `test/FilesystemSpec.hs`, and `test/R2Spec.hs`. This plan does **not** include repairing those unrelated failures.
2. Every step below uses **targeted** verification first: `stack build hs-hath:lib hs-hath:exe:hs-hath hs-hath:exe:migrate-to-r2`.
3. Add new unit tests in isolated modules, but do not claim full suite success until the unrelated test failures are fixed in a separate task.
4. Preserve external behavior:
   - download order stays left-to-right;
   - the first successful URL still wins;
   - canonical file IDs stay byte-for-byte identical to current output.

---

### Task 1: Add focused tests for short-circuit selection

**Files:**
- Create: `test/InterfaceAPIOptimizationSpec.hs`
- Modify: `test/Spec.hs`

**Step 1: Add a pure or IO-testable helper target**

Plan the tests around a new helper with semantics equivalent to:

```haskell
firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
```

The tests should not hit the network. They should use an `IORef [Text]` or `State`-style accumulator to record which inputs were evaluated.

**Step 2: Write the failing tests**

Add tests that prove:

```haskell
it "stops after the first Just" $ do
  -- evaluator records every visited URL
  -- expected visit order: only the prefix up to the first success

it "returns Nothing after visiting every candidate when all fail" $ do
  -- expected: all URLs visited, result Nothing

it "preserves left-to-right evaluation order" $ do
  -- expected visit order matches input order
```

**Step 3: Wire the test module into `test/Spec.hs`**

Add the new spec import and invoke it from `main`.

**Step 4: Run targeted verification**

Run:

```bash
stack build hs-hath:lib hs-hath:exe:hs-hath hs-hath:exe:migrate-to-r2
```

Expected: library and executables build successfully.

**Step 5: Attempt targeted test compile if feasible**

Run:

```bash
stack test --fast --ta '--match "Interface.API optimization"'
```

Expected: this may still fail because of unrelated pre-existing test breakage. Record the unrelated failures without widening scope.

**Step 6: Commit**

```bash
jj commit -m "test: add short-circuit selection regression specs"
```

---

### Task 2: Implement monadic short-circuit selection in `Interface.API`

**Files:**
- Modify: `src/Interface/API.hs:420-459`
- Test: `test/InterfaceAPIOptimizationSpec.hs`

**Step 1: Add a local helper near the download logic**

Introduce a helper with explicit recursion:

```haskell
firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x:xs) = f x >>= maybe (firstJustM f xs) (pure . Just)
```

Keep it local to `Interface.API` unless another module needs it.

**Step 2: Refactor `downloadGalleryFile`**

Replace:

```haskell
maybe (operate (retries + 1)) (pure . Just) . asum
  =<< traverse (download . BSC.unpack) urls
```

with a call to `firstJustM`:

```haskell
firstJustM (download . BSC.unpack) urls >>= maybe (operate (retries + 1)) (pure . Just)
```

**Step 3: Refactor `fetchResource`**

Replace:

```haskell
asum <$> traverse (download token . BSC.unpack) urls
```

with:

```haskell
firstJustM (download token . BSC.unpack) urls
```

**Step 4: Keep behavior comments close to the code**

Add a short comment explaining why the helper is required:

```haskell
-- `traverse` would execute every fallback download eagerly.
-- We need monadic short-circuiting to stop after the first success.
```

**Step 5: Run targeted verification**

Run:

```bash
stack build hs-hath:lib hs-hath:exe:hs-hath hs-hath:exe:migrate-to-r2
```

Expected: PASS.

**Step 6: Run focused tests if the harness is usable**

Run:

```bash
stack test --fast --ta '--match "Interface.API optimization"'
```

Expected: the new specs pass once unrelated test failures are removed from the path.

**Step 7: Commit**

```bash
jj commit -m "perf: short-circuit fallback downloads"
```

---

### Task 3: Add canonical `FileURI` render helpers

**Files:**
- Modify: `src/Types.hs:244-351`
- Create or update docs in code comments only

**Step 1: Add explicit render helpers to the export list**

Export two new helpers:

```haskell
renderFileURIText
renderFileURIBytes
```

If needed for path building, also add:

```haskell
renderFileURIPrefixText
```

Only add the helpers actually used by call sites.

**Step 2: Implement `ByteString` rendering without `String`**

Use a builder-based or strict-`ByteString` implementation. Example shape:

```haskell
renderFileURIBytes :: FileURI -> ByteString
renderFileURIBytes FileURI{..} =
  toStrictByteString
    ( byteString fileHash
   <> "-"
   <> intDec fileSize
   <> "-"
   <> intDec fileXRes
   <> "-"
   <> intDec fileYRes
   <> "-"
   <> byteString fileExt
    )
```

If `Mason.Builder` is already in scope elsewhere and fits project style, prefer it over ad-hoc `show`.

**Step 3: Implement `Text` rendering from the byte renderer**

Use UTF-8 decode from the canonical byte representation:

```haskell
renderFileURIText :: FileURI -> Text
renderFileURIText = decodeUtf8 . renderFileURIBytes
```

This is safe because file IDs are ASCII.

**Step 4: Rewrite the `Show FileURI` instance**

Keep `Show` for debugging only:

```haskell
instance Show FileURI where
  show = toString . renderFileURIText
```

This preserves visible debugging behavior while moving hot paths off `String`.

**Step 5: Add pure regression tests**

Add tests that prove:

```haskell
renderFileURIBytes (parseFileURI fid) == fid
renderFileURIText uri == decodeUtf8 (renderFileURIBytes uri)
show uri == toString (renderFileURIText uri)
```

Use canonical file IDs such as:

```haskell
"abcdef1234567890-12345-1024-768-jpg"
```

**Step 6: Run targeted verification**

Run:

```bash
stack build hs-hath:lib
```

Expected: PASS.

**Step 7: Commit**

```bash
jj commit -m "refactor: add explicit FileURI render helpers"
```

---

### Task 4: Migrate hot-path call sites off `show`

**Files:**
- Modify: `src/Storage/Database.hs:44-45,69-71`
- Modify: `src/Storage/R2.hs:85-88`
- Modify: `src/Storage/Filesystem.hs:38-43`
- Modify: `src/Storage/Locate.hs:78-84`
- Modify: `src/Types.hs:343-351`
- Modify: `src/Interface/API.hs:450-459`
- Modify: `app/Migrate.hs:274-277`

**Step 1: Migrate database keys**

Replace:

```haskell
show @Text uri
show @ByteString uri
```

with:

```haskell
renderFileURIText uri
renderFileURIBytes uri
```

**Step 2: Migrate R2 object key generation**

Replace:

```haskell
fileId = show uri
```

with:

```haskell
fileId = renderFileURIText uri
```

Keep the `s4` logic unchanged.

**Step 3: Migrate filesystem path generation**

Replace:

```haskell
fileId = takeFileName (show uri)
```

with:

```haskell
fileId = takeFileName (toString (renderFileURIText uri))
```

If you can prove the rendered file ID never contains `/`, drop `takeFileName` entirely in a separate, explicitly reviewed step. Do not mix that semantic change into this refactor.

**Step 4: Migrate record construction**

Replace:

```haskell
fileRecordFileId = show fileURI
fileId = T.pack $ show uri
```

with:

```haskell
fileRecordFileId = renderFileURIText fileURI
fileId = renderFileURIText uri
```

**Step 5: Migrate request token construction in `fetchResource`**

Replace:

```haskell
encodeUtf8 (key cfg) <> show fileURI
```

with:

```haskell
encodeUtf8 (key cfg) <> renderFileURIBytes fileURI
```

This avoids `String` allocation in a request-path token.

**Step 6: Migrate migration-tool key generation**

Replace the local `fileURIToKey` helper to use `renderFileURIText`.

**Step 7: Search for remaining hot-path `show uri` usage**

Run:

```bash
rg -n "show\s+@?(Text|ByteString)?\s*uri|show\s+uri|show\s+fileURI|T\.pack \$ show" src app -g '*.hs'
```

Expected: only debugging-oriented uses remain, or no matches remain.

**Step 8: Run targeted verification**

Run:

```bash
stack build hs-hath:lib hs-hath:exe:hs-hath hs-hath:exe:migrate-to-r2
```

Expected: PASS.

**Step 9: Commit**

```bash
jj commit -m "perf: remove hot-path FileURI string conversions"
```

---

### Task 5: Add equivalence checks and document trade-offs

**Files:**
- Modify: `test/InterfaceAPIOptimizationSpec.hs`
- Create: `test/FileURIRenderSpec.hs`
- Modify: `test/Spec.hs`
- Optionally modify: `docs/plans/2026-03-23-short-circuit-downloads-and-fileuri-rendering.md`

**Step 1: Add `FileURI` equivalence tests**

Add explicit examples for:

- canonical image IDs;
- video-like IDs;
- small and large numeric fields.

**Step 2: Add a comment on the semantic boundary**

Document in tests and code comments:

- `renderFileURIBytes . parseFileURI` is only guaranteed for canonical file IDs;
- non-canonical garbage input is out of scope for the renderer equivalence law.

**Step 3: Run targeted verification**

Run:

```bash
stack build hs-hath:lib hs-hath:exe:hs-hath hs-hath:exe:migrate-to-r2
```

Expected: PASS.

**Step 4: Record the known test-suite limitation**

Run:

```bash
stack test --fast
```

Expected: FAIL for the same unrelated pre-existing issues unless they were fixed separately. Record the failure as pre-existing, not introduced by this refactor.

**Step 5: Commit**

```bash
jj commit -m "test: cover FileURI rendering equivalence"
```

---

## Equational Reasoning Notes

### Spot 1: short-circuiting

Current form:

```haskell
asum <$> traverse f xs
```

Reasoning:

```haskell
traverse f [x1, x2, x3]
= (:) <$> f x1 <*> ((:) <$> f x2 <*> ((:) <$> f x3 <*> pure []))
```

So all effects run before `asum` inspects the resulting list. Replacing this with explicit recursion preserves result semantics while changing effect behavior to the intended short-circuit semantics.

### Spot 2: canonical rendering

Current hot path often does:

```haskell
parseFileURI fid
...
show uri
```

For canonical file IDs:

```haskell
renderFileURIBytes (parseFileURI fid) = fid
```

So carrying or re-rendering the canonical byte form is equivalent to the current output while avoiding the `ByteString -> String -> Text/ByteString` round-trip.

---

## Risks and Review Checklist

- Do not change fallback URL order.
- Do not parallelize fallback attempts in this refactor.
- Do not weaken hash verification.
- Do not remove `takeFileName` from filesystem paths unless reviewed separately.
- Do not assume `parseFileURI` round-trips arbitrary malformed input; only canonical IDs are in scope.
- Re-run `rg` after migration to ensure hot-path `show` calls are gone.

---

**Plan complete and saved to `docs/plans/2026-03-23-short-circuit-downloads-and-fileuri-rendering.md`. Two execution options:**

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

**Which approach?**

---

## Implementation Results

### 2026-03-23 Progress

- Implemented `firstJustM` in `src/Utils.hs`.
- Replaced eager fallback selection in `src/Interface/API.hs` with monadic short-circuiting for both `downloadGalleryFile` and `fetchResource`.
- Added focused standalone Hspec regression script `test/InterfaceAPIOptimizationSpec.hs`.
- Added `renderFileURIBytes` and `renderFileURIText` in `src/Types.hs` and rewrote `Show FileURI` to use the canonical renderer.
- Migrated hot-path `FileURI` rendering in:
  - `src/Storage/Database.hs`
  - `src/Storage/R2.hs`
  - `src/Storage/Filesystem.hs`
  - `src/Storage/Locate.hs`
  - `src/Interface/API.hs`
  - `src/Types.hs`
  - `app/Migrate.hs`
- Added focused standalone Hspec regression script `test/FileURIRenderSpec.hs`.
- Updated `test/Spec.hs` to include the new spec modules.

### Verification Evidence

Commands run after implementation:

```bash
stack runghc -- ... test/InterfaceAPIOptimizationSpec.hs
stack runghc -- ... test/FileURIRenderSpec.hs
stack build hs-hath:lib hs-hath:exe:hs-hath hs-hath:exe:migrate-to-r2
stack test --fast
```

Observed results:

- Both standalone focused Hspec scripts passed: `3 examples, 0 failures` each.
- Targeted build for library and executables passed.
- `stack test --fast` still fails for pre-existing unrelated test-suite issues in `test/FilesystemSpec.hs`, `test/R2Spec.hs`, and `test/Integration.hs`.
- During implementation, one compile regression in `src/Storage/Locate.hs` (`ShortByteString` vs `ByteString`) was caught and fixed before final verification.

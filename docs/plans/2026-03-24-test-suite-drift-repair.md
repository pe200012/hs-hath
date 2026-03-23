# Test Suite Drift Repair Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Repair the pre-existing test-suite compile failures so `stack test --fast` can progress beyond component drift errors.

**Architecture:** This repair addresses three independent drift classes. First, expose the vendored internal library `hs-hath-polysemy-zoo` to the test suite so `Polysemy.KVStore` imports compile. Second, update integration test fixtures to match the current `ClientConfig` shape by filling `lruCacheSize`. Third, add a narrow `Server.mkServerLoopContext` helper so tests can construct a `ServerLoopContext` without reaching into newly internalized runtime records. The helper keeps the production API disciplined while letting tests adapt to the refactor.

**Tech Stack:** Haskell, Stack, Hpack (`package.yaml`), Hspec, Polysemy, STM.

---

## Constraints and Current Evidence

Fresh evidence from `stack test --fast` on 2026-03-24 shows three compile-time root causes:

1. `test/FilesystemSpec.hs` and `test/R2Spec.hs` fail with hidden package errors for `Polysemy.KVStore` because the test suite does not depend on `hs-hath-polysemy-zoo`.
2. `test/Integration.hs:testConfig` omits the strict `lruCacheSize :: Int64` field now required by `ClientConfig`.
3. `test/Integration.hs:withTestServer` still calls the old many-argument `makeApplication`, but production code now uses `makeApplication :: ServerLoopContext -> Application`.

This plan fixes those drift issues first. If new failures appear after these repairs, handle them in a follow-up task rather than guessing now.

---

### Task 1: Expose `hs-hath-polysemy-zoo` to the test suite

**Files:**
- Modify: `package.yaml:tests.hs-hath-test.dependencies`
- Verify generated output via: `hs-hath.cabal:test-suite hs-hath-test.build-depends`
- Test: `test/FilesystemSpec.hs`, `test/R2Spec.hs`

**Step 1: Use the existing failing test-suite compile as RED**

Current failing evidence:

```text
Could not load module ‘Polysemy.KVStore’
It is a member of the hidden package ‘hs-hath-1.1.2:hs-hath-polysemy-zoo’
```

This is the failing test signal for this task.

**Step 2: Add the missing internal library dependency**

In `package.yaml`, under:

```yaml
tests:
  hs-hath-test:
    dependencies:
```

add:

```yaml
      - hs-hath-polysemy-zoo
```

Do not add extra internal libraries unless the next compile failure proves they are needed.

**Step 3: Verify the generated cabal file reflects the change**

Run:

```bash
stack build --dry-run
```

Then confirm `hs-hath.cabal` contains `hs-hath-polysemy-zoo` in `test-suite hs-hath-test` `build-depends`.

**Step 4: Re-run the failing suite compile**

Run:

```bash
stack test --fast
```

Expected: the hidden-package errors in `FilesystemSpec.hs` and `R2Spec.hs` disappear, exposing the next drift failures.

**Step 5: Commit**

```bash
jj commit -m "test: expose polysemy zoo to test suite"
```

---

### Task 2: Repair `ClientConfig` fixture drift in integration tests

**Files:**
- Modify: `test/Integration.hs:120-136`
- Reference: `src/Types.hs:82-100`

**Step 1: Use the existing failing fixture compile as RED**

Current failing evidence:

```text
Constructor ‘ClientConfig’ does not have the required strict field(s):
  lruCacheSize :: Int64
```

This is the failing test signal for this task.

**Step 2: Update the fixture minimally**

Modify `testConfig` in `test/Integration.hs` so it includes:

```haskell
  , lruCacheSize = 100 * 1024 * 1024
```

Prefer the same default used in production `defaultClientConfig` unless a test-specific value is required.

**Step 3: Re-run the suite compile**

Run:

```bash
stack test --fast
```

Expected: the `ClientConfig` constructor error disappears, leaving only the `makeApplication` API drift and any later failures.

**Step 4: Commit**

```bash
jj commit -m "test: update integration config fixture"
```

---

### Task 3: Add a narrow `mkServerLoopContext` helper for tests

**Files:**
- Modify: `src/Server.hs:1-20,181-209,482-490`
- Modify: `test/Integration.hs:45-60,138-160`

**Step 1: Use the existing failing call-site compile as RED**

Current failing evidence:

```text
The function ‘makeApplication’ is applied to 10 visible arguments,
but its type ‘ServerLoopContext -> Application’ has only three
```

This is the failing test signal for this task.

**Step 2: Add a focused constructor helper in `Server`**

Export a new helper from `src/Server.hs`:

```haskell
mkServerLoopContext
  :: ClientConfig
  -> TVar HathSettings
  -> TChan ServerAction
  -> TChan GalleryTask
  -> Bool
  -> Bool
  -> CacheRunner
  -> StatsEnv
  -> ServerLoopContext
```

Implementation should be a thin record constructor:

```haskell
mkServerLoopContext cfg settings action gallery disableRL trustProxy cacheRunner statsEnv =
  ServerLoopContext
    { loopConfig = cfg
    , loopChannels = ServerChannels
        { serverActionChan = action
        , galleryTaskChan = gallery
        }
    , loopState = ServerState
        { serverSettings = settings
        , serverStatsEnv = statsEnv
        }
    , loopFlags = ServerFlags
        { disableRateLimit = disableRL
        , trustProxyHeaders = trustProxy
        }
    , loopCacheRunner = cacheRunner
    }
```

Do not export the individual runtime record types just to satisfy tests.

**Step 3: Refactor `withTestServer` to use the helper**

In `test/Integration.hs`, replace the old `makeApplication ...many args...` call with:

```haskell
let ctx = mkServerLoopContext clientCfg ssVar serverChan galleryHandler False False cacheRunner statsEnv
    app = makeApplication ctx
```

Remove the obsolete `ipMap` / `ksVar` setup if it becomes unused.

**Step 4: Re-run the suite compile**

Run:

```bash
stack test --fast
```

Expected: the `makeApplication` compile errors disappear. If further failures emerge, they are new evidence for the next debugging step.

**Step 5: Commit**

```bash
jj commit -m "test: adapt integration helper to server context"
```

---

### Task 4: Clean up newly exposed test compile issues and verify full suite status

**Files:**
- Modify only if the next `stack test --fast` output proves another compile drift exists
- Otherwise no code changes

**Step 1: Run the full test command fresh**

Run:

```bash
stack test --fast
```

**Step 2: Interpret results honestly**

Possible outcomes:

- **Best case:** tests compile and run; proceed to fixing any runtime failures if present.
- **Expected case:** one or more additional compile/test failures surface now that the first blockers are gone.

**Step 3: If new failures appear, stop and classify them**

Do not guess. Group them by root cause and decide whether they belong in this same repair batch or a follow-up batch.

**Step 4: Commit only if no additional code changes are needed**

If Task 1–3 are sufficient and no new source changes are required, skip this commit.

---

## Verification Checklist

Before claiming this repair batch succeeded, fresh evidence must show:

- `stack test --fast` no longer fails with hidden `Polysemy.KVStore` package errors.
- `stack test --fast` no longer fails on missing `lruCacheSize` in `testConfig`.
- `stack test --fast` no longer fails on obsolete `makeApplication` call shape.
- Any remaining failures are new and explicitly reported.

---

## Notes

- Use `package.yaml` as the source of truth. Let Stack/Hpack regenerate `hs-hath.cabal`.
- Keep the new helper production-safe and small. This is a test-adaptation seam, not a new public abstraction layer.
- Do not widen scope to runtime/server behavior changes unless the updated test output requires it.

---

## Implementation Results

### 2026-03-24 Progress

- Added `hs-hath-polysemy-zoo` to test-suite dependencies in `package.yaml` so `Polysemy.KVStore` is visible to tests.
- Updated `test/FilesystemSpec.hs` and `test/R2Spec.hs` to match the current cache effect type `KVStore FileURI StorageResult` and to provide `Reader ClientConfig` via `runReader defaultClientConfig`.
- Added/used narrow test seam `mkServerLoopContext` in `src/Server.hs` and rewired `test/Integration.hs` to construct `ServerLoopContext` instead of calling the removed many-argument `makeApplication` shape.
- Updated integration fixtures to include `lruCacheSize`.
- Refactored integration helpers so `withTestServer` disables rate limiting by default and `withRateLimitedTestServer` is used only for the explicit rate-limit test.
- Fixed an off-by-one bug in `src/Middleware.hs` by changing the block condition from `requestTimes > maxRequests` to `requestTimes >= maxRequests`. The existing rate-limit integration test exposed this production bug.

### Verification Evidence

Commands run after implementation:

```bash
stack test --fast --ta '--match "/Full Server Lifecycle/applies rate limiting with 429 after bursts/"'
stack test --fast
```

Observed results:

- The targeted rate-limit regression test passed.
- The full test suite passed: `53 examples, 0 failures, 3 pending`.

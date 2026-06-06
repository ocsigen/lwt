# How the Lwt documentation is generated

The Lwt documentation published at <https://ocsigen.org/lwt/> is built entirely
with **odoc** and themed with the Ocsigen site chrome by
[**wodoc**](https://github.com/ocsigen/wodoc) (an odoc driver). The same odoc
sources are also what ocaml.org renders.

## Sources

| What | Where | Format |
|---|---|---|
| Manual | [`docs/manual.mld`](../docs/manual.mld) | odoc page |
| Package landing | [`src/core/index.mld`](../src/core/index.mld) | odoc page |
| API | the `.mli` of every `lwt*` package | odoc comments (native `{!…}` refs) |
| Site configuration (nav, packages, …) | [`doc/wodoc`](wodoc) | wodoc config (S-expression) |

Lwt is a modern dune project with several packages (`lwt`, `lwt_ppx`,
`lwt_react`, `lwt_retry`) but no client/server split, so a single
`dune build @doc` (plain odoc) builds the manual **and** the API of every package
in one run. This replaces the former ocamldoc/wikidoc pipeline. The left
navigation, theme and per-page assembly used to be hand-written HTML and Python
scripts; they are now declared in [`doc/wodoc`](wodoc) and produced by
`wodoc build` (the page template, the version selector, the left navigation and
the cross-package reference resolution are all built in).

## Build

```
wodoc build --config doc/wodoc --label dev --out _doc-site/dev \
  --menu https://raw.githubusercontent.com/ocsigen/ocsigen.github.io/master/wodoc/menu.html
```

That single command (no wrapper script) does everything — `--menu` takes the
shared top menu's canonical copy in `ocsigen.github.io` (fetched for you). It:

1. runs `dune build @doc` — odoc HTML for the manual and the API of every `lwt*`
   package in one run, into `_build/default/_doc/_html/`;
2. assembles every page into the Ocsigen site (shared header/menu/drawer, the
   version `<select>`, the left navigation declared in `doc/wodoc`) and links the
   cross-package references to the sibling packages (`lwt_ppx`, `lwt_react`,
   `lwt_retry`), then ships the version redirect and the `latest` symlink.

Output goes to `<outdir>/<label>/` (default `_doc-site/<label>/`), laid out to
match `https://ocsigen.org/lwt/<label>/`. Internal links are version-relative;
only the version `<select>` is absolute (`/lwt`). The themed stylesheet is served
centrally at `/css/ocsigen-odoc.css` by ocsigen.org.

## Deployment (CI)

[`.github/workflows/doc.yml`](../.github/workflows/doc.yml) builds and publishes
to the project's **`gh-pages`** branch (served at `ocsigen.org/lwt/`):

- **push to `master`** → rebuilds and deploys the **`dev`** docs (`dev/`).
- **manual run** (Actions → *Documentation* → *Run workflow*) → builds any
  version. For a release: set *label* to the version (e.g. `6.0.0`), *ref* to the
  tag, and tick *set_latest* to repoint `latest`. A tag cut **before** this
  migration has no `doc/` infra, so the workflow overlays the (version-independent)
  doc sources from `master`.

Each run replaces only its own `<label>/` directory; the other version
directories already on `gh-pages` are preserved.

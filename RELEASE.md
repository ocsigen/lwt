# How to make a release.


- Bump the relevant version numbers:
  - Oasis
  - CHANGES
- `sh dist.sh`. It creates a tag and a branch numbered by the version.
- Push **only the tag and not the branch**. You obtain a tag with no branch.
- Let github create a tarball.
- Delete the local branch.

- In [ocsigen.org-data](https://github.com/ocsigen/ocsigen.org-data), copy `tyxml/dev` to the new version number.
- Add the new version in the [download page](http://ocsigen.org/tyxml/install).

- Publish on opam.

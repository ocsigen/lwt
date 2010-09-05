include setup.data

.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(pkg_name)-$(pkg_version)

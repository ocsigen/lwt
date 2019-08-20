#! /usr/bin/env python

# Retrieves source code of OPAM packages recursively depending on Lwt into a
# subdirectory ./dependees/, so you can grep through the code.

import os.path
import subprocess

DEPENDEES = "dependees"

def main():
    subprocess.check_call(["opam", "update"])

    packages = subprocess.check_output([
        "opam", "list", "--all", "--depends-on=lwt", "--dev", "--recursive",
        "--short"])

    depopt_packages = subprocess.check_output([
        "opam", "list", "--all", "--depends-on=lwt", "--depopts", "--dev",
        "--short", "--with-test", "--with-doc"])

    packages = packages.strip().split("\n")
    depopt_packages = depopt_packages.strip().split("\n")

    packages = set(packages).union(set(depopt_packages))
    packages = list(packages)
    packages.sort()

    print "Downloading %i packages..." % len(packages)

    subprocess.check_call(["rm", "-rf", DEPENDEES])

    for package in packages:
        directory = os.path.join(DEPENDEES, package)
        remove_command = ["rm", "-rf", directory]

        use_opam_source = False

        info_command = ["opam", "info", "--field=dev-repo:", package]
        try:
            field = subprocess.check_output(info_command)
            no_quotes = field[1:-2]
            if no_quotes[0:9] != "git+https":
                use_opam_source = True
            else:
                repo = no_quotes[4:]
                clone_command = \
                    ["git", "clone", "-q", "--depth", "1", repo, directory]
                print "[%s]" % package, " ".join(clone_command)
                subprocess.check_call(clone_command)
        except:
            use_opam_source = True

        if not use_opam_source:
            continue

        source_command = ["opam", "source", "--dir=" + directory]
        subprocess.check_call(remove_command)
        try:
            subprocess.check_call(source_command + ["--dev-repo", package])
        except subprocess.CalledProcessError as e:
            subprocess.check_call(remove_command)
            try:
                subprocess.check_call(source_command + [package])
            except subprocess.CalledProcessError as e:
                pass

if __name__ == "__main__":
    main()

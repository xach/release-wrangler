"release-wrangler" automates some of the work involved in preparing a
project for release.

It updates versions and other release-related text in the project,
creates a new tag, creates a tarball for it, signs the tarball,
publishes the tarball and documentation to my website, and pushes the
update back upstream.

It's highly customized to my project structure and is not meant to be
useful to anyone else.


The simplest usage is (release project-name new-version-number).

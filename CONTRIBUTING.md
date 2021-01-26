# Contributing

Want to contribute? Really? Awesome! Contributing is always welcome!

Here are some guidelines on how to contribute to Binbo.

Please note we have a [Code of Conduct](CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.

## Performance

Since Binbo is aimed to be used on game servers, you should always think about perfomance.

Your code should be as fast as possible at run-time.

Use bitwise operations where possible.

Use binaries rather than strings (lists) where possible.

## Bug reports

We use GitHub issues to track bugs. Report a bug by opening a new issue.

## Questions and discussions

If you have any question, feel free to start a new discussion on GitHub.

## Feature requests

Feature requests are always welcome. Open a new issue on GitHub and describe what the new feature is.

## Pull Requests

We actively welcome your pull requests.

The `master` branch corresponds to the current release. The `develop` branch is used for development of new features that will end up in the next release. If you fix a bug open a pull request on `master`, if it's a new feature - on `develop`.

**Note**:

* If you have added code, add tests.
* If you have changed API, update [README.adoc](README.adoc) file with details of changes to the interface and examples.
* Ensure the test suite passes on your local computer.
* Ensure the build and test suite passes on travis.

## Coding Style

Use horizontal tabs for indentation. Use one tab per indentation level.

There is no hard limit for line length, just try to keep it as readable as possible.

Write small functions when possible. Use comments.

You should remove whitespace at the end of lines.

You should also refer to the [.editorconfig](.editorconfig) file.

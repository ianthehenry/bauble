# bauble tests

This is a script that writes an HTML file that contains a bunch of rendered images. It shells out to the following external programs which need to be on your `PATH`:

- `shasum`
- `pngcrush`

You can run tests like this:

```
$ jpm -l deps # first time only
$ jpm -l janet suite.janet
$ jpm -l janet gc.janet # before you commit
```

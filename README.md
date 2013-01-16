Parenthesize refactoring, which preserves layout, and should produce compileable code.

To demo:

```bash
cd src
ghci Examples
```

The output in "Examples.output" should be generated.


Based on a significant portion of Roman Cheplyaka's hasfix:
https://github.com/feuerbach/hasfix

It's used to allow code changes while appropriately respecting block layout.  The modules used are in src/HasFix, with minimal changes.


Cabal file is TODO.  I think the following deps are what matter (from hasfix):

    haskell-src-exts,
    syb-with-class >= 0.6.1,
    data-lens,
    data-lens-template,


If errors are encountered in SrcLocUtils.hs, I needed to install the data-lens-template from

git://github.com/ekmett/data-lens-template.git

Because Roman's patch to it was reverted in the roconnor's repo (which is on hackage)


TODO list:

  * Cabal file

  * Turn examples into tests  (make sure the output parses and is the same)

  * Support comments 

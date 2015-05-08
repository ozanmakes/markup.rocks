markup.rocks
============

> *markup.rocks* is a client-side app that lets you **edit**, **preview** and
> **convert** between documents written in various markup languages in your
> browser.

## Open source!
Check out markup.rocks on [github] to view the source code, file issues and
contribute.

markup.rocks wouldn't be possible without these open source projects:

* [GHCJS] - Amazing Haskell to JavaScript compiler
* [Pandoc] - Extraordinary document converter which I shamelessly compiled to
  JS and built an interface around
* [Reflex] and [Reflex-DOM] - Great set of libraries that hold this app
  together with the Functional Reactive Programming constructs they provide.

### Building

The easiest way to get a working GHCJS installation is to use the provided
nix-based build environment.

```bash
$ git clone https://github.com/osener/markup.rocks.env
$ git clone https://github.com/osener/markup.rocks
$ markup.rocks.env/activate  # this will take a while
$ make -C markup.rocks
```

[github]:https://github.com/osener/markup.rocks
[GHCJS]:https://github.com/ghcjs/ghcjs
[Pandoc]:http://pandoc.org/
[Reflex]:https://github.com/ryantrinkle/reflex
[Reflex-DOM]:https://github.com/ryantrinkle/reflex-dom

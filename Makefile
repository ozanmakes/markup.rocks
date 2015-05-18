GZIP=zopfli -c -i1000
# GZIP=gzip -c
FONTS=static/themes/default/assets/fonts

JS=vendor/codemirror/lib/codemirror.js									\
	 vendor/codemirror/addon/mode/overlay.js							\
	 vendor/codemirror/mode/meta.js												\
	 vendor/codemirror/mode/xml/xml.js										\
	 vendor/codemirror/mode/markdown/markdown.js					\
	 vendor/codemirror/mode/gfm/gfm.js										\
	 vendor/codemirror/mode/python/python.js							\
	 vendor/codemirror/mode/stex/stex.js									\
	 vendor/codemirror/mode/rst/rst.js										\
	 vendor/codemirror/mode/javascript/javascript.js			\
	 vendor/codemirror/mode/css/css.js										\
	 vendor/codemirror/mode/textile/textile.js						\
	 vendor/codemirror/mode/htmlmixed/htmlmixed.js				\
	 vendor/codemirror/mode/troff/troff.js								\
	 vendor/codemirror/mode/coffeescript/coffeescript.js	\
	 vendor/codemirror/mode/commonlisp/commonlisp.js			\
	 vendor/codemirror/mode/erlang/erlang.js							\
	 vendor/codemirror/mode/clojure/clojure.js						\
	 vendor/codemirror/mode/go/go.js											\
	 vendor/codemirror/mode/haskell/haskell.js						\
	 vendor/codemirror/mode/lua/lua.js										\
	 vendor/codemirror/mode/perl/perl.js									\
	 vendor/codemirror/mode/r/r.js												\
	 vendor/codemirror/mode/ruby/ruby.js									\
	 vendor/codemirror/mode/shell/shell.js								\
	 vendor/codemirror/mode/scheme/scheme.js							\
	 vendor/codemirror/mode/sql/sql.js										\
	 static/highlight.pack.js															\
	 static/jquery.js																			\
	 static/semantic.min.js																\
	 vendor/FileSaver.min.js															\
	 static/jsbits.js

CSS=vendor/codemirror/lib/codemirror.css	\
		static/neo.css												\
		static/hljs-theme.css									\
		static/semantic.min.css

compile:
	cd src; ghcjs --make Main.hs -DGHCJS_BROWSER -DGHCJS_BUSY_YIELD=5 -DGHCJS_SCHED_QUANTUM=5
	cat $(CSS) static/app.css > static/style.css
	cat $(JS) src/Main.jsexe/all.js > static/app.js

compress:
	mkdir -p dist/$(FONTS) dist/static/icon

	closure-compiler --js static/app.js --js_output_file static/app.min.js -W QUIET

	$(GZIP) index.html > dist/index.html
	$(GZIP) static/style.css > dist/static/style.css
	$(GZIP) static/app.min.js > dist/static/app.js
	for file in static/icon/*; do $(GZIP) "$$file" > "dist/$$file"; done
	for file in $(FONTS)/*; do $(GZIP) "$$file" > "dist/$$file"; done


clean:
	rm -f static/all.min.js
	rm -f static/app.min.js
	rm -f static/style.css
	rm -rf src/Main.jsexe
	rm -rf dist
	find -L src -name "*.js*" -exec rm {} \;

sync:
	aws s3 sync dist/static s3://markup.rocks/static --content-encoding "gzip" --delete --acl "public-read" --cache-control "public, max-age=31536000"
	aws s3 cp dist/index.html s3://markup.rocks/index.html --content-encoding "gzip" --acl "public-read"

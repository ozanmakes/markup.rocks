GZIP=zopfli -c -i1000
# GZIP=gzip -c
FONTS=static/themes/default/assets/fonts

compile:
	cd src; ghcjs --make Main.hs -DGHCJS_BROWSER
	cat vendor/codemirror/lib/codemirror.css	\
			static/neo.css												\
			static/hljs-theme.css									\
			static/semantic.min.css								\
			static/app.css												\
		> static/style.css

	cat vendor/codemirror/lib/codemirror.js							\
			vendor/codemirror/addon/mode/overlay.js					\
			vendor/codemirror/mode/meta.js									\
			vendor/codemirror/mode/xml/xml.js								\
			vendor/codemirror/mode/markdown/markdown.js			\
			vendor/codemirror/mode/gfm/gfm.js								\
			vendor/codemirror/mode/python/python.js					\
			vendor/codemirror/mode/stex/stex.js							\
			vendor/codemirror/mode/rst/rst.js								\
			vendor/codemirror/mode/javascript/javascript.js	\
			vendor/codemirror/mode/css/css.js								\
			vendor/codemirror/mode/textile/textile.js				\
			vendor/codemirror/mode/htmlmixed/htmlmixed.js		\
			static/highlight.pack.js                        \
			static/jquery.js																\
			static/semantic.min.js													\
			static/jsbits.js																\
			src/Main.jsexe/all.js														\
		> static/app.js

compress:
	mkdir -p dist/$(FONTS)
	closure-compiler --js src/Main.jsexe/all.js --js_output_file static/all.min.js
	cat static/jquery.js static/codemirror.js static/semantic.min.js static/jsbits.js static/all.min.js > static/app.js

	$(GZIP) index.html > dist/index.html
	$(GZIP) static/style.css > dist/static/style.css
	$(GZIP) static/app.js > dist/static/app.js

	$(GZIP) $(FONTS)/icons.otf > dist/$(FONTS)/icons.otf
	$(GZIP) $(FONTS)/icons.svg > dist/$(FONTS)/icons.svg
	$(GZIP) $(FONTS)/icons.ttf > dist/$(FONTS)/icons.ttf
	$(GZIP) $(FONTS)/icons.woff > dist/$(FONTS)/icons.woff
	$(GZIP) $(FONTS)/icons.woff2 > dist/$(FONTS)/icons.woff2


clean:
	rm -f static/all.min.js
	rm -f static/app.min.js
	rm -f static/style.css
	rm -rf src/Main.jsexe
	rm -rf dist
	find -L src -name "*.js*" -exec rm {} \;

sync:
	aws s3 sync dist s3://markup.rocks/ --content-encoding "gzip" --delete --acl "public-read"

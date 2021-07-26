SRCS=$(wildcard src/*.ml)
OBJS=$(patsubst src/%.ml,lib/js/src/%.js,$(SRCS))

all: static/index.js

clean:
	rm -rf lib static/index.js

$(OBJS): $(SRCS)
	npm run build

static/index.js: lib/js/src/demo.js src/start.js
	./node_modules/.bin/browserify -o $@ src/start.js

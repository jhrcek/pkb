script=static/script.js

front:
	cd frontend && elm make src/Main.elm --optimize --output=../$(script)

mini: front
	uglifyjs $(script) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$(script)

deploy: mini
	stack install --pedantic

dev: front back

back:
	stack build --fast

clean:
	rm $(script)
	stack clean

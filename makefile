script=static/script.js
script_mini=static/script.min.js

front:
	cd frontend && ./elm make src/Main.elm --output=../$(script)

mini: front
	uglifyjs $(script) --compress --mangle --output $(script_mini)
	mv $(script_mini) $(script)

deploy: mini
	stack install --pedantic

dev: front back

back:
	stack build --fast

clean:
	rm $(script)
	stack clean

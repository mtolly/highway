all: img/bg.png

%.png: %.svg
	inkscape -f "$<" -e "$@"

%: %.haml
	haml "$<" > "$@"

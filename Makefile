todobackend.native: todobackend.ml
	corebuild -pkg opium,cow.syntax todobackend.native

publish: todobackend.native
	ssh 54.72.243.203 rm todobackend.native
	scp todobackend.native 54.72.243.203:

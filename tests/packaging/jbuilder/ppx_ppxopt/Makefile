.PHONY : test
test : clean
	# Expecting failure with "Unbound value >>".
	! jbuilder build user.exe --root .

.PHONY : clean
clean :
	jbuilder clean --root .

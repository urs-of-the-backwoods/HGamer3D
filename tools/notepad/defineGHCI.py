from GHCI import *

# check, if session exists, if not start one

# check, if session exists, if not create one
buffer = Buffer()
if buffer.sessionname() not in console.__dict__:
	console.__dict__[buffer.sessionname()] = GhciSession(console)
	
# get text and convert
text = ghciLet(getSelectionOrLine())

# run command, write to session
console.__dict__[buffer.sessionname()].write(text)
	

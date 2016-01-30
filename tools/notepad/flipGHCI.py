from GHCI import *

# stop if session exists
buffer = Buffer()
if buffer.sessionname() in console.__dict__:
	console.__dict__[buffer.sessionname()].shadow.flipBuffer()
	
	
	

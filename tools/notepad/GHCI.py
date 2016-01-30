# GHCI module for Notepad++ editor
# (c) 2015 Peter Althainz, MIT License
# use at your own risk

from Npp import *
import subprocess, threading, time, Queue, os.path, os

# The command line, which is executed in the interpreter session
processCommandLine = "stack exec ghci"

# Running sessions handler on notepad close
#
runningSessions = {}

def cleanSessions(args):
	snames = runningSessions.keys()
	for s in snames:
		runningSessions[s].stopShort()
	time.sleep(1)

notepad.callback(cleanSessions, [NOTIFICATION.SHUTDOWN])

# Text Utilities
#

# delivers selection, if empty, complete current line
def getSelectionOrLine():
	selStart = editor.getSelectionStart()
	selEnd = editor.getSelectionEnd()
	if selEnd > selStart:
		return editor.getTextRange(selStart, selEnd)
	else:
		return editor.getCurLine()

# convert text to include multiline comment	
def ghciMultiline(text):
	return (":{\n" + text + "\n:}\n")

# convert text to let clause in GHCI
def ghciLet(text):
	return (":{\nlet " + text.replace("\n","\n    ") + "\n:}\n")


# a Buffer remembers the actual open document and stores some parameters about it
# writing to a buffer, activates it, writes to it and then activates the previous buffer
#
class Buffer:
	def __init__(self):
		self.bufferID = notepad.getCurrentBufferID()
		self.filename = notepad.getCurrentFilename()
		self.path = os.path.dirname(self.filename)
		self.basename = os.path.basename(self.filename)
		
	def activate(self):
		notepad.activateBufferID(self.bufferID)
		
	def sessionname(self):
		if self.basename[0:7] == "GHCI - ":
			return self.basename
		else:
			return ("GHCI - " + self.basename)
		

# keeps a pair of buffers, the current one in buffer and a newly created ghciBuffer
# supports flip between them, ...
#
class GhciShadow:
	def __init__(self):
		self.buffer = Buffer()
		notepad.new(self.buffer.sessionname())
		self.ghciBuffer = Buffer()
		self.buffer.activate()
		
	def write(self, message):
		self.ghciBuffer.activate()
		if message[-1:] != "\n":
			message = message + "\n"
		editor.write(message)
		self.buffer.activate()
		
	def flipBuffer(self):
		current = notepad.getCurrentBufferID()
		if  current == self.buffer.bufferID:
			self.ghciBuffer.activate()
		if  current == self.ghciBuffer.bufferID:
			self.buffer.activate()
				
class ReadingThread(threading.Thread):
    def __init__(self, o, q):
        threading.Thread.__init__(self)
        self.o = o
        self.q = q
        self.daemon = False
		
    def run(self):
        runFlag = True
        while runFlag:
            out = self.o.readline()
            self.q.put(out)
            if (out.find("Leaving GHCi.") >= 0):
                runFlag = False

class HandlingThread(threading.Thread):
	def __init__(self, stdin, queue, buffer):
		threading.Thread.__init__(self)
		self._stop = threading.Event()
		self.i = stdin
		self.q = queue
		self.c = buffer
		self.daemon = False
		self.m = None
		self.l = threading.Lock()
		
	def stop(self):
		self._stop.set()
		
	def write(self, m):
		self.l.acquire()
		self.m = m
		self.l.release()
		
	def run(self):
		runFlag = True
		buff = ""
		while runFlag:
			self.l.acquire()
			if self.m != None:
#				self.c.write(self.m)  prevents the shutdown in case of running sessions
				self.i.write(self.m)
				self.m = None
			self.l.release()
			try: out =  self.q.get_nowait()
			except Queue.Empty:
				if buff == "":
					time.sleep(.3)
				else:
					self.c.write(buff)
					buff = ""
			else:
				buff = buff + out
			if self._stop.isSet():
				runFlag = False
			

class GhciSession:

	def __init__(self, console):
		self.shadow = GhciShadow()
		self.queue = Queue.Queue()
		self.proc = subprocess.Popen(processCommandLine, 
							shell=True,
							stdin=subprocess.PIPE,
							stdout=subprocess.PIPE,
							stderr=subprocess.STDOUT,
							bufsize=0,
							cwd=self.shadow.buffer.path
							)
							
		# start reading thread
		self.reader = ReadingThread(self.proc.stdout, self.queue)
		self.reader.start()
		
		# start handling thread
		self.handler = HandlingThread(self.proc.stdin, self.queue, self.shadow)
		self.handler.start()
		
		# write to load basename
#		self.write(":l \"" + self.shadow.buffer.basename + "\"\n")
		
		# add myself to console, to find me later
		console.__dict__[self.shadow.ghciBuffer.basename] = self
		runningSessions[self.shadow.ghciBuffer.basename] = self
		
#		time.sleep(2)
		
	def write(self, inMessage):
		# prevent :q from being processed
		qFlag = False
		lines = inMessage.split("\n")
		for line in lines:
			if line.strip()[0:2] == ":q":
				qFlag = True
				
		if qFlag:
			self.stop()
		else:
			self.handler.write(inMessage)
		
	def stopShort(self):
		self.handler.write("\n\n:q\n")
		self.proc.wait()
		self.handler.stop()
	
	def stop(self):
		self.handler.write("\n\n:q\n")
		self.proc.wait()
		self.handler.stop()
		self.shadow.ghciBuffer.activate()
		notepad.saveAs(self.shadow.buffer.path + os.sep + self.shadow.ghciBuffer.basename)
		notepad.close()
		self.shadow.buffer.activate()
		del console.__dict__[self.shadow.ghciBuffer.basename]
		del runningSessions[self.shadow.ghciBuffer.basename]
	
	
	

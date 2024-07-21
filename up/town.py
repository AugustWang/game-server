import sys
def handle(_string):
    if _string.startswith("combine|"):
        string = "".join( _string[8:].split(","))
        return string

"""waiting for input """
while 1:
    # Recv. Binary Stream as Standard IN
    _stream = sys.stdin.readline()

	if not _stream: break
    # Scheme, Turn into  Formal String
    inString  = _stream.strip(" ")
    # handle String
    outString = handle(inString)
    # send to port as Standart OUT
    sys.stdout.write("%s " % (outString,))
    sys.exit(0)
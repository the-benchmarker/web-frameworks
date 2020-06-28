log = Log::IOBackend.new
log.formatter = Dexter::JSONLogFormatter.proc
Log.dexter.configure(:none, log)

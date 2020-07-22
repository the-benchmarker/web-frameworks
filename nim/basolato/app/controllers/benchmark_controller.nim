import strformat
from strutils import parseInt
# framework
import basolato/controller


type BenchmarkController* = ref object of Controller

proc newBenchmarkController*(request:Request):BenchmarkController =
  return BenchmarkController.newController(request)


proc index*(this:BenchmarkController):Response =
  var header = newHeaders()
  header.set("Content-Type", "text/plain")
  return render("").setHeader(header)

proc show*(this:BenchmarkController, id:string):Response =
  let id = id.parseInt
  return render(&"{id}")

proc store*(this:BenchmarkController):Response =
  var header = newHeaders()
  header.set("Content-Type", "text/plain")
  return render("").setHeader(header)
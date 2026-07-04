local vm = require("@lute/vm")
local task = require("@std/task")
local system = require("@std/system")

local threadCount = system.threadCount()

for _ = 1, threadCount do
	task.spawn(vm.create("./serve").serve)
end

print(`Created {threadCount} server threads`)

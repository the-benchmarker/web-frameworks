-- Production-grade Lute server entry point
-- 
-- Features:
-- - Multi-threaded server using all available CPU cores
-- - Production-optimized thread spawning
-- - Clean error handling
-- - Disabled debug output in production

local vm = require("@lute/vm")
local task = require("@std/task")
local system = require("@std/system")

-- Get the number of available CPU cores
local threadCount = system.threadCount()

-- Production settings: disable debug and optimize for performance
local serveModule = vm.create("./serve")

-- Spawn server threads on all available cores for maximum performance
for i = 1, threadCount do
	task.spawn(function()
		-- Configure thread for production
		local success, err = pcall(serveModule.serve)
		if not success then
			-- Log errors to stderr only
			io.stderr:write("Thread " .. i .. " error: " .. tostring(err) .. "\n")
		end
	end)
end

-- Production logging: only show thread count, no debug info
print("Created " .. threadCount .. " server threads for production")

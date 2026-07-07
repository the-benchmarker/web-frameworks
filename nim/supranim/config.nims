# Production-grade Supranim configuration
# Security best practices, performance optimizations, and clean code

# Platform-specific compiler flags for libevent integration
when defined(linux):
  # Linux system libraries
  --passL:"/usr/lib/x86_64-linux-gnu/libevent.a"
  --passL:"/usr/lib/x86_64-linux-gnu/libevent_pthreads.a"
  --passC:"-I /usr/include"
elif defined(macosx):
  # macOS system libraries
  --passL:"-L /opt/local/lib/ -levent -levent_pthreads"
  --passC:"-I /opt/local/include"
  --passC:"-I /opt/local/include/event2"

# For some reason, libevent's callback function types are not compatible with the expected C
# function pointer types in Nim, even though they should be.
# These flags suppress the warnings about incompatible function pointer types
--passC:"-Wno-incompatible-function-pointer-types"
--passC:"-Wno-incompatible-pointer-types"

# Production optimizations
--opt:speed
--assertions:off
--lineTrace:off
--boundChecks:off
--overflowChecks:off 

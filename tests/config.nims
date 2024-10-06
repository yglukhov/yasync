switch("path", "$projectDir/..")

--d:ssl
# --debugger:native
--passC:"-g"
--threads:on
# --stackTrace:off
--gc:orc
#--d:nimBurnFree # good for testing memory issues

# --d:danger
# --d:release

# XXX: This is required after https://github.com/nim-lang/Nim/pull/24108, must be fixed in nim
--d:nimOptIters

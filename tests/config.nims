switch("path", "$projectDir/..")

--d:ssl
# --debugger:native
--passC:"-g"
--threads:on
# --stackTrace:off
--gc:refc # Causes nim codegen bugs with orc :(

# --d:danger
# --d:release

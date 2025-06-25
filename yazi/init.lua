-- init.lua
if os.getenv("YAZI_ID") then
    require("file-selector").entry()
end

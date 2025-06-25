--- @sync entry
-- main.lua
return {
    entry = function(state)
        local id = os.getenv("YAZI_ID") or ""
        local modes = {
            ["101"] = "Single-file selection",
            ["102"] = "Multi-file selection",
            ["103"] = "Folder selection",
        }

        local mode = modes[id]
        if mode then
            ya.notify {
                title   = "Selection Mode",
                content = mode,
                timeout = 4.0
            }
        end
    end,
}

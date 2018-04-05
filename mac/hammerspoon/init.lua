function activateApp(appName, packageID)
   local app = hs.application.find(packageID)
   if not app then
      hs.alert.show(appName.." is not running :/")
      return
   end

   if not app:activate() then
      hs.alert.show(appName.." not activated :/")
      return
   end
end

function activateEmacs()
   local emacsApp = hs.application.find("org.gnu.Emacs")
   if not emacsApp then
      hs.alert.show("Emacs is not running :/")
      return
   end

   if not emacsApp:activate() then
      hs.alert.show("Emacs not activated :/")
      return
   end
end

function activateXcode()
   local xcodeApp = hs.application.find("com.apple.dt.Xcode")
   if not xcodeApp then
      hs.alert.show("Xcode is not running :/")
      return
   end

   if not xcodeApp:activate() then
      hs.alert.show("Xcode not activated :/")
      return
   end
end

function addEmacsOrgModeTODO()
   activateEmacs()

   output,success = hs.execute("~/homebrew/bin/emacsclient -ne \"(ar/org-add-todo-in-file)\" -s /tmp/emacs*/server")
   if not success then
      hs.alert.show("Emacs did not add TODO")
   end
end

hs.hotkey.bind({"alt"}, "T", addEmacsOrgModeTODO)
hs.hotkey.bind({"alt"}, "E", function() activateApp("org.gnu.Emacs", "Emacs") end)
hs.hotkey.bind({"alt"}, "X", function() activateApp("com.apple.dt.Xcode", "Xcode") end)

local choices = {}
for _, emoji in ipairs(hs.json.decode(io.open("emojis/emojis.json"):read())) do
   table.insert(choices,
                {text=emoji['name'],
                 subText=table.concat(emoji['kwds'], ", "),
                 image=hs.image.imageFromPath("emojis/" .. emoji['id'] .. ".png"),
                 chars=emoji['chars']
   })
end

-- Focus the last used window.
local function focusLastFocused()
    local wf = hs.window.filter
    local lastFocused = wf.defaultCurrentSpace:getWindows(wf.sortByFocusedLast)
    if #lastFocused > 0 then lastFocused[1]:focus() end
end

-- Create the chooser.
-- On selection, copy the emoji and type it into the focused application.
local chooser = hs.chooser.new(function(choice)
    if not choice then focusLastFocused(); return end
    hs.pasteboard.setContents(choice["chars"])
    focusLastFocused()
    hs.eventtap.keyStrokes(hs.pasteboard.getContents())
end)

chooser:searchSubText(true)
chooser:choices(choices)

hs.hotkey.bind({"alt"}, ";", function() chooser:show() end)

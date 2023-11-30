-- Enable repl via /Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs
require("hs.ipc")

require("ar.window")

-- Easier installation of spoons.
hs.loadSpoon("SpoonInstall")

spoon.SpoonInstall.use_syncinstall = true
-- spoon.SpoonInstall:andUse("KSheet")

-- ksheetVisible = false
-- hs.hotkey.bind({"alt"}, "H", function()
--       if ksheetVisible then
--          spoon.KSheet:hide()
--          ksheetVisible = false
--       else
--          spoon.KSheet:show()
--          ksheetVisible = true
--       end
-- end)

-- Spectacle Window Manager Keybindings For Hammerspoon
-- https://github.com/scottwhudson/Lunette
hs.loadSpoon("Lunette")
spoon.Lunette:bindHotkeys({
  leftHalf = {{"cmd", "alt"}, "left"},
  rightHalf = {{"cmd", "alt"}, "right"},
  topHalf = {{"cmd", "alt"}, "up"},
  bottomHalf = {{"cmd", "alt"}, "down"},
  topLeft = {{"alt"}, "Left"},
  topRight = {{"alt"}, "Right"},
  bottomLeft = {{"alt", "shift"}, "Left"},
  bottomRight = {{"alt", "shift"}, "Right"},
  fullScreen = false,
  center = false,
  nextThird = false,
  prevThird = false,
  enlarge = false,
  shrink = false,
  undo = false,
  redo = false,
  nextDisplay = false,
  prevDisplay = false,
})

-- Aliases

-- Easily dump variables to the console.
i = hs.inspect.inspect
inspect = hs.inspect.inspect
d = hs.doc
doc = hs.doc

function split(str, delimiter)
   local result = {}
   for match in (str..delimiter):gmatch("(.-)"..delimiter) do
      table.insert(result, match);
   end
   return result
end

function fuzzyMatch(terms, text)
   if terms == nil or terms == '' then
      return true
   end
   local haystack = text:lower()
   for _, needle in ipairs(split(terms, " ")) do
      hs.printf(needle)
      if not haystack:match(needle:lower()) then
         return false
      end
   end
   return true
end

function activateFirstOf(queries)
   local query = hs.fnutils.find(queries, function(query) return hs.application.find(query["name"]) ~= nil end)
   if not query then
      hs.alert.show("No app found\n "..hs.inspect.inspect(queries))
      return
   end
   local app = hs.application.find(query["name"])
   if not app:activate() then
      hs.alert.show(app["name"].." not activated :/")
      return
   end
end

function emacsExecute(activate, elisp)
   if activate then
      activateFirstOf({
            {
               bundleID="org.gnu.Emacs",
               name="Emacs"
            }
      })
   end

   local socket, found = emacsSocketPath()
   if not found then
      hs.alert.show("Could not get emacs socket path")
      return "", false
   end

   local output,success = hs.execute("/opt/homebrew/bin/emacsclient -ne \""..elisp.."\" -s "..socket)
   if not success then
      hs.alert.show("Emacs did not execute: "..elisp)
      return "", false
   end

   return output, success
end

function emacsSocketPath()
   local output,success = hs.execute("~/local/bin/emacssocket")
   return output, success
end

function addEmacsOrgModeTODO()
   appRequestingEmacs = hs.application.frontmostApplication()
   emacsExecute(false, "(ar/hammerspoon-org-modal-add-todo)")
   activateFirstOf({
         {
            bundleID="org.gnu.Emacs",
            name="Emacs"
         }
   })
end

function searchEmacsBrowserBookmarks()
   appRequestingEmacs = hs.application.frontmostApplication()
   emacsExecute(false, "(os-present-bookmarks)")
   activateFirstOf({
         {
            bundleID="org.gnu.Emacs",
            name="Emacs"
         }
   })
end

function launchEmacsKeybindingI()
   appRequestingEmacs = hs.application.frontmostApplication()
   emacsExecute(false, "(os-present-key-binding-i)")
   activateFirstOf({
         {
            bundleID="org.gnu.Emacs",
            name="Emacs"
         }
   })
end

function launchEmacsKeybindingR()
   appRequestingEmacs = hs.application.frontmostApplication()
   emacsExecute(false, "(ar/modal-key-binding-r)")
   activateFirstOf({
         {
            bundleID="org.gnu.Emacs",
            name="Emacs"
         }
   })
end

function launchEmacsKeybindingV()
   appRequestingEmacs = hs.application.frontmostApplication()
   emacsExecute(false, "(os-present-clipboard-manager)")
   activateFirstOf({
         {
            bundleID="org.gnu.Emacs",
            name="Emacs"
         }
   })
end

function backFromEmacs()
   if appRequestingEmacs == nil then
      hs.alert("Emacs not previously requested")
      return
   end
   if appRequestingEmacs:bundleID() == "org.gnu.Emacs" then
      -- No need to bounce back to Emacs if invoked from Emacs.
      return
   end
   appRequestingEmacs:activate()
   appRequestingEmacs = nil
end

function addEmacsOrgModeGoLink()
   emacsExecute(false, "(ar/org-add-short-link-in-file)")
end

function getEmacsOrgShortLinks()
   output,success = emacsExecute(false, "(ar/org-short-links-json)")
   if not success then
      return nil
   end
   local decoded = hs.json.decode(output)
   return hs.json.decode(decoded)
end

function searchEmacsOrgShortLinks()
   appRequestingEmacs = hs.application.frontmostApplication()
   emacsExecute(false, "(ar/modal-ivy-search-short-links)")
   activateFirstOf({
         {
            bundleID="org.gnu.Emacs",
            name="Emacs"
         }
   })
end

hs.hotkey.bind({"alt"}, "T", addEmacsOrgModeTODO)
hs.hotkey.bind({"alt"}, "W", searchEmacsBrowserBookmarks)
hs.hotkey.bind({"alt"}, "I", launchEmacsKeybindingI)
hs.hotkey.bind({"alt"}, "R", launchEmacsKeybindingR)
hs.hotkey.bind({"alt"}, "V", launchEmacsKeybindingV)
hs.hotkey.bind({"alt"}, "L", searchEmacsOrgShortLinks)

-- hs.hotkey.bind({"alt"}, "D", function() activateFirstOf({
--             {
--                bundleID="com.kapeli.dashdoc",
--                name="Dash"
--             }
-- }) end)

-- hs.hotkey.bind({"alt"}, "E", function() activateFirstOf({
--             {
--                bundleID="org.gnu.Emacs",
--                name="Emacs"
--             }
-- }) end)

-- hs.hotkey.bind({"alt"}, "X", function() activateFirstOf({
--             {
--                bundleID="com.apple.dt.Xcode",
--                name="Xcode"
--             }
-- }) end)

hs.hotkey.bind({"cmd", "ctrl"}, "L", function()
      hs.caffeinate.startScreensaver()
end)

-- hs.hotkey.bind({"cmd", "ctrl"}, "E", function()
--       emacsExecute(false, "(new-frame)")
--       activateFirstOf({
--             {
--                bundleID="org.gnu.Emacs",
--                name="Emacs"
--             }
--       })
-- end)

-- hs.hotkey.bind({"alt"}, "B", function() activateFirstOf({
--             {
--                bundleID="com.google.Chrome",
--                name="Google Chrome"
--             },
--             {
--                bundleID="org.mozilla.firefox",
--                name="Firefox"
--             },
--             {
--                bundleID="com.apple.Safari",
--                name="Safari"
--             },
-- }) end)

-- hs.hotkey.bind({"alt"}, "M", function() activateFirstOf({
--             {
--                bundleID="com.apple.mail",
--                name="Mail"
--             },
--             {
--                bundleID="org.epichrome.app.GoogleMail",
--                name="Google Mail"
--             },
-- }) end)

hs.hotkey.bind({"alt"}, "C", function() activateFirstOf({
            {
               bundleID="com.apple.iCal",
               name="Calendar"
            },
            {
               bundleID="org.epichrome.app.GoogleCalend",
               name="Google Calendar"
            },
}) end)

hs.hotkey.bind({"alt"}, "S", function() activateFirstOf({
            {
               bundleID="com.electron.chat",
               name="Google Chat"
            },
}) end)

-- Window management

hs.window.animationDuration = 0

hs.grid.setMargins("0x0")

hs.grid.setGrid("4x2")

hs.hotkey.bind({"alt"}, "G", hs.grid.show)

function reframeFocusedWindow()
   local win = hs.window.focusedWindow()
   local maximizedFrame = win:screen():frame()
   maximizedFrame.x = maximizedFrame.x + 15
   maximizedFrame.y = maximizedFrame.y + 15
   maximizedFrame.w = maximizedFrame.w - 30
   maximizedFrame.h = maximizedFrame.h - 30

   local leftFrame = win:screen():frame()
   leftFrame.x = leftFrame.x + 15
   leftFrame.y = leftFrame.y + 15
   leftFrame.w = leftFrame.w / 2 - 15
   leftFrame.h = leftFrame.h - 30

   local rightFrame = win:screen():frame()
   rightFrame.x = rightFrame.w / 2
   rightFrame.y = rightFrame.y + 15
   rightFrame.w = rightFrame.w / 2 - 15
   rightFrame.h = rightFrame.h - 30

   if win:frame() == maximizedFrame then
     win:setFrame(leftFrame)
     return
   end

   if win:frame() == leftFrame then
     win:setFrame(rightFrame)
     return
   end

   win:setFrame(maximizedFrame)
end

hs.hotkey.bind({"alt"}, "F", reframeFocusedWindow)

function readFile(file)
   local f = assert(io.open(file, "rb"))
   local content = f:read("*all")
   f:close()
   return content
end


function findfunction(x)
   assert(type(x) == "string")
   local f=_G
   for v in x:gmatch("[^%.]+") do
      if type(f) ~= "table" then
         return nil, "looking for '"..v.."' expected table, not "..type(f)
      end
      f=f[v]
   end
   if type(f) == "function" then
      return f
   else
      return nil, "expected function, not "..type(f)
   end
end

function getModuleByName(name)
   return hs.fnutils.find(doc._jsonForModules, function(module)
                             return module['name'] == name
   end)
end

-- Given  "hs.window.desktop("
-- We get "hs.window.desktop() -> hs.window object"
function signatureFromQualifiedName(qualifiedName)
   -- hs.grid.show( -> hs.grid
   -- hs.grid.show -> hs.grid
   local moduleName = string.match(qualifiedName, "(.*)[.]")

   -- hs.grid.show(-> show
   -- hs.grid.show -> show
   local name = string.match(qualifiedName, "[.]([a-zA-Z]*)[(]?$")

   local module = getModuleByName(moduleName)
   if not module then
      return nil
   end

   local constant = hs.fnutils.find(module['Constant'], function(f)
                                       return f['name'] == name
   end)
   if constant then
      return constant['signature']
   end

   local constructor = hs.fnutils.find(module['Constructor'], function(f)
                                          return f['name'] == name
   end)
   if constructor then
      return constructor['signature']
   end

   local method = hs.fnutils.find(module['Method'], function(f)
                                     return f['name'] == name
   end)
   if method then
      return method['signature']
   end

   local variable = hs.fnutils.find(module['Variable'], function(f)
                                       return f['name'] == name
   end)
   if variable then
      return variable['signature']
   end

   local phunction = hs.fnutils.find(module['Function'], function(f)
                                        return f['name'] == name
   end)
   if phunction then
      return phunction['signature']
   end

   return nil
end

function signatureCompletionForText(text)
   local completions = hs.completionsForInputString(text)
   return hs.fnutils.imap(completions, function(fallback)
                             local signature = signatureFromQualifiedName(fallback)
                             if signature then
                                return signature
                             end

                             return fallback
   end)
end

hs.hotkey.bind({"alt"}, "N", ar.window.focusNext)
hs.hotkey.bind({"alt"}, "P", ar.window.focusPrevious)

--
-- ace-window style focused-window switcher.
--
hs.hints.hintChars = {'a','s','d','f','g','h','j','k','l'}
hs.hotkey.bind({"alt"}, "J", hs.hints.windowHints)

-- This must be the last line.
-- hs.notify.new({title="Hammerspoon", informativeText="Reloaded"}):send()
spoon.SpoonInstall:andUse("FadeLogo",
                          {
                             config = {
                                default_run = 1.0,
                             },
                             start = true
})

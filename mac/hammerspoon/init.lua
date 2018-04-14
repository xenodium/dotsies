-- Enable repl via /Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs
require("hs.ipc")

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
   local query = hs.fnutils.find(queries, function(query) return hs.application.find(query["bundleID"]) ~= nil end)
   if not query then
      hs.alert.show("No app found\n "..hs.inspect.inspect(queries))
      return
   end

   local app = hs.application.find(query["bundleID"])
   if not app:activate() then
      hs.alert.show(app["bundleID"].." not activated :/")
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

   output,success = hs.execute("~/homebrew/bin/emacsclient -ne \""..elisp.."\" -s /tmp/emacs*/server")
   if not success then
      hs.alert.show("Emacs did not execute: "..elisp)
   end

   return output, success
end

function addEmacsOrgModeTODO()
   emacsExecute(true, "(ar/org-add-todo-in-file)")
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
   local chooser = hs.chooser.new(function(choice)
         if not choice then
            focusPreviousWindow()
            return
         end
         output,success = hs.execute("open http://"..choice['text'])
         if not success then
            hs.alert.show("Could not open: "..choice['text'])
         end
   end)

   local links = hs.fnutils.map(getEmacsOrgShortLinks(), function(item)
                                   return {
                                      text=item['link'],
                                      subText=item['description']
                                   }
   end)
   chooser:queryChangedCallback(function(query)
         chooser:choices(hs.fnutils.filter(links, function(item)
                                              hs.printf(item["text"])
                                              -- Concat text and subText to search in either
                                              return fuzzyMatch(query, item["text"]..item["subText"]) end))
   end)

   chooser:choices(links)
   chooser:show()
end

hs.hotkey.bind({"alt"}, "T", addEmacsOrgModeTODO)
hs.hotkey.bind({"alt"}, "L", searchEmacsOrgShortLinks)

hs.hotkey.bind({"alt"}, "D", function() activateFirstOf({
            {
               bundleID="com.kapeli.dashdoc",
               name="Dash"
            }
}) end)

hs.hotkey.bind({"alt"}, "E", function() activateFirstOf({
            {
               bundleID="org.gnu.Emacs",
               name="Emacs"
            }
}) end)

hs.hotkey.bind({"alt"}, "X", function() activateFirstOf({
            {
               bundleID="com.apple.dt.Xcode",
               name="Xcode"
            }
}) end)

hs.hotkey.bind({"alt"}, "B", function() activateFirstOf({
            {
               bundleID="org.mozilla.firefox",
               name="Firefox"
            },
            {
               bundleID="com.apple.Safari",
               name="Safari"
            },
            {
               bundleID="com.google.Chrome",
               name="Google Chrome"
            },
}) end)

hs.hotkey.bind({"alt"}, "M", function() activateFirstOf({
            {
               bundleID="com.apple.mail",
               name="Mail"
            },
            {
               bundleID="org.epichrome.app.GoogleMail",
               name="Google Mail"
            },
}) end)

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

hs.hotkey.bind({"alt"}, "F", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen():frame()

      f.x = screen.x + 15
      f.y = screen.y + 15
      f.w = screen.w - 30
      f.h = screen.h - 30

      win:setFrame(f)
end)

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

function circularNext(items, from)
   local len = #items

   if len == 0  then
      return nil
   end

   if from < 1 and from > len then
      return nil
   end

   if from < len then
      return items[from + 1]
   end

   return items[1]
end

function circularPrevious(items, from)
   local len = #items

   if len == 0  then
      return nil
   end

   if from < 1 and from > len then
      return nil
   end

   if from > 1 and from <= len then
      return items[from -1]
   end

   return items[len]
end

function newestWindows()
   return hs.fnutils.filter(hs.window.filter.defaultCurrentSpace:getWindows(hs.window.filter.sortByCreatedLast),
                            function(item)
                               return hs.fnutils.contains(hs.window.allWindows(), item)
   end)
end

function focusNextWindow()
   hs.window.focus(circularNext(newestWindows(),
                                hs.fnutils.indexOf(newestWindows(),
                                                   hs.window.focusedWindow())))
end

function focusPreviousWindow()
   hs.window.focus(circularPrevious(newestWindows(),
                                    hs.fnutils.indexOf(newestWindows(),
                                                       hs.window.focusedWindow())))
end

hs.hotkey.bind({"alt"}, "N", focusNextWindow)
hs.hotkey.bind({"alt"}, "P", focusPreviousWindow)

-- This must be the last line.
hs.notify.new({title="Hammerspoon", informativeText="Reloaded"}):send()

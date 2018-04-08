-- Enable repl via /Applications/Hammerspoon.app/Contents/Resources/extensions/hs/ipc/bin/hs blah
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
      activateApp("org.gnu.Emacs", "Emacs")
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
               bundleID="org.epichrome.app.GoogleCalen",
               name="Google Calendar"
            },
}) end)

-- Window management

hs.window.animationDuration = 0

hs.hotkey.bind({"alt"}, "G", hs.grid.show)

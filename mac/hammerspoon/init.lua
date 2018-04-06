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

function emacsExecute(activate, elisp)
   if activate then
      activateEmacs()
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
hs.hotkey.bind({"alt"}, "E", function() activateApp("org.gnu.Emacs", "Emacs") end)
hs.hotkey.bind({"alt"}, "X", function() activateApp("com.apple.dt.Xcode", "Xcode") end)

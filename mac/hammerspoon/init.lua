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

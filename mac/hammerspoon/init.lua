hs.hotkey.bind({"alt"}, "T", function()
      local emacsApp = hs.application.find("org.gnu.Emacs")
      if not emacsApp then
         hs.alert.show("Emacs is not running :/")
         return
      end

      if not emacsApp:activate() then
         hs.alert.show("Emacs not activated :/")
         return
      end

      output,success = hs.execute("~/homebrew/bin/emacsclient -ne \"(ar/org-add-todo-in-file)\" -s /tmp/emacs*/server")
      if not success then
         hs.alert.show("Emacs did not add TODO")
      end
end)

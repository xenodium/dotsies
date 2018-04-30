local window = {}

if ar == nil then
   ar = {}
end
ar.window = window

local function circularNext(items, from)
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

local function circularPrevious(items, from)
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

function window.lastCreatedWindows()
   return hs.window.filter.defaultCurrentSpace:getWindows(hs.window.filter.sortByCreatedLast)
end

function window.focusNext()
   local windows = window.lastCreatedWindows()
   circularNext(windows,
                hs.fnutils.indexOf(windows,
                                   window.focusedWindow())):focus()
end

function window.focusPrevious()
   local windows = window.lastCreatedWindows()
   circularPrevious(windows,
                    hs.fnutils.indexOf(windows,
                                       window.focusedWindow())):focus()
end

function window.focusedWindow()
   if hs.fnutils.contains(ar.window.lastCreatedWindows(),
                          hs.window.focusedWindow()) then
      return hs.window.focusedWindow()
   end
   -- No draggable focused window. Pick the first draggable one.
   return ar.window.lastCreatedWindows()[1]
end

return window

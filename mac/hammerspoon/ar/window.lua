local window = {}

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
   hs.window.focus(circularNext(window.lastCreatedWindows(),
                                hs.fnutils.indexOf(window.lastCreatedWindows(),
                                                   hs.window.focusedWindow())))
end

function window.focusPrevious()
   hs.window.focus(circularPrevious(window.lastCreatedWindows(),
                                    hs.fnutils.indexOf(window.lastCreatedWindows(),
                                                       hs.window.focusedWindow())))
end

return window

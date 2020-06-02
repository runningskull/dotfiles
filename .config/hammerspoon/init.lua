--------------------------------------------------------------------------------
-- General Utils

function _assign(target, src)
  for k,v in pairs(src) do target[k] = v end
end

function assign(target, ...)
  for i = 1, select('#', ...) do _assign(target, select(i, ...)) end
  return target
end

function alert(msg)
  hs.alert.show(msg)
end

function err(msg)
  hs.alert.show(' ✗ '..msg, {fillColor={red=0.618}})
end

function prompt(main, sub)
  local win = curwin()
  hs.focus()
  local _,input = hs.dialog.textPrompt(main, (sub or ''))
  win:focus()
  return input
end

fmt = string.format
curwin = hs.window.focusedWindow
copy = hs.pasteboard.writeObjects
F = hs.fnutils


--------------------------------------------------------------------------------
-- Window Switching

named_windows = hs.settings.get('named_windows') or {}
hs.settings.clear('named_windows')

function name_window()
  local win, name = curwin(), nil
  local appid = win:application():pid()
  local winwatch, appwatch
  while true do
    name = prompt('Window Name:')
    if name:len() == 0 then return end
    local existing = F.find(named_windows, function(nw) return nw.text == name end)
    if existing then err('There is already a window named '..name) 
    else break end
  end
  local del = function()
    for i,nw in ipairs(named_windows) do
      if nw.text == name then table.remove(named_windows, i) end
    end
    winwatch:stop() ; appwatch:stop()
    winwatch = nil  ; appwatch = nil
  end
  appwatch = hs.application.watcher.new(function(_, evt, app)
    if (evt == hs.application.watcher.terminated) and (app:pid() == appid) then del() end
  end)
  winwatch = win:newWatcher(del)
  appwatch:start()
  winwatch:start{hs.uielement.watcher.elementDestroyed}
  table.insert(named_windows, {text=name, winid=win:id()})
end

function switch_to_named_window()
  hs.chooser.new(function(choice)
    local winid = curwin():id()
    for i,nw in ipairs(named_windows) do
      if nw.winid == winid then
        table.remove(named_windows, i)
        table.insert(named_windows, 1, nw)
        break
      end
    end
    if not choice then return end
    local win = hs.window.get(choice.winid)
    if win then win:focus() end
  end):choices(named_windows):show()
end

function rename_window()
  hs.chooser.new(function(choice)
    if not choice then return end
    local nw = F.find(named_windows, function(w) return w.text == choice.text end)
    for i,nw in ipairs(named_windows) do
      if nw.text == choice.text then
        local new_name = prompt('Rename Window ['..choice.text..'] to:')
        if new_name:len() > 0 then nw.text = new_name
        else table.remove(named_windows, i) end
      end
    end
  end):choices(named_windows):show()
end

hs.hotkey.bind({'alt', 'shift'}, 'space', name_window)
hs.hotkey.bind({'cmd', 'shift'}, 'space', switch_to_named_window)
hs.hotkey.bind({'alt', 'shift'}, 'delete', rename_window)


--------------------------------------------------------------------------------
-- Main Chooser

cmds = {

  {'show_winframe', 'Show Window Frame (+copy)',
    function()
      local f = curwin():frame()
      hs.alert(fmt('%i×%i @ %i,%i', f.w,f.h, f.x,f.y), nil, nil, 4)
      copy(fmt('{w=%i, h=%i, x=%i, y=%i}', f.w,f.h, f.x,f.y))
    end
  },

  {'vimr_make_helper', '[VimR] Make Helper Window',
    function()
      curwin():setSize{w=891, h=1246, x=-2093, y=47}
    end
  },

  {'vimr_make_main', '[VimR] Make Main Window',
    function()

      curwin():setSize{w=2018, h=1365, x=177, y=46}
      -- hs.eventtap.keyStroke({'cmd'}, '1')
      -- hs.eventtap.keyStroke({'cmd'}, '.')
    end
  },

  {'vimr_make_repl', '[VimR] Make REPL Window',
    function()
      curwin():setSize{w=1410, h=1072, x=635, y=191}
    end
  },

  {'term_make_irc', '[Term] Make IRC',
    function()
      curwin():setSize{w=1200, h=1053, x=3020, y=23}
    end
  },

  {'resize_right_side_to_mouse', 'Resize Right Side to Mouse Position',
    function()
      local m = hs.mouse.getAbsolutePosition()
      m.x=math.floor(m.x) ; m.y=math.floor(m.y)
      local w = curwin()
      local f = w:frame()
      local new_width = m.x - f.x
      w:setSize(assign(f, {w=new_width}))
    end
  },

  {'name_window', '[win] Name Window (alt-shift-space)', name_window},
  {'switch_to_named_window', '[win] Focus Window (cmd-shift-space)', switch_to_named_window},
  {'rename_window', '[win] Rename Window (alt-shift-delete)', rename_window},

}


-- keep cmds by name because chooser cannot store functions directly
cmd_lookup = {}
for _,cmd in ipairs(cmds) do
  local name,desc,fn = table.unpack(cmd)
  cmd_lookup[name] = fn
end

cmd_choices = {}
for _,cmd in ipairs(cmds) do
  local name,desc,fn = table.unpack(cmd)
  table.insert(cmd_choices, {text=desc, cmd=name})
end

cmd_chooser = hs.chooser.new(function(choice)
  if not choice then return end
  cmd_lookup[choice.cmd]()
end)
cmd_chooser:choices(cmd_choices)
hs.hotkey.bind({}, 'f19', function() cmd_chooser:show() end)
hs.hotkey.bind({'cmd'}, 'pad+', function() cmd_chooser:show() end)


--------------------------------------------------------------------------------


-- docs = hs.json.read('~/.config/hammerspoon/docs_index.json')

-- doc_choices = {}
-- for _,doc in ipairs(docs) do
--   local module = (doc.module or '') .. '.'
--   table.insert(doc_choices, {
--     text = module .. doc.name .. '  (' .. doc.type .. ')',
--     subText = doc.desc,
--     module = doc.module,
--     name = doc.name
--   })
-- end

-- doc_chooser = hs.chooser.new(function(choice)
--   if not choice then return end
--   local url = 'https://www.hammerspoon.org/docs/' .. choice.module .. '#' .. choice.name
--   print(url)
-- end)
-- doc_chooser:choices(doc_choices)
-- hs.hotkey.bind({}, 'f16', function() doc_chooser:show() end)


--------------------------------------------------------------------------------
function reload_config()
  hs.settings.set('named_windows', named_windows)
  hs.reload()
end
hs.hotkey.bind({'cmd'}, 'f19', reload_config)
hs.hotkey.bind({'cmd', 'shift'}, 'pad+', reload_config)
--------------------------------------------------------------------------------
hs.alert.show('Config loaded')
--------------------------------------------------------------------------------
-- vim: set et sw=2 fdm=indent :

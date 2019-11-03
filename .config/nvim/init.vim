let g:manual_color=1
set termguicolors

if has("nvim")
  set runtimepath^=~/.vim runtimepath+=~/.vim/after
  let &packpath = &runtimepath
  source ~/.vimrc

  if !has("gui_vimr")
    call Colors_Dark()
  endif
endif

if has("gui_vimr")
  call Colors_Light()
endif


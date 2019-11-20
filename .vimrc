" vim: set fdm=indent sw=2 et :

set nocompatible
let mapleader = "," 
filetype plugin on


"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Plugins

  call plug#begin('~/.vim/plugged')

  " rougly in order of most life-changing
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'justinmk/vim-dirvish'
    Plug 'tpope/vim-vinegar'
    Plug 'tpope/vim-commentary'
    Plug 'rking/ag.vim'
    Plug 'tpope/vim-dispatch'
    Plug 'liuchengxu/vim-which-key'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-obsession'
    Plug 'ciaranm/detectindent'
    Plug 'tpope/vim-eunuch'

  " language runtimes
    Plug 'tpope/vim-markdown'
    Plug 'guns/vim-clojure-static'
    Plug 'ingydotnet/yaml-vim'
    Plug 'vim-scripts/glsl.vim'
    Plug 'pangloss/vim-javascript'
    Plug 'hail2u/vim-css3-syntax'
    Plug 'othree/html5.vim'
    " Plug 'jonsmithers/vim-html-template-literals'
    " Plug 'Quramy/vim-js-pretty-template'
    " Plug 'runningskull/vim-mustache-handlebars'
    " Plug 'keith/swift.vim'
    " Plug 'chr4/nginx.vim'
    " Plug 'leafgarland/typescript-vim'
    " Plug 'tbastos/vim-lua'
    Plug 'artoj/qmake-syntax-vim'

  " colors
    Plug 'runningskull/monoid.vim'

  " test-driving
    Plug 'junegunn/vim-peekaboo'
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'neoclide/coc.nvim', {'branch': 'release'}
    " Plug 'kassio/neoterm'
    " Plug 'kergoth/vim-hilinks'

  call plug#end()




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Vim Core

  " fix for delay exiting insert mode in terminal
  set timeoutlen=500 ttimeoutlen=0

  " swap files in their own folder
  set directory=~/.backup//,/tmp//
  set undodir=~/.undo//,/tmp//

  " undo, etc.
  set undofile
  set undolevels=200
  set undoreload=500

  " store lots of history (default is 20)
  set history=500

  " allow vim to hide buffers w/o saving
  set hidden

  " allow folding single-line folds
  set fml=0

  " use system clipboard
  set clipboard=unnamed

  " make backspace delete everything
  set bs=2




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" UI Options

  set number
  set foldcolumn=0
  set nowrap
  set cursorline

  " mouse in the terminal
  set mouse=a
  if has('mouse_sgr')
    set ttymouse=sgr
  endif

  " no beeping
  set noeb
  set novb

  " we don't need no steeenking scrollbars
  set guioptions-=r
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L

  " make macros/mappings execute faster
  set lazyredraw

  " fugitive status appears in preview window
  set previewheight=25

  " window layout
  set noequalalways

  " status line
  set statusline=\ \%F%m%r%h%w\ \(%L\)\ \|\ %l.%v
  set laststatus=2

  " buffer space top/bottom
  set scrolloff=8

  " set cursor shapes
  " just collecting every incantation i've needed
  let &t_ti.="\e[1 q"
  let &t_SI.="\e[5 q"
  let &t_EI.="\e[1 q"
  let &t_te.="\e[0 q"
  set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20
  if exists('$TMUX')
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif 
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Util Functions

  function! CurChar(...)
    let offset = 1 - (a:0 > 0 ? a:1 : 0)
    return strcharpart(strpart(getline('.'), col('.') - offset), 0, 1) 
  endfunction

  function! CurSyntax(...)
    let id = synID(line('.'), col('.'), 1)
    let id = a:0 ? id : synIDtrans(id)
    return synIDattr(id, 'name')
  endfunction

  function! CurInWord()
    return (CurChar(-1) =~ '\w')
  endfunction

  function! CurInComment()
    return CurSyntax() =~ 'Comment'
  endfunction

  function! CurInString()
    return CurSyntax() =~ 'String'
  endfunction

  function! CurIsAfter(char)
    return (CurChar(-1) =~ a:char)
  endfunction

  function! CurIsBefore(char)
    return (CurChar() =~ a:char)
  endfunction

  function! SetKey(dict, path, val)
    let keys = split(a:path, '\.')
    let final_key = keys[-1]
    let level = a:dict
    if len(keys) > 1
      let keys = keys[:-2]
      for k in keys
        if !has_key(level, k) | let level[k] = {} | endif
        if type(level[k]) == v:t_dict
          let level = level[k]
        else
          echoe "Could not set path ".a:path." : Key '".k."' is not a dictionary"
          return
        endif
      endfor
    endif
    let level[final_key] = a:val
  endfunction

  function! GUI()
    return ( has('gui_vimr') || has('gui_running') )
  endfunction

  function! TitleCase(s)
    return substitute(a:s, '\<\w', '\U\0', 'g')
  endfunction

  function! OrStr(a, b)
    return len(a:a) ? a:a : a:b
  endfunction




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Colors

  syn on
  set t_Co=256

  " easy switch light/dark
  function! Colors_Light()
    set bg=light
    " colo mnml-light
    " colo one
    colo monoid
  endfunction

  function! Colors_Dark()
    set bg=dark
    colo smyck
  endfunction

  command! Dark call Colors_Dark() | let g:manual_color=1
  command! Light call Colors_Light() | let g:manual_color=1

  if !exists('*Colors_Default')
    function! Colors_Default()
      call call('Colors_' . (GUI() ? 'Light' : 'Dark'), [])
    endfunction
  endif

  " startup color theme
  function! Colors_Init()
    if exists('g:manual_color') | return | endif
    if GUI() | return Colors_Default() | endif

    let term = readfile(expand('~/.config/termcolor'))
    call call('Colors_' . (len(term) ? TitleCase(term[0]) : 'Defaults'), [])
  endfunction
  
  call Colors_Init()

  if has('nvim')
    au VimResume * call Colors_Init()
  endif




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Which-Key

  let g:which_key_map = {}
  call which_key#register(',', 'g:which_key_map')
  command! -nargs=+ WK call SetKey(g:which_key_map, <f-args>)
  nnoremap <silent> <leader> :WhichKey ','<CR>
  hi link WhichKeyDesc Identifier
  hi link WhichKeyGroup Label




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" In-buffer Editing

  " quick hop up and down (visual and normal)
  noremap <c-k> 8k
  noremap <c-j> 8j

  " quick comment (can't be noremap's)
  nmap <leader>; gcl
  vmap <leader>; gc
  WK ; comment

  " quick save
  noremap <silent> <leader>s :update<cr>
  WK s save

  " more ergonomic marks
  noremap ' `
  noremap ` '

  "cycle visual -> visual-line -> normal
  vnoremap v V

  " paste without yanking
  vnoremap p "_dP

  " default indenting
  set autoindent
  set expandtab
  set sts=2
  set ts=2
  set sw=2

  " searching is a fast way to navigate
  set ignorecase
  set smartcase
  set incsearch
  set nohls

  " usually I don't actually want the window center
  noremap zz zz10<c-e>

  " highlight matching angle brackets
  set matchpairs+=<:>

  " detect tabs or spaces
  set smarttab

  " operate on the current line
  xnoremap il g_o_o
  xnoremap al $o_o
  onoremap il :normal vil<CR>
  onoremap al :normal val<CR>

  " force tabs instead of spaces
  command! TABS set noexpandtab|set sts=0|set ts=4|set sw=0

  " force tab size
  noremap <leader>2 :set shiftwidth=2<cr>
  noremap <leader>4 :set shiftwidth=4<cr>
  WK 2 which_key_ignore
  WK 4 which_key_ignore

  " less chording for snake_case
  function! SwapDashUnderscore()
    inoremap <buffer> <expr> - (!CurInWord() ? '-' : (CurInComment() ? '-' : '_'))
    inoremap <buffer> <expr> _ (!CurInWord() ? '_' : (CurIsAfter('_') ? '_' : '-'))
  endfunction




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Folding

  " quick fold/unfold
  nnoremap <expr> <leader><space> 'z' . (foldclosed('.') > -1 ? 'A' : 'a')
  WK SPC un/fold

  " open folds by default
  set foldlevel=99

  " indent folding is the kiss solution
  set foldmethod=indent

  " whatever
  set foldnestmax=12

  WK f.name +fold

  " fold/unfold one level
  nnoremap <leader>f<leader> za
  WK f., toggle-one-level

  " set foldlevel to a particular level
  nnoremap <leader>fs :setl foldlevel=
  WK f.s set-foldlevel

  " jump to the next/previous fold and toggle it
  nnoremap <leader>fj zjzA
  nnoremap <leader>fk zkzA
  WK f.j toggle-next
  WK f.k toggle-prev

  " no fillchars
  set fillchars=fold:\ 

  " open folds by clicking them
  function! OpenClickedFold()
    if (foldclosed('.') > -1)
      normal za
    endif
  endfunction
  " nnoremap <silent> <LeftRelease> :call OpenClickedFold()<cr>
  nnoremap <silent> <2-LeftMouse> :call OpenClickedFold()<CR>

  " nicer foldtext function
  set foldtext=MyFoldText()
  function! MyFoldText()
    let ind = indent(v:foldstart - 1)
    let info = v:foldend - v:foldstart + 1
    return repeat(" ", ind) . "┆ » " . info . " « ┆"
  endfunction




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" File/Buffer/Window Navigation

  " quick jump windows
  noremap <c-h> <c-w>h
  noremap <c-l> <c-w>l
  noremap <leader>j <c-w>j
  noremap <leader>k <c-w>k
  WK j which_key_ignore
  WK k which_key_ignore

  " quick switch between 2 buffers
  nnoremap <silent> <leader>z :b#<cr>
  WK z mru-buffer

  " quick quit window
  nnoremap <silent> <leader>q :q<cr>
  WK q kill-window

  " quick browse files
  nnoremap - :Dirvish %<cr>

  " quick project-level search
  nnoremap <leader>/ :Ag!<space>
  WK / search-project

  " quick suspend
  nnoremap Z <c-z>

  " open a scratch buffer
  nnoremap <leader>ee :e /tmp/scratch-<c-r>=strftime("%Y%m%d%H%M")<cr><cr>i
  WK e.name +edit
  WK e.e scratch-buffer

  " ctrl-p is all that and a bag of chips
  nnoremap \ :CtrlPMRU<cr>
  noremap <c-\> :CtrlP<cr>
  let g:ctrlp_by_filename = 0
  let g:ctrlp_custom_ignore = {'dir':  'build$\|dist$\|tmp$\|node_modules$'}
  let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
  let g:ctrlp_prompt_mappings = {'PrtClearCache()': ['<c-r>']}

  " jump to specific buffer (obsoleted by ctrlp plugin)
  " noremap <c-\> :ls<cr>:b<space>

  " sane command-mode autocomplete
  set wildmode=list:longest

  " ignore certain files in autocomplete
  set wildignore+=*.png,*.gif,*.jpg,*.psd
  set wildignore+=*.pyc,.git
  set wildignore+=*.swp,.DS_Store
  set wildignore+=*.scssc
  set wildignore+=*/node_modules/*
  set wildignore+=*/bower_components/*
  set wildignore+=*/build/*

  " window commands w/o chording
  nnoremap <leader>w <c-w>
  WK w which_key_ignore

  " navigate quickfix (error) list
  noremap <leader>co :Copen<cr>
  noremap <leader>cO :copen<cr>
  noremap <leader>cn :cn<cr>
  noremap <leader>cp :cp<cr>
  noremap <leader>cc :cc
  WK c.name +quickfix
  WK c.o open
  WK c.O open-latest
  WK c.n next
  WK c.p prev
  WK c.c goto

  " close buffer w/o closing window
  noremap <leader>bd :b#\|bd \#<cr>
  noremap <leader>bz :tabe %<cr>
  WK b.name +buffer
  WK b.d kill
  WK b.z zoom




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Misc Helpers

  " show help for word under cursor
  nnoremap <leader>hw :h <c-r><c-w><CR>
  WK h.name +help
  WK h.w help-word

  " easy/powerful autocomplete from all buffers
  function! TabComplete(key)
    if (pumvisible()) | return a:key | endif
    return ((col('.') > 1) && (CurInWord())) ? a:key : "\<tab>"
  endfunction
  inoremap <expr>   <tab> TabComplete("\<c-n>")
  inoremap <expr> <s-tab> TabComplete("\<c-p>")
  set completeopt=menuone
  
  " show syntax/highlight help for character under cursor
  function! HiTrace()
    let rootID = synIDtrans(synID(line('.'),col('.'), 1))
    let out  =   ' '  . synIDattr(           synID(line('.'),col('.'), 0) ,'name')
    let out .= ' > '  . synIDattr(           rootID,                       'name')
    let out .= "  :  " . OrStr(synIDattr(rootID, 'fg'), 'x')
    let out .= "|"  . OrStr(synIDattr(rootID, 'bg'), 'x')
    return out
  endfunction
  nnoremap <leader>hs :echo HiTrace()<CR>
  WK h.name +help
  WK h.s show-syntax

  " sessions
  nnoremap <leader>os :Obsession ~/.vim/sessions/
  nnoremap <leader>oo :source ~/.vim/sessions/
  WK o.name +sessions
  WK o.s save
  WK o.o open

  " soft wrap is useful for for editing prose
  function! SoftWrap()
    setl wrap
    setl formatoptions=l
    setl linebreak
    map <buffer> j gj
    map <buffer> k gk
  endfunction
  command! SoftWrap call SoftWrap()

  " get path to current dir/file easily
  cabbr <expr> %d expand('%:p:h')
  cabbr <expr> %f expand('%:p')

  " run things as vim commands
  vnoremap <leader>vx "yy:@y<cr>
  nmap <leader>vx my_v$h,vx'y
  WK v.name +vimscript
  WK v.x execute

  " reload things
  noremap <leader>rr :syntax sync fromstart<cr> 
  noremap <leader>rv :so ~/.vimrc<cr>:echo "reloaded"<cr>
  noremap <leader>rg :call GitVimrc_Load()<cr>
  noremap <leader>rf :so %<cr>
  noremap <leader>rc :call Colors_MatchTerminal()<cr>
  noremap <leader>rl kw<cr>:!osascript ~/bin/reload_chcan<cr><cr>
  WK r.name +reload
  WK r.r syntax
  WK r.v vimrc-main
  WK r.g vimrc-git
  WK r.f vimrc-file
  WK r.c termcolor
  WK r.l browser




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Project-Local Vim Config

  function! GitVimrc_Path()
    let l:current = trim(system('echo ' . expand('%:p:h')))
    let l:gitroot = trim(system('git -C ' . current . ' rev-parse --show-toplevel'))
    return v:shell_error == 0 ? l:gitroot : ''
  endfunction

  function! GitVimrc_Load()
    if !(len(&filetype) && &modifiable) | return | endif
    if (expand('%:p:h') == expand('~')) | return | endif
    if (expand('%:t') == '.vimrc') | return | endif

    let l:gitroot = GitVimrc_Path()
    let l:filename = l:gitroot . '/.vimrc'
    if len(l:gitroot) && filereadable(l:filename)
      exec printf('source %s', l:filename)
    endif
  endfunction

  function! GitVimrc_Edit()
    let l:gitroot = GitVimrc_Path()
    if len(l:gitroot)
      edit gitroot . '/.vimrc'
    endif
  endfunction

  autocmd BufWinEnter * call GitVimrc_Load()




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Snippets

  inoremap <c-;> <esc>/___<cr>cw
  inoremap <c-u> <esc>?___<cr>cw
  inoremap \<tab> <esc>:set paste<cr>my"ycaw<c-r>=trim(join(readfile(expand('~/.vim/snips/<c-r>y'),'b'), "\n"))<cr><esc>:set nopaste<cr>'y/___<cr>cw
  command! -nargs=1 Snip split $HOME/.vim/snips/<args>




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" File Tree Sidebar

  function! Filebar_Open()
    " open flies in the current window
    let g:netrw_chgwin=winnr()+1
    " open the sidebar, do layout
    topleft Lex
    silent vertical resize 31
    set winfixwidth
    set winhighlight=Normal:Filebar,CursorLine:FilebarSelected
    wincmd =
    let g:Filebar_isOpen=1
    let b:Filebar_isFileWindow=1
    " establish buffer-local keybindings
    nnoremap <buffer> <silent> <leader>q :call Filebar_Close()<CR>
    nnoremap <buffer> <silent> <leader>a :call Filebar_OpenInFar(1)<CR>
    nnoremap <buffer> <silent> <leader>f :call Filebar_OpenInFar()<CR>
    nnoremap <buffer> <leader><tab> <c-w>p
  endfunction

  function! Filebar_OpenInFar(left)
    if (a:left)
      let g:netrw_chgwin=2
      normal! <cr>
    else
      wincmd b
      let g:netrw_chgwin=winnr()
      wincmd p
      normal! <cr>
    endif
  endfunction

  function! Filebar_Close()
    if exists('g:Filebar_isOpen')
      unlet g:Filebar_isOpen
    endif
    wincmd t
    if exists('b:Filebar_isFileWindow')
      1q
    endif
    wincmd p
    wincmd =
  endfunction

  function! Filebar_JumpIn()
    if !exists('g:Filebar_isOpen')
      call Filebar_Open()
    endif
    let g:netrw_chgwin = winnr()
    wincmd t
  endfunction

  let g:netrw_liststyle=3
  command! Files call Filebar_Open()
  command! Nofiles call Filebar_Close()
  nnoremap <silent> <leader><tab> :call Filebar_JumpIn()<cr>
  WK <Tab> filebar




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Auto-Pair Brackets

  function! AutoPair_Sub(cmd, open, close)
    return substitute(substitute(a:cmd, '{', a:open, 'g'), '}', a:close, 'g')
  endfunction

  function! AutoPair(pair)
    let [open, close] = split(a:pair, '\zs')
    exe AutoPair_Sub('inoremap {{ {}<left>', open, close)
    exe AutoPair_Sub('inoremap <expr> } (CurIsBefore("}") ? "\<right>" : "}")', open, close)
    exe AutoPair_Sub('inoremap {} {}', open, close)
    exe AutoPair_Sub('inoremap {<CR> {}<left><CR><esc>O', open, close)
  endfunction

  call AutoPair('()')
  call AutoPair('[]')
  call AutoPair('{}')
  " call AutoPair('<>')




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Specific Languages/Filetypes

  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " html/js/css

    " js/cs 'norm' is 2-space indents
    au FileType js,coffee,jade setl shiftwidth=2

    " don't wrap html
    au FileType html setl tw=0

    function! CloseTags_Init()
      setl iskeyword=@,48-57,_,-,192-255
      inoremap <buffer> >> <esc>myF<l"yye`ya></<c-r>y><esc>F<i
      inoremap <buffer> ><CR> <esc>myF<lye`ya></<c-r>y><esc>F<i<cr><esc>O
    endfunction
    au FileType js call CloseTags_Init()

    " re-order css properties
    function! CSS_Comforts()
      nmap <buffer> <leader>css vi{:call css#SortProperties()<CR>
      vmap <buffer> <leader>css :call css#SortProperties()<CR>
      setl iskeyword=@,48-57,_,-,?,!,192-255
    endfunction
    au FileType css,scss call CSS_Comforts()


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " glsl

    au FileType frag,vert,fp,vp,glsl setf glsl 


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " c++

    function! OpenMatchingFile(...)
      for ext in a:000
        let fname = expand("%<") . "." . ext
        if filereadable(fname)
          exe 'edit ' . fname
          return
        endif
      endfor
      echo "No matching file!"
    endfunction

    function! Cpp_Comforts()
      call SwapDashUnderscore()

      iabbr <buffer> inc< #include ><left>
      iabbr <buffer> inc" #include "<left>

      iabbr <buffer> cosnt const

      inoremap <buffer><expr> .. ((CurInString() \|\| CurInComment()) ? '..' : '->')
      inoremap <buffer> ;; ::

      inoremap <buffer> ;s std::
      inoremap <buffer> ;a &

      " open corresponding header/impl files
      nnoremap <buffer> <silent> <leader>ec :call OpenMatchingFile('cc', 'cpp', 'c')<CR>
      nnoremap <buffer> <silent> <leader>eh :call OpenMatchingFile('hh', 'hpp', 'h')<CR>
      WK e.c cpp-impl
      WK e.h cpp-header
    endfunction

    au FileType h,hh,hpp,c,cc,cpp call Cpp_Comforts()


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " lua

    function! Lua_Comforts()
      call SwapDashUnderscore()
    endfunction

    au FileType lua call Lua_Comforts()




":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Experiments 

  " find/replace word under cursor
  nnoremap <leader>% :%s/<c-r><c-w>//g<left><left>
  WK % replace-word

  " Make Y behave like other capitals
  nnoremap Y y$

  " qq to record, Q to replay
  nnoremap Q @q

  " quick git actions
  nnoremap ,gs :Gstatus<cr>
  nnoremap ,gc :Gcommit<cr>
  nnoremap ,gp :Dispatch! git push<cr>
  WK g.name +git

  " preview registers before pasting
  let g:peekaboo_window='bo 60vnew'

  " delete without yanking
  nnoremap gd "_d
  vnoremap gd "_d

  " sort lines by length
  vnoremap g\| !awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR>

  " i use these for temporary things
  WK m which_key_ignore
  WK x which_key_ignore

  " cycle through windows
  nnoremap <tab> :wincmd w<CR>

  " more ergonomic backspace-by-word
  inoremap <c-bs> <c-w>

  " keep selection when indenting
  vnoremap < <gv
  vnoremap > >gv

  " quick toggle virtual edit (useful for quickfix window)
  nnoremap <expr> <leader>vv (':set ve=' . (&ve == '' ? 'all' : '') . "\<CR>")

  " align code
  command! -nargs=1 -range Align <line1>,<line2>!sed "s/<args>/•&•/" | column -t -s "•"



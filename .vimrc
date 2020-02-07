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
    Plug 'bfrg/vim-cpp-modern'
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
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'neoclide/coc.nvim', {'branch': 'release'}

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
  set history=1000

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
  set noeb vb t_vb=

  " we don't need no steeenking scrollbars
  set guioptions-=r
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L

  " make macros/mappings execute faster
  set lazyredraw

  " window layout
  set noequalalways

  " status line
  set statusline=\ \%F%m%r%h%w\ ·\ %p%%\ ·\ %l.%c
  set laststatus=2

  " buffer space top/bottom
  set scrolloff=8

  " fugitive status appears in preview window
  set previewheight=25

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
    let offset = (mode() == 'i') ? 1 : 0
    let id = synID(line('.'), col('.') - offset, 1)
    let id = a:0 ? id : synIDtrans(id)
    return synIDattr(id, 'name')
  endfunction

  function! SetKey(dict, path, val)
    let keys = split(a:path, '\.')
    let dict = a:dict
    while len(keys) > 1
      if !has_key(dict, keys[0]) | let dict[keys[0]] = {} | endif
      let [dict, keys] = [dict[keys[0]], keys[1:]]
    endwhile
    try | let dict[keys[0]] = a:val
    catch | call Warn("Bad SetKey: ".a:path) | endtry
  endfunction

  function! EatChar(...)
    let c = getchar(0)
    return (a:0 ? (a:1 =~ c ? '' : c) : '')
  endfunction

  function! GitRoot()
    let root = system('git -C '.expand('%:p:h').' rev-parse --show-toplevel')
    return v:shell_error == 0 ? trim(root) : ''
  endfunction

  function! Warn(msg)
    echohl WarningMsg | echo a:msg | echohl None
  endfunction

  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " one-liners

  function! Defn(body)
    let f = map(split(a:body, ' | '), {_,x -> trim(x)})
    let f[-1] = substitute(f[-1], '^\(return\)\{,1}', 'return ', 'i')
    exe "fu! ".join(f, "\n")."\nendfu"
  endfunction
  command! -nargs=+ Fn call Defn(<q-args>)

  Fn CurInWord()       | CurChar(-1) =~ '\w'
  Fn CurInComment()    | CurSyntax() =~ 'Comment'
  Fn CurInString()     | CurSyntax() =~ 'String'
  Fn CurInProse()      | CurSyntax() =~ '\(Comment\|String\)'
  Fn CurInCodeWord()   | CurInWord() && !CurInProse()
  Fn CurIsAfter(char)  | CurChar(-1) =~ a:char               
  Fn CurIsBefore(char) | CurChar() =~ a:char                 

  Fn Chars(str)   | split(a:str, '\zs')
  Fn GUI()        | has('gui_vimr') || has('gui_running')
  Fn TitleCase(s) | substitute(a:s, '\<\w', '\U\0', 'g')
  Fn OrStr(a, b)  | len(a:a) ? a:a : a:b




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

  " startup color theme (try to match terminal)

  function! Colors_Init()
    if exists('g:manual_color') | return | endif
    if GUI() | return Colors_Default() | endif

    silent! let term = readfile(expand('~/.config/termcolor'))
    call call('Colors_' . (len(term) ? TitleCase(term[0]) : 'Default'), [])
  endfunction
  
  if has('nvim')
    augroup termcolor | au!
      au VimResume * call Colors_Init()
    augroup END
  endif

  call Colors_Init()




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
  noremap <silent> <leader>w :update<cr>
  WK w write

  " more ergonomic marks
  noremap ' `
  noremap ` '

  " cycle: visual -> visual-[line/block] -> normal
  vnoremap <expr> v (mode() ==# 'v' ? 'V' : '<c-v>')

  " paste without yanking
  vnoremap p "_dP

  " qq to record, Q to replay, :qq to edit
  nnoremap Q @q
  cnoremap qq let @q = '<c-r><c-r>q'<c-b><s-right><s-right><s-right><right><right>

  " default indenting
  set autoindent
  set expandtab
  set sts=2
  set ts=2
  set sw=2

  " detect tabs or spaces
  set smarttab

  " searching is a fast way to navigate
  set ignorecase
  set smartcase
  set incsearch
  set nohlsearch

  " show realtime replacement
  if has('nvim')
    set inccommand=nosplit
  endif

  " usually I want slightly higher than window center
  " can override on different machines with g:zz_offset
  noremap <expr> zz 'zz'.get(g:,'zz_offset',10)."\<c-e>"

  " highlight matching angle brackets
  set matchpairs+=<:>
  let g:matchparen_timeout = 10
  let g:matchparen_insert_timeout = 10

  " operate on the current line
  xnoremap il g_o_o
  xnoremap al $o_o
  onoremap il :normal vil<CR>
  onoremap al :normal val<CR>

  " less chording for snake_case
  function! SwapDashUnderscore()
    inoremap <buffer> <expr> - (CurInCodeWord() ? '_' : '-')
    inoremap <buffer> <expr> _ ((CurInCodeWord() && !CurIsAfter('_')) ? '-' : '_')
  endfunction

  " add a character to the end of the line w/o moving
  function! AppendCurLine(andthen, insertmode)
    exe 'normal myA'.nr2char(getchar())."\<esc>'y".a:andthen
    if a:insertmode | startinsert | endif
  endfunction
  nnoremap <silent> <c-;> :call AppendCurLine('',0)<CR>
  imap <silent> <c-;> <esc>:call AppendCurLine('l',1)<CR>
  nmap <silent> <c-;><c-;> <c-;>;
  imap <silent> <c-;><c-;> <c-;>;

  " align things
  command! -nargs=1 -range Align 
        \ <line1>,<line2>!sed "s/<args>/•&/g" 
                      \ | column -t -s "•" 
                      \ | sed "s/  <args>/<args>/"




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Folding

  " quick fold/unfold
  nnoremap <expr> <leader><space> 'z' . (foldclosed('.') > -1 ? 'A' : 'a')
  WK <SPC> un/fold

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

  " nicer foldtext function
  set foldtext=MyFoldText()
  function! MyFoldText()
    let ind = indent(v:foldstart)
    let info = v:foldend - v:foldstart + 1
    return repeat(" ", ind) . "┆» " . info . " «┆"
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
  nnoremap <silent> <leader>. :b#<cr>
  WK z mru-buffer

  " quick quit window
  nnoremap <silent> <leader>q :q<cr>
  WK q kill-window

  " quick browse files
  nnoremap - :Dirvish %<cr>

  " quick project-level search
  nnoremap <c-/> :Ag!<space>
  vnoremap <c-/> :<c-r>Ag! <c-r><c-w>

  " quick suspend
  nnoremap Z <c-z>

  " edit various things
  nnoremap <leader>ee :e /tmp/scratch-<c-r>=strftime("%Y%m%d%H%M")<cr><cr>i
  nnoremap <leader>ev :e ~/.vimrc<cr>
  WK e.name +edit
  WK e.e *scratch*
  WK e.v vimrc

  " edit file in same directory as current file
  command! -nargs=1 Edit exe 'e '.expand('%:p:h').'/<args>'

  " ctrl-p is all that and a bag of chips
  nnoremap \ :CtrlPMRU<cr>
  nnoremap <c-\> :CtrlP<cr>
  let g:ctrlp_by_filename = 0
  let g:ctrlp_custom_ignore = {'dir':  'build$\|dist$\|tmp$\|node_modules$'}
  let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
  let g:ctrlp_prompt_mappings = {
        \ 'PrtClearCache()': ['<c-o>'], 
        \ 'AcceptSelection("t")': ['<tab>'],
        \ 'AcceptSelection("h")': ['=']
  \ }

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
  set wildignore+=*.luac

  " navigate quickfix (error) list
  noremap <leader>co :Copen<cr>
  noremap <leader>cq :cclose<cr>
  noremap <leader>cn :cn<cr>
  noremap <leader>cp :cp<cr>
  noremap <leader>cc :cc
  WK c.name +quickfix
  WK c.o open
  WK c.q close
  WK c.n next
  WK c.p prev
  WK c.c goto

  " close buffer w/o closing window
  noremap <silent> <leader>bd :silent! b#\|silent! bd #<cr>
  WK b.name +buffer
  WK b.d kill

  " 'zoom' on buffer by opening it alone in a new tab
  noremap <silent> <leader>bz mY:tabe %<cr>`Y
  WK b.z zoom




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Misc Helpers

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
    let out  = ' '    . synIDattr(synID(line('.'),col('.'), 0) ,'name')
    let out .= ' → '  . synIDattr(rootID,                       'name')
    let out .= ' : {' . OrStr(synIDattr(rootID, 'fg'), '-')
    let out .= '/'    . OrStr(synIDattr(rootID, 'bg'), '-') . '} '
    for mod in split('bold italic reverse standout underline undercurl strikethrough')
      let out .= (synIDattr(rootID, mod) ? ' '.mod : '')
    endfor
    return out
  endfunction
  nnoremap <leader>hs :echo HiTrace()<CR>
  nnoremap <leader>hf :set ft?<CR>
  WK h.name +help
  WK h.s show-syntax
  WK h.f show-filetype

  " show help for word under cursor
  nnoremap <leader>hw :h <c-r><c-w><CR>
  vnoremap <leader>hw "yy:h <c-r>y<CR>
  WK h.w help-word

  " sessions
  nnoremap <leader>ss :Obsession ~/.vim/sessions/
  nnoremap <leader>so :source ~/.vim/sessions/
  WK s.name +sessions
  WK s.s save
  WK s.o open

  " get path to current dir/file easily
  cnoreabbr <expr> %d expand('%:p:h')
  cnoreabbr <expr> %f expand('%:t')
  cnoreabbr <expr> %p expand('%:p')
  cnoreabbr <expr> %g GitRoot()

  " reload things
  noremap <leader>rr :syntax sync fromstart<cr>:redraw!<cr>
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

  " soft wrap is useful for for editing prose
  function! SoftWrap()
    setl wrap
    setl formatoptions=l
    setl linebreak
    map <buffer> j gj
    map <buffer> k gk
  endfunction
  command! SoftWrap call SoftWrap()




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Project-Local Vim Config

  function! GitVimrc_Load()
    if !(len(&filetype) && &modifiable) | return | endif  " not in non-files
    if (expand('%:p:h') == expand('~')) | return | endif  " not ~/.vimrc
    if (expand('%:t') == '.vimrc')      | return | endif  " not the .vimrc itself

    let gitroot = GitRoot()
    let filename = gitroot.'/.vimrc'
    if len(gitroot) && filereadable(filename)
      exec 'source '.filename
    endif
  endfunction

  augroup gitvimrc | au!
    au BufNewFile,BufRead * call GitVimrc_Load()
  augroup END




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Snippets

  " create/edit (use 3 or more underscores for a placeholder)
  command! -nargs=1 Snip split ~/.vim/snips/<args>

  " insert
  inoremap \<tab> <esc>"ydiw:r ~/.vim/snips/<c-r>y<cr>=']kJ/___<cr>"_cw
  inoremap \\<tab> <esc>pi/___<cr>"_cw

  " move between placeholders
  "   note: select-mode will clobber clipboard, so black-hole instead
  inoremap <c-l> <esc>/___<cr>"_cw
  inoremap <c-u> <esc>?___<cr>"_cw




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
" Auto-Pair Characters

  " Defines these imaps:    (`|` is cursor position)
  "   |{      ->  {|}
  "   |}      ->  }|
  "   |{}     ->  {}|
  "   |{{     ->  {|
  "   |{_     ->  { | }      (where _ is <SPC>)
  "   |{\n    ->  {\n|\n}
  "   |{<BS>  ->  |

  " `{` and `}` are placeholders for the real open/close
  let s:AutoPair_Maps = {
    \  'open':      'inoremap { {}<left>'
    \, 'openonly':  'inoremap {{ {'
    \, 'openclose': 'inoremap {} {}'
    \, 'newline':   'inoremap {<CR> {}<left><CR><esc>O'
    \, 'space':     'inoremap {<space> {  }<left><left>'
    \, 'backspace': 'inoremap <expr> {<bs> ""'
    \, 'eatclose':  'inoremap <expr> } (CurIsBefore("\}") ? "\<right>" : "\}")'    
  \}
  function! AutoPair_Sub(cmd, open, close)
    return substitute(substitute(a:cmd, '{', a:open, 'g'), '}', a:close, 'g')
  endfunction

  function! AutoPairQuote(c)
    let eatclose = 'CurIsBefore("\}") ? "\<right>" : '
    let notinword = 'CurInWord() ? "\}" : '
    let openclose = '"\{\}\<left>"'
    exe AutoPair_Sub('inoremap <expr> { '.eatclose.notinword.openclose, a:c, a:c)
  endfunction

  function! AutoPair(pair, ...)
    let override = get(a:000, 0, {})
    let [open, close] = split(a:pair, '\zs')
    for [key, val] in items(s:AutoPair_Maps)
      exe AutoPair_Sub(get(override, key, val), open, close)
    endfor
  endfunction

  call AutoPair('()')
  call AutoPair('[]')
  call AutoPair('{}')
  call AutoPair('<>', {'space': 'inoremap {<space> { '})
  call AutoPairQuote('"')
  call AutoPairQuote("'")




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" REPL / Eval

  if has('nvim') " TODO: port to vim8
    function! FT_terminal()
      tnoremap <buffer> <esc> <c-\><c-n>
      nnoremap <buffer> <esc> <c-w>p
      setl nonu
      startinsert
    endfunction

    function! REPL_Send(lines)
      call jobsend(g:last_terminal_job_id, add(a:lines, ''))
    endfunction

    augroup terminal | au!
      au TermOpen * let g:last_terminal_job_id = b:terminal_job_id
      au TermOpen * call FT_terminal()
      au BufEnter term://* startinsert
    augroup END

    command! REPLSendLine call REPL_Send([getline('.')])
    command! -range REPLSendSelection call REPL_Send(getline(<line1>,<line2>))
    command! REPLSendBuffer call REPL_Send(getline(0, line('$')))

    nnoremap <silent> <leader>xx :REPLSendLine<CR>
    vnoremap <silent> <leader>xx :REPLSendSelection<CR>
    nnoremap <silent> <leader>xb :REPLSendBuffer<CR>
    nnoremap <silent> <leader>x, :call REPL_Send([''])<CR>
    WK x.name +execute
    WK x.x line/selection
    WK x.b buffer
    WK x., newline

    nnoremap <leader>tv :botright vne<CR>:terminal 
    nnoremap <leader>ts :botright ne<CR>:terminal 
    WK t.name +terminal
    WK t.v new-vert
    WK t.s new-horiz
  endif

  " run things as vim commands
  vnoremap <leader>xv "yy:@y<cr>
  nnoremap <leader>xv "yyy:@y<cr>
  WK x.v vimscript




"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Specific Languages/Filetypes

  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " html/js/css

    function! CloseTags_Init()
      setl iskeyword=@,48-57,_,-,192-255
      inoremap <buffer> >> <esc>myF<l"yye`ya></<c-r>y><esc>F<i
      inoremap <buffer> ><CR> <esc>myF<lye`ya></<c-r>y><esc>F<i<cr><esc>O
    endfunction

    function! FT_js()
      setl shiftwidth=2
      call CloseTags_Init()
    endfunction

    augroup ft_js | au!
      au FileType js,coffee,jade call FT_js()
    augroup END

    " don't wrap html
    augroup ft_html | au!
      au FileType html setl tw=0
    augroup END

    function! FT_css()
      setl iskeyword=@,48-57,_,-,?,!,192-255
      " re-order css properties
      nmap <buffer> <leader>css vi{:call css#SortProperties()<CR>
      vmap <buffer> <leader>css :call css#SortProperties()<CR>
    endfunction

    augroup ft_css | au!
      au FileType css,scss call FT_css()
    augroup END


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " glsl

    augroup ft_glsl | au!
      au FileType frag,vert,fp,vp,glsl setf glsl 
    augroup END


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " c++

    function! OpenMatchingFile(...)
      for ext in a:000
        let fname = expand("%<") . "." . ext
        if filereadable(fname)
          exe 'edit ' . fname
          return
        endif
      endfor
      call Warn("No matching file!")
    endfunction

    function! Cpp_ShouldArrow()
      return CurInCodeWord() || CurIsAfter(')') || CurIsAfter(']')
    endfunction

    function! FT_cpp()
      call SwapDashUnderscore()

      " speed-dial
      inoremap <buffer><expr> > (CurIsBefore('>') ? "\<right>" : (Cpp_ShouldArrow() ? '->' : '>'))
      inoremap <buffer> ;; ::
      inoremap <buffer> ;s std::
      inoremap <buffer> ;a &
      inoremap <buffer> ;i #include 
      inoremap <buffer> ;t template <><left>

      " override Auto-Pairs for stream operators
      inoremap <buffer> << <<

      " typos
      iabbr <buffer> cosnt const
      iabbr <buffer> atuo auto

      " open corresponding header/impl files
      nnoremap <buffer><silent> <leader>ec :call OpenMatchingFile('cc', 'cpp', 'c')<CR>
      nnoremap <buffer><silent> <leader>eh :call OpenMatchingFile('hh', 'hpp', 'h')<CR>
      WK e.c cpp-impl
      WK e.h cpp-header

      " dunno why this is glitchy, but it isn't necessary
      syn clear cUserLabel
    endfunction

    augroup ft_cpp | au!
      au FileType h,hh,hpp,c,cc,cpp call FT_cpp()
    augroup END


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " lua

    function! FT_lua()
      call SwapDashUnderscore()
      iabbr let local
    endfunction

    augroup ft_lua | au!
      au FileType lua call FT_lua()
    augroup END


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " vim

    function! FT_vim()
      inoremap <buffer> " "
      inoremap <buffer> "" ""<left>

      " speed-dial
      inoremap <buffer> ;fu function! X<cr>endfunction<esc>k0fXs
      inoremap <buffer> ;ag augroup X \| au!<cr>augroup END<esc>k0fXs
      inoremap <buffer> ;b <buffer> 
      inoremap <buffer> ;e <expr>
      inoremap <buffer> ;l <lt>leader>
    endfunction

    augroup ft_vim | au!
      au FileType vim call FT_vim()
    augroup END


  "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  " quickfix

    function! FT_qf()
      setl nowrap
      exe get(g:, 'qf_winsize', 10).' wincmd _' 
    endfunction

    augroup ft_qf | au!
      au FileType qf call FT_qf()
    augroup END




":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Experiments 

  " find/replace word under cursor
  nnoremap <leader>% :%s/<c-r><c-w>//g<left><left>
  WK % replace-word

  " sort lines by length
  vnoremap g\| !awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR>

  " temporary things
  WK m (scratch)

  " easier scrolling w/o moving cursor
  nnoremap <up> <c-y>
  nnoremap <down> <c-e>

  " working with tabs
  nnoremap <silent> { :tabprev<CR>
  nnoremap <silent> } :tabnext<CR>

  " regex substitute
  cnoreabbr %s %s/\v<c-r>=EatChar()<CR>

  " ctags
  function! RefreshCtags()
    let root = GitRoot()
    if !len(root) | call Warn("Not in a git repo") | return | endif
    exe 'silent! !ctags -R -f '.root.'/.git/tags '.root
  endfunction
  nnoremap <silent> <leader>rt :call RefreshCtags()<CR>

  " leave cursor in place if possible
  nnoremap <c-d> 30<c-e>
  nnoremap <c-u> 30<c-y>

  " quick jump between windows
  nnoremap <tab> <c-w>p

  " quick buffer-local abbr
  cabbr mapl map <buffer> 
  cabbr inol inoremap <buffer>


  nnoremap <leader>= :wincmd =<CR>
  WK = eq-windows

  set tags^=.git/tags



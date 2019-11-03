" vim: set foldmethod=indent sw=2 et :

set nocompatible
let mapleader = "," 
filetype plugin on


"``````````````````````````````````````````````````````````````````````````````
" vim-plug 

  call plug#begin('~/.vim/plugged')

  " rougly in order of most life-changing
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'justinmk/vim-dirvish'
    Plug 'tpope/vim-vinegar'
    Plug 'tpope/vim-commentary'
    Plug 'rking/ag.vim'
    Plug 'tpope/vim-dispatch'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-obsession'
    Plug 'ciaranm/detectindent'
    Plug 'liuchengxu/vim-which-key'

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

  " test-driving
    Plug 'junegunn/vim-peekaboo'
    Plug 'tpope/vim-eunuch'
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'neoclide/coc.nvim', {'branch': 'release'}
    " Plug 'kassio/neoterm'

  call plug#end()




"``````````````````````````````````````````````````````````````````````````````
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




"``````````````````````````````````````````````````````````````````````````````
" UI Options

  set nu
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




"```````````````````````````````````````````````````````````````````````````````
" Colors

  syn on
  set t_Co=256

  " easy switch light/dark
  function! Colors_Light()
    set bg=light
    " colo mnml-light
    colo one
  endfunction

  function! Colors_Dark()
    set bg=dark
    colo smyck
  endfunction

  command! Dark call Colors_Dark() | let g:manual_color=1
  command! Light call Colors_Light() | let g:manual_color=1

  " startup color theme
  function! Colors_MatchTerminal()
    if (!exists('g:manual_color'))
      let l:termcolor = readfile(expand('~/.config/termcolor'))
      if (len(l:termcolor))
        if (l:termcolor[0] == 'light')
          call Colors_Light()
        else
          call Colors_Dark()
        endif
      else
        " default
        call Colors_Dark()
      endif
    endif
  endfunction
  
  call Colors_MatchTerminal()

  if has('nvim')
    au VimResume * call Colors_MatchTerminal()
  endif




"``````````````````````````````````````````````````````````````````````````````
" In-buffer Editing

  " quick hop up and down
  map <c-k> 8k
  map <c-j> 8j

  " quick comment
  nmap <leader>; gcl
  vmap <leader>; gc

  " quick save
  map <leader>s :w<cr>

  " more ergonomic marks
  noremap ' `
  noremap ` '

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

  " detect tabs or spaces
  set smarttab

  " force tabs instead of spaces
  command! TABS set noexpandtab|set sts=0|set ts=4|set sw=0

  " force tab size
  noremap <leader>2 :set shiftwidth=2<cr>
  noremap <leader>4 :set shiftwidth=4<cr>




"``````````````````````````````````````````````````````````````````````````````
" Folding

  " quick recursive fold/unfold
  noremap <leader><space> zA

  " open folds by default
  set foldlevel=99

  " indent folding is the kiss solution
  set foldmethod=indent

  " whatever
  set foldnestmax=12

  " fold/unfold one level
  noremap <leader>f<leader> za

  " set foldlevel to a particular level
  noremap <leader>fs :setl foldlevel=

  " jump to the next/previous fold and toggle it
  noremap <leader>fj zjza
  noremap <leader>fk zkza

  " no fillchars
  set fillchars=fold:\ 

  " open folds by clicking them
  function! OpenClickedFold()
    if (foldclosed('.') > -1)
      normal za
    endif
  endfunction
  nmap <LeftRelease> :call OpenClickedFold()<cr>

  " nicer foldtext function
  set foldtext=MyFoldText()
  function! MyFoldText()
    let line = getline(v:foldstart)
    let l:ind = indent(v:foldstart - 1)
    let l:ind2 = indent(v:foldstart) - l:ind - 1
    if match( line, '^[ \t]*\(\/\*\|\/\/\)[*/\\]*[ \t]*$' ) == 0
      let initial = substitute( line, '^\([ \t]\)*\(\/\*\|\/\/\)\(.*\)', '\1\2', '' )
      let linenum = v:foldstart + 1
      while linenum < v:foldend
        let line = getline( linenum )
        let comment_content = substitute( line, '^\([ \t\/\*]*\)\(.*\)$', '\2', 'g' )
        if comment_content != ''
          break
        endif
        let linenum = linenum + 1
      endwhile
      let sub = initial . ' ' . comment_content
    else
      let sub = line
      let startbrace = substitute( line, '^.*{[ \t]*$', '{', 'g')
      if startbrace == '{'
        let line = getline(v:foldend)
        let endbrace = substitute( line, '^[ \t]*}\(.*\)$', '}', 'g')
        if endbrace == '}'
          let sub = sub.substitute( line, '^[ \t]*}\(.*\)$', '...}\1', 'g')
        endif
      endif
    endif
    let n = v:foldend - v:foldstart + 1
    let info = n
    let sub = "                                                                                                                  "
    let num_w = getwinvar( 0, '&number' ) * getwinvar( 0, '&numberwidth' )
    let fold_w = getwinvar( 0, '&foldcolumn' )
    let sub = strpart( sub, 0, winwidth(0) - strlen( info ) - num_w - fold_w - 1 )
    let subin = strpart( sub, 0, n - 1 )
    return repeat(" ", l:ind) . "┆ " . repeat(" ", l:ind2-2) . "» " . info . " « ┆"
  endfunction




"``````````````````````````````````````````````````````````````````````````````
" File/Buffer/Window Navigation

  " quick jump windows
  noremap <c-h> <c-w>h
  noremap <c-l> <c-w>l
  noremap <leader>j <c-w>j
  noremap <leader>k <c-w>k

  " quick switch between 2 buffers
  noremap <leader>z :b#<cr>

  " quick quit window
  noremap <leader>q :q<cr>

  " quick browse files
  nnoremap - :Dirvish %<cr>

  " quick project-level search
  noremap <leader>/ :Ag!<space>

  " quick suspend
  nnoremap Z <c-z>

  " open a scratch buffer
  map <leader>ee :e /tmp/scratch-<c-r>=strftime("%Y%m%d%H%M")<cr><cr>i

  " ctrl-p is all that and a bag of chips
  nnoremap \ :CtrlP<cr>
  noremap <tab> :CtrlPMRU<cr>
  let g:ctrlp_by_filename = 0
  let g:ctrlp_custom_ignore = {'dir':  'build$\|dist$\|tmp$\|node_modules$'}
  let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

  " jump to specific buffer (obsoleted by ctrlp plugin)
  noremap <c-\> :ls<cr>:b<space>

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
  map <leader>w <c-w>
  map <leader>w\ :vsp<cr>

  " navigate quickfix (error) list
  noremap <leader>co :Copen<cr>
  noremap <leader>cO :copen<cr>
  noremap <leader>cn :cn<cr>
  noremap <leader>cp :cp<cr>
  noremap <leader>cc :cc

  " close buffer w/o closing window
  noremap <leader>bd :b#\|bd \#<cr>




"```````````````````````````````````````````````````````````````````````````````
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
    nnoremap <buffer> <leader>a :call Filebar_OpenInFar(1)<cr>
    nnoremap <buffer> <leader>f :call Filebar_OpenInFar()<cr>
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
    if exists('g:Filebar_isOpen')
      let g:netrw_chgwin = winnr()
      wincmd t
    endif
  endfunction

  command! Files call Filebar_Open()
  command! Nofiles call Filebar_Close()
  noremap <leader><tab> :call Filebar_JumpIn()<cr>
  let g:netrw_liststyle=3




"```````````````````````````````````````````````````````````````````````````````
" Misc Utils

  function! CursorSyntax()
    return join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'))
  endfunction

  function! CurChar(...)
    let l:offset = 1 - (a:0 > 0 ? a:1 : 0)
    return strcharpart(strpart(getline('.'), col('.') - l:offset), 0, 1) 
  endfunction

  " easy/powerful autocomplete from all buffers
  function! SmartTab_Complete()
    if (pumvisible()) | return "\<C-N>" | endif
    return ((col('.') > 1) && (CurChar(-1) =~ '\w')) ? "\<C-N>" : "\<tab>"
  endfunction
  inoremap <tab> <c-r>=SmartTab_Complete()<CR>
  set completeopt=menuone

  " show syntax stack of character under cursor
  function! SynStack()
    let stack  = 'hi<' . synIDattr(synID(line("."),col("."),1),"name") . '> '
    let stack .= 'trans<' . synIDattr(synID(line("."),col("."),0),"name") . '> '
    let stack .= 'lo<' . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . '>'
    return stack
  endfunction
  map <leader>hc :echo SynStack()<CR>

  " sessions
  nnoremap <leader>os :Obsession ~/.vim/sessions/
  nnoremap <leader>oo :source ~/.vim/sessions/

  " soft wrap is useful for for editing prose
  command! SoftWrap set wrap|set formatoptions=l|set lbr|map j gj|map k gk

  " barebones snippets
  inoremap <c-l> <esc>/___<cr>cw
  inoremap <c-u> <esc>?___<cr>cw
  inoremap ;<tab> <esc>:set paste<cr>my"ycaw<c-r>=trim(join(readfile(expand('~/.vim/snips/<c-r>y'),'b'), "\n"))<cr><esc>:set nopaste<cr>'yi<c-l>
  command! -nargs=1 Snip split $HOME/.vim/snips/<args>

  " run things as vim commands
  noremap <leader>vx my_v$hy:<c-r>"<cr>'y
  vnoremap <leader>vx "yy:@y<cr>

  " reload things
  noremap <leader>rr :syntax sync fromstart<cr> 
  noremap <leader>rv :so ~/.vimrc<cr>:echo "reloaded"<cr>
  noremap <leader>rg :call GitVimrc_Load()<cr>
  noremap <leader>rf :so %<cr>
  noremap <leader>rc :call Colors_MatchTerminal()<cr>
  noremap <leader>rl kw<cr>:!osascript ~/bin/reload_chcan<cr><cr>

  " help me find/remember mappings
  nnoremap <silent> <leader> :WhichKey ','<CR>




"```````````````````````````````````````````````````````````````````````````````
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




"```````````````````````````````````````````````````````````````````````````````
" Specific Languages/Filetypes

  "````````````````````````````````````````
  " HTML/JS/CSS

    " js/cs 'norm' is 2-space indents
    au FileType js,coffee,jade setl shiftwidth=2

    " don't wrap html
    au FileType html setl tw=0

    function! CloseTags_Init()
      setl iskeyword=@,48-57,_,-,192-255
      inoremap <buffer> >> <esc>myF<l"yye`ya></<c-r>y><esc>F<i
      inoremap <buffer> ><cr> <esc>myF<lye`ya></<c-r>y><esc>F<i<cr><esc>O
    endfunction
    au FileType js call CloseTags_Init()

    " re-order css properties
    function! CSS_Comforts()
      nmap <buffer> <leader>css vi{:call css#SortProperties()<cr>
      vmap <buffer> <leader>css :call css#SortProperties()<cr>
      setl iskeyword=@,48-57,_,-,?,!,192-255
    endfunction
    au FileType css,scss call CSS_Comforts()


  "``````````````````````````````````````````````````
  " glsl

    au FileType frag,vert,fp,vp,glsl setf glsl 


  "````````````````````````````````````````
  " c++

    function! IfInWord(then_, else_)
      return (CurChar(-1) =~ '\w') ? a:then_ : a:else_
    endfunction

    function! IfAfterUnderscore(then_, else_)
      return CurChar(-1) =~ '_' ? a:then_ : a:else_
    endfunction

    function! OpenMatchingFile(...)
      for ext in a:000
        let fname = expand("%<") . "." . ext
        if filereadable(fname)
          edit fname
          return
        endif
      endfor
      echo "No matching file!"
    endfunction

    function! Cpp_Comforts()
      iabbr <buffer> inc< #include ><left>
      iabbr <buffer> inc" #include "<left>

      inoremap <buffer> - <c-r>=IfInWord("_", "-")<CR>
      inoremap <buffer> _ <c-r>=IfInWord(IfAfterUnderscore("_", "-"), "_")<CR>

      inoremap <buffer> .. ->
      inoremap <buffer> ;; ::
      inoremap <buffer> ,, std::

      " open corresponding header/impl files
      nnoremap <buffer> <leader>ec :call OpenMatchingFile("cc", "cpp", "c")<CR>
      nnoremap <buffer> <leader>eh :call OpenMatchingFile("hh", "hpp", "h")<CR>
    endfunction

    au FileType h,hh,hpp,c,cc,cpp call Cpp_Comforts()


  "````````````````````````````````````````
  " lua

  function! InLuaString()
    let syn = CursorSyntax()
    return syn =~? 'luaString' ? 1 : 0
  endfunction

  function! InLuaComment()
    let syn = CursorSyntax()
    return syn =~? 'luaComment' ? 1 : 0
  endfunction

  function! Smart_Underscore()
    let line = getline('.')
    let substr = strpart(line, -1, col('.'))
    let force_hyphen = InLuaString() || InLuaComment()
    let is_word = match(substr, '\w$') > -1
    if (force_hyphen || !is_word)
      return "-"
    else
      return "_"
    end
  endfunction

  au FileType lua inoremap <buffer> - <c-r>=Smart_Underscore()<CR>




"``````````````````````````````````````````````````````````````````````````````
"``````````````````````````````````````````````````````````````````````````````
" Experiments 

  " find/replace word under cursor
  nnoremap <leader>% :%s/<c-r><c-w>//g<left><left>

  " Make Y behave like other capitals
  nnoremap Y y$

  " qq to record, Q to replay
  nnoremap Q @q

  " just feels right
  vnoremap v V

  " poor man's pear-tree
  inoremap [] []<left>
  inoremap {} {}<left>
  inoremap {<cr> {}<left><cr><esc>O
  inoremap /*<space> /* X */<esc>FXs

  cabbr <expr> %% expand('%:p:h')

  nnoremap ,gs :Gstatus<cr>
  nnoremap ,gc :Gcommit<cr>
  nnoremap ,gp :Dispatch! git push<cr>

  nnoremap ,tt :Topen<cr>


  " if has('nvim')
  "   autocmd BufWinEnter,WinEnter term://* startinsert
  "   tnoremap <esc> <C-\><C-n>
  "   tnoremap <C-h> <C-\><C-n><C-w><C-h>
  "   tnoremap <C-l> <C-\><C-n><C-w><C-l>
  "   nnoremap <leader>xx :TREPLSendLine<cr>
  "   vnoremap <leader>xx 20<gv:TREPLSendLine<cr>u
  "   nnoremap <leader>xf myggVG20<gv:TREPLSendLine<cr>u`y
  "   nnoremap <leader>xo myvap20<gv:TREPLSendLine<cr>u`y

  "   " hack to send line like `local foo = bar` without 'local'
  "   noremap <leader>xl my0f lv$h:TREPLSendSelection<cr>`y
  " end

  " copy current line w/ no newline
  map <leader>vy my_v$hy

  let g:peekaboo_window='bo 60vnew'

  " delete without yanking
  nnoremap gd "_d
  vnoremap gd "_d
  " paste without yanking
  vnoremap p "_dP


" vim: set foldmethod=indent :

set nocompatible
let mapleader = "," 
filetype plugin on


"``````````````````````````````````````````````````````````````````````````````
" vim-plug 

  call plug#begin('~/.vim/plugged')

  " Rougly in order of most life-changing
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'justinmk/vim-dirvish'
    Plug 'tpope/vim-vinegar'
    Plug 'tpope/vim-commentary'
    Plug 'rking/ag.vim'
    " Plug 'tpope/vim-surround'
    Plug 'tpope/vim-obsession'
    Plug 'ciaranm/detectindent'

  " Language Runtimes
    Plug 'tpope/vim-markdown'
    Plug 'guns/vim-clojure-static'
    Plug 'ingydotnet/yaml-vim'
    Plug 'vim-scripts/glsl.vim'
    Plug 'pangloss/vim-javascript'
    Plug 'hail2u/vim-css3-syntax'
    Plug 'jonsmithers/vim-html-template-literals'
    Plug 'Quramy/vim-js-pretty-template'
    " Plug 'runningskull/vim-mustache-handlebars'
    " Plug 'keith/swift.vim'
    " Plug 'chr4/nginx.vim'
    " Plug 'leafgarland/typescript-vim'
    Plug 'tbastos/vim-lua'
    Plug 'bfrg/vim-cpp-modern'
    Plug 'artoj/qmake-syntax-vim'

  " Test-driving
    Plug 'junegunn/vim-peekaboo'
    " Plug 'tmsvg/pear-tree'
    " Plug 'kshenoy/vim-signature'
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'othree/html5.vim'
    " Plug 'kassio/neoterm'
    Plug 'liuchengxu/vim-which-key'
    Plug 'tpope/vim-dispatch'
    Plug 'tpope/vim-eunuch'

  call plug#end()
  



"``````````````````````````````````````````````````````````````````````````````
" Experiments 

  vnoremap v V

  
  " barebones snippets
  inoremap <c-l> <esc>/___<cr>cw
  inoremap <c-u> <esc>?___<cr>cw
  nnoremap <c-n> /___<cr>.
  nnoremap <c-p> ?___<cr>.
  imap ;<tab> <esc>:set paste<cr>my"ycaw<c-r>=trim(join(readfile(expand('~/.vim/snips/<c-r>y'),'b'), "\n"))<cr><esc>:set nopaste<cr>'yi<c-l>
  nnoremap ;<tab> "yyaw:sp<cr>:e $HOME/.vim/snips/<c-r>y<cr>i


  " open corresponding header/impl files
  nnoremap <leader>ec :e %<.cc<CR>
  nnoremap <leader>eh :e %<.hh<CR>

  inoremap <c-o> <esc>O

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

  nnoremap <silent> <leader> :WhichKey ','<CR>


  if has('nvim')
    autocmd BufWinEnter,WinEnter term://* startinsert
    tnoremap <esc> <C-\><C-n>
    tnoremap <C-h> <C-\><C-n><C-w><C-h>
    tnoremap <C-l> <C-\><C-n><C-w><C-l>
    nnoremap <leader>xx :TREPLSendLine<cr>
    vnoremap <leader>xx 20<gv:TREPLSendLine<cr>u
    nnoremap <leader>xf myggVG20<gv:TREPLSendLine<cr>u`y
    nnoremap <leader>xo myvap20<gv:TREPLSendLine<cr>u`y

    " hack to send line like `local foo = bar` without 'local'
    noremap <leader>xl my0f lv$h:TREPLSendSelection<cr>`y
  end


  " useful for doing mappings only in certain contexts
  function! CursorSyntax()
    return join(map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")'))
  endfunction

  function! CurChar()
    return strcharpart(strpart(getline('.'), col('.') - 1), 0, 1) 
  endfunction
  

  command! Nococ let b:coc_enabled=0|let b:coc_suggest_disable=1


  " Use tab for trigger completion with characters ahead and navigate.
  " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
  " inoremap <silent><expr> <TAB>
  "       \ pumvisible() ? "\<C-n>" :
  "       \ <SID>check_back_space() ? "\<TAB>" :
  "       \ coc#refresh()
  " inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
  endfunction

  " let g:ycm_semantic_triggers = {
  "     \   'css': [ 're!^', 're!^\s+', ': ' ],
  "     \   'scss': [ 're!^', 're!^\s+', ': ' ],
  "     \ }

  map <leader>rl kw<cr>:!osascript ~/bin/reload_chcan<cr><cr>

  function! CloseTags_Init()
    set iskeyword=@,48-57,_,-,192-255
    inoremap >> <esc>myF<l"yye`ya></<c-r>y><esc>F<i
    inoremap ><cr> <esc>myF<lye`ya></<c-r>y><esc>F<i<cr><esc>O
  endfunction
  au BufNewFile,BufReadPost *.js call CloseTags_Init()



  " let g:pear_tree_pairs = {
  "       \ '(': {'closer': ')'},
  "       \ '[': {'closer': ']'},
  "       \ '{': {'closer': '}'},
  "       \ "'": {'closer': "'"},
  "       \ '"': {'closer': '"'},
  "       \ 'html`': {'closer': '`'},
  "       \ 'css`': {'closer': '`'}
  "       \ }


  let g:ycm_enable_diagnostic_highlighting = 0 
  let g:ycm_show_diagnostics_ui = 0
  let g:ycm_enable_diagnostic_signs = 0



  " needed for indent in <style> tags
  let g:html_indent_style1 = "inc"

  " this behavior is weird and i don't need the benefit it provides
  " let g:pear_tree_repeatable_expand=0

  " close buffer w/o closing window
  map <leader>bd :bp\|bd \#<cr>

  " re-order css properties
  " nmap ,css vi{:call css#SortProperties()<cr>
  " vmap ,css :call css#SortProperties()<cr>

  " run the current line as a command
  map ,,x my_v$hy:<c-r>"<cr>'y

  " copy current line
  map ,,y my_v$hy


  let g:peekaboo_window='bo 60vnew'

  " delete without yanking
  nnoremap gd "_d
  vnoremap gd "_d
  " paste without yanking
  vnoremap p "_dP

  nnoremap - :Dirvish %<cr>


  "------------------------------------------------------------
  " load a .vimrc from the git root if one exists

  function! GitVimrc_Path()
    let l:current = trim(system('echo ' . expand('%:p:h')))
    let l:gitroot = trim(system('git -C ' . current . ' rev-parse --show-toplevel'))
    if v:shell_error == 0
      return l:gitroot
    else
      return ''
    endif
  endfunction

  function! GitVimrc_Load()
    if !(len(&filetype) && &modifiable)
      return
    endif
    let l:doload = expand('%:t') != '.vimrc' " don't apply to the vimrc itself
    let l:gitroot = GitVimrc_Path()
    let l:filename = l:gitroot . '/.vimrc'
    if l:doload && len(l:gitroot) && filereadable(l:filename)
      exec printf('source %s', l:filename)
      " echom 'Loaded ' l:gitroot '/.vimrc'
    endif
  endfunction

  function! GitVimrc_Edit()
    let l:gitroot = GitVimrc_Path()
    if len(l:gitroot)
      edit gitroot . '/.vimrc'
    endif
  endfunction

  autocmd BufWinEnter * call GitVimrc_Load()

  "------------------------------------------------------------


  " Shift Width
  noremap <leader>2 :set shiftwidth=2<cr>
  noremap <leader>4 :set shiftwidth=4<cr>



  " File browsing sidebar
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



  " Open a scratch buffer
  map <leader>ee :e /tmp/scratch-<c-r>=strftime("%Y%m%d%H%M")<cr><cr>i

  " Use system clipboard
  set clipboard=unnamed

  " Sane regex searching
  map <leader><leader>/ /\v

  " Shortcuts
  " ---------
  " window mode
  map <leader>w <c-w>
  map <leader>w\ :vsp<cr>
  " save
  map <leader>s :w<cr>
  " comment
  nmap <leader>; gcl
  vmap <leader>; gc
  " quit
  noremap <leader>q :q<cr>

  " Lua plugin is too zealous
  " let g:lua_complete_dynamic=0

  " syntax checking is handy, but there's a but that
  " causes random lines to turn red, even after error
  " is fixed. could remove this if that gets resolved.
  let g:lua_check_syntax=0

  " CSS uses hyphens & stuff
  autocmd FileType css,scss set iskeyword=@,48-57,_,-,?,!,192-255

  " Turn on Swift Autocomplete
  " autocmd BufNewFile,BufRead *.swift set filetype=swift

  " custom file extensions
  autocmd BufNewFile,BufRead *.cfg set filetype=yaml
  " autocmd BufNewFile,BufRead *.hhtml set filetype=html.handlebars
  " autocmd BufNewFile,BufRead *.cljx set filetype=clojure
  " autocmd BufNewFile,BufRead *.tag set filetype=javascript

  let g:mustache_operators = 1

  " iabbr eldl // eslint-disable-line
  " map <tab> mx=a{'x

  " nmap <silent> ,ln <Plug>(ale_next_wrap)
  " nmap <silent> ,lp <Plug>(ale_previous_wrap)

  " slightly nicer vim-vinegar experience
  let g:netrw_liststyle=3







"``````````````````````````````````````````````````````````````````````````````
" UI Options


  " make macros/mappings execute faster
  set lazyredraw

  " mouse in the terminal
  set mouse=a
  if has('mouse_sgr')
    set ttymouse=sgr
  endif

  " no beeping
  set noeb
  set novb

  " paste in the terminal
  " nnoremap <leader>p :set invpaste paste?<cr>i
  " set pastetoggle=<leader>p
  " set showmode

  " we don't need no steeenking scrollbars
  set guioptions-=r
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L

  " quickly find active line
  " set cursorline

  " fugitive status appears in preview window
  set previewheight=25

  " window layout
  set noequalalways

  " Status Bar
  function! StatusMode()
    let l:mode = mode(1)
    if (l:mode == 'i')
      return '-I- '
    elseif (l:mode == 'n')
      return '[N] '
    elseif (l:mode == 'v')
      return '#V# '
    else
      return l:mode
    endif
  endfunction

  " " with file format:
  " set statusline=\ %{StatusMode()}\ \%F%m%r%h%w\ (%{&fileformat})\ \|\ %l×%v
  " " without file format
  " set statusline=\ %{StatusMode()}\ \%F%m%r%h%w\ \(%L\)\ \|\ %l×%v
  " without mode indicator
  set statusline=\ \%F%m%r%h%w\ \(%L\)\ \|\ %l.%v
  set laststatus=2

  " Buffer space top/bottom
  set scrolloff=10

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
  function! ColorScheme_Light()
    set bg=light
    colo mnml-light
  endfunction

  function! ColorScheme_Dark()
    set bg=dark
    colo smyck
  endfunction

  command! Dark call ColorScheme_Dark() | let g:manual_color=1
  command! Light call ColorScheme_Light() | let g:manual_color=1

  " startup color theme
  function! ColorScheme_Init()
    if (!exists('g:manual_color'))
      let l:termcolor = readfile(expand('~/.config/termcolor'))
      if (len(l:termcolor))
        if (l:termcolor[0] == 'light')
          call ColorScheme_Light()
        else
          call ColorScheme_Dark()
        endif
      else
        " default
        call ColorScheme_Dark()
      endif
    endif
  endfunction
  
  call ColorScheme_Init()

  if has('nvim')
    au VimResume * call ColorScheme_Init()
  endif




"``````````````````````````````````````````````````````````````````````````````
" Vim Core

  " Fix for delay exiting insert mode in terminal
  set timeoutlen=500 ttimeoutlen=0

  " Swap files in their own folder
  set directory=~/.backup//,/tmp//
  set undodir=~/.undo//,/tmp//

  " Undo, etc.
  set undofile
  set undolevels=200
  set undoreload=500

  " Store lots of history (default is 20)
  set history=500

  " Allow vim to hide buffers w/o saving
  set hidden

  " Allow folding single-line folds
  set fml=0


  " Reload things
  " syntax highlighting
  noremap <leader>rr :syntax sync fromstart<cr> 
  " vimrc
  noremap <leader>rv :so ~/.vimrc<cr>:echo "reloaded"<cr>
  noremap <leader>rg :call GitVimrc_Load()<cr>
  " current file
  noremap <leader>rf :so %<cr>
  " color
  noremap <leader>rc :call ColorScheme_Init()<cr>




"``````````````````````````````````````````````````````````````````````````````
" In-buffer Editing

  " make backspace delete everything
  set bs=2

  " Indenting
  set autoindent
  set expandtab
  set sts=2
  set ts=2
  set sw=2

  command! TABS set noexpandtab|set sts=0|set ts=4|set sw=0

  " Detect tabs or spaces
  set smarttab

  " Line numbers
  set nu " set nonu
  set foldcolumn=0

  " Line wrapping pfff :)
  set nowrap

  " For navigating context menus w/o using arrows
  function! OmniPopupMove(action)
    if pumvisible()
      if a:action == 'j'
        return "\<C-N>"
      elseif a:action == 'k'
        return "\<C-P>"
      endif
    endif
	return ''
  endfunction
  inoremap <silent><c-j> <C-R>=OmniPopupMove('j')<CR>
  inoremap <silent><c-k> <C-R>=OmniPopupMove('k')<CR>

  " Searching is a fast way to navigate
  set ignorecase
  set smartcase
  set incsearch
  set nohls

  " Copy to system clipboard
  " noremap <leader>ca mygg"*yG'y
  " vnoremap <leader>cc "*y




"``````````````````````````````````````````````````````````````````````````````
" Folding

  " Open folds by default
  autocmd BufRead * normal zR

  " Indent folding is the KISS solution
  set foldmethod=indent

  " whatever
  set foldnestmax=12

  " quick fold/unfold
  noremap <leader><space> zA

  " fold/unfold recursively
  noremap <leader>ff zA

  " set foldlevel to a particular level
  noremap <leader>fs :set foldlevel=

  " jump to the next/previous fold and toggle it
  noremap <leader>fj zjza
  noremap <leader>fk zkza

  set fillchars=fold:\ 




"``````````````````````````````````````````````````````````````````````````````
" File/Buffer/Window Navigation

  " Quickly list open buffers (obsoleted by ctrlp plugin)
  noremap <c-\> :ls<cr>:b<space>

  " Quickly switch between 2 buffers (cancels ZZ to close)
  noremap <leader>z :b#<cr>

  " ctrl-p is all that and a bag of chips
  nnoremap <tab> :CtrlP<cr>
  noremap \ :CtrlPMRU<cr>
  let g:ctrlp_by_filename = 0
  let g:ctrlp_custom_ignore = {'dir':  'build$\|dist$\|tmp$\|node_modules$'}
  let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

  " sane autocomplete
  set completeopt=longest,menuone
  set wildmode=list:longest

  " omnicompletion - more friendly enter key
  " inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

  " Ignore certain files in autocomplete
  set wildignore+=*.png,*.gif,*.jpg,*.psd
  set wildignore+=*.pyc,.git
  set wildignore+=*.swp,.DS_Store
  set wildignore+=*.scssc
  set wildignore+=*/node_modules/*
  set wildignore+=*/bower_components/*
  set wildignore+=*/build/*

  " Switch windows easily.
  noremap <c-h> <c-w>h
  noremap <c-l> <c-w>l
  noremap <leader>j <c-w>j
  noremap <leader>k <c-w>k

  " Using silver searcher instead
  noremap <leader>/ :Ag!<space>

  " navigate quickfix (error) list
  noremap <leader>co :Copen<cr>
  noremap <leader>cO :copen<cr>
  noremap <leader>cn :cn<cr>
  noremap <leader>cp :cp<cr>
  noremap <leader>cc :cc

  " usually I don't actually want the window center
  noremap zz zz10<c-e>

  " Make marks a little friendlier
  noremap ' `
  noremap ` '

  " quicker suspend
  nnoremap Z <c-z>



"```````````````````````````````````````````````````````````````````````````````
" Sessions

  nnoremap <leader>os :Obsession ~/.vim/sessions/
  nnoremap <leader>oo :source ~/.vim/sessions/



"``````````````````````````````````````````````````````````````````````````````
" Specific Languages/Filetypes

  " Wrap lines. Useful for editing prose (blog post, email, comments, etc...)
  command! SoftWrap set wrap|set formatoptions=l|set lbr|map j gj|map k gk

  " Close HTML Tags
  " inoremap /<tab> </<c-x><c-o><esc>a

  " JS/CS 'norm' is 2-space indents
  au BufNewFile,BufReadPost *.js,*.coffee,*.jade setl shiftwidth=2

  " GLSL
  au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl 

  " Django Templates
  au BufNewFile,BufRead *.html command! HTML setfiletype htmldjango

  " Don't wrap HTML
  au BufNewFile,BufRead *.html set tw=0




"``````````````````````````````````````````````````````````````````````````````
" Stuff that's 1/2 effective - maybe find a plugin?

  " Util for aligning things w/ colons (like CSS properties or JS object fields)
  " noremap <leader>m 0f:wi<tab><esc>
  " vnoremap <leader>m :norm ,m<cr>


  " HTML 5 tags
  syn keyword htmlTagName contained article aside audio bb canvas command datagrid
  syn keyword htmlTagName contained datalist details dialog embed figure footer
  syn keyword htmlTagName contained header hgroup keygen mark meter nav output
  syn keyword htmlTagName contained progress time ruby rt rp section time video
  syn keyword htmlTagName contained source figcaption

  " HTML 5 arguments
  syn keyword htmlArg contained autofocus autocomplete placeholder min max step
  syn keyword htmlArg contained contenteditable contextmenu draggable hidden item
  syn keyword htmlArg contained itemprop list sandbox subject spellcheck
  syn keyword htmlArg contained novalidate seamless pattern formtarget manifest
  syn keyword htmlArg contained formaction formenctype formmethod formnovalidate
  syn keyword htmlArg contained sizes scoped async reversed sandbox srcdoc
  syn keyword htmlArg contained hidden role
  syn match   htmlArg "\<\(aria-[\-a-zA-Z0-9_]\+\)=" contained
  syn match   htmlArg contained "\s*data-[-a-zA-Z0-9_]\+"

  " new CSS features
  syn keyword cssTransformProp contained rotate




  "" experiment

  " Set a nicer foldtext function
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
    " let sub = sub . "                                                                                                                  "
    let sub = "                                                                                                                  "
    let num_w = getwinvar( 0, '&number' ) * getwinvar( 0, '&numberwidth' )
    let fold_w = getwinvar( 0, '&foldcolumn' )
    let sub = strpart( sub, 0, winwidth(0) - strlen( info ) - num_w - fold_w - 1 )
    let subin = strpart( sub, 0, n - 1 )
    " return sub . info
    return repeat(" ", l:ind) . "|" . repeat(" ", l:ind2-2) . "» " . info . " «"
  endfunction



  " tab omnicomplete

  function! SmartTab_Complete()
    if (pumvisible())
      return "\<C-N>"
    endif

    let line = getline('.')
    let substr = strpart(line, -1, col('.'))

    let is_dot = match(substr, '\.$') > -1
    let is_word = match(substr, '\w$') > -1

    if (is_dot)
      return "\<C-X>\<C-O>\<C-N>"  " omni
    elseif (is_word)
      return "\<C-N>"  " word
    else
      return "\<tab>"
    endif
  endfunction

  inoremap <tab> <c-r>=SmartTab_Complete()<CR>
  inoremap <s-tab> <c-x><c-o>






  " experiment to replace easymotion
  
  " set relativenumber

  map <c-k> 8k
  map <c-j> 8j
  " map <c-n> 3j
  " map <c-m> 3k



  " these lines must be after syntax highlighting is set up
  call jspretmpl#register_tag('css', 'css')
  autocmd! FileType javascript JsPreTmpl




  " show syntax stack of character under cursor
  " map <leader>hc :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
  " \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
  " \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

  fu! SynStack()
    for id in synstack(line("."), col("."))
       echo synIDattr(id, "name")
    endfor
  endfu

  map <leader>hc :call SynStack()<cr>



"```````````````````````````````````````````````````````````````````````````````
" Lua Creature Comforts

  function! InsideString()
    let syn = CursorSyntax()
    return syn =~? 'luaString' ? 1 : 0
  endfunction

  function! InsideComment()
    let syn = CursorSyntax()
    return syn =~? 'luaComment' ? 1 : 0
  endfunction

  function! Smart_Underscore()
    let line = getline('.')
    let substr = strpart(line, -1, col('.'))
    let force_hyphen = InsideString() || InsideComment()
    let is_word = match(substr, '\w$') > -1
    if (force_hyphen || !is_word)
      return "-"
    else
      return "_"
    end
  endfunction

  au BufNewFile,BufReadPost *.lua inoremap - <c-r>=Smart_Underscore()<CR>


"```````````````````````````````````````````````````````````````````````````````
" C++ Creature Comforts

  " function! Smart_Arrow()
  "   let c = CurChar()
  "   return match(c, '\w') > -1 ? '->' : '>'
  " endfunction

  " au BufNewFile,BufReadPost *.cc inoremap > <c-r>=Smart_Arrow()<CR>

  function! IfInWord(then_, else_)
    let line = getline('.')
    let substr = strpart(line, -1, col('.'))
    let is_word = match(substr, '\w$') > -1
    return is_word ? a:then_ : a:else_
  endfunction

  function! Cpp_Comforts()
    iabbr inc< #include ><left>
    iabbr inc" #include "<left>

    inoremap - <c-r>=IfInWord("_", "-")<CR>
    inoremap _ <c-r>=IfInWord("-", "_")<CR>

    inoremap .. ->
    inoremap ;; ::
  endfunction

  au BufNewFile,BufReadPost *.cc,*.hh,*.cpp,*.h call Cpp_Comforts()


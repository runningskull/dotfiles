set nocompatible
let mapleader = "," 
filetype plugin on



"``````````````````````````````````````````````````````````````````````````````
" vim-plug 

  call plug#begin('~/.vim/plugged')

  " Rougly in order of most life-changing
  Plug 'kien/ctrlp.vim'
  Plug 'Lokaltog/vim-easymotion'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-surround'
  Plug 'bling/vim-airline'
  Plug 'rking/ag.vim'
  Plug 'tpope/vim-dispatch'

  " Test-driving
  Plug 'Raimondi/delimitMate'
  Plug 'tpope/vim-obsession'
  Plug 'tpope/vim-vinegar'
  Plug 'mattn/webapi-vim'
  Plug 'jtratner/vim-flavored-markdown'
  Plug 'csscomb/vim-csscomb'
  Plug 'ciaranm/detectindent'
  Plug 'michaeljsmith/vim-indent-object'
  Plug 'wesQ3/vim-windowswap'

  " Colors
  Plug 'runningskull/vim-colors-solarized'
  Plug 'reedes/vim-colors-pencil'
  Plug 'w0ng/vim-hybrid'

  " Language Runtimes
  Plug 'pangloss/vim-javascript'
  Plug 'tpope/vim-markdown'
  Plug 'jnwhiteh/vim-golang'
  Plug 'digitaltoad/vim-jade'
  Plug 'groenewege/vim-less'
  Plug 'kchmck/vim-coffee-script'
  Plug 'vim-scripts/glsl.vim'
  Plug 'runningskull/vim-mustache-handlebars'
  Plug 'jnwhiteh/vim-golang'
  Plug 'guns/vim-clojure-static'
  Plug 'vim-scripts/django.vim'
  " Plug 'cakebaker/scss-syntax.vim'
  Plug 'vim-scripts/lua.vim'
  Plug 'xolox/vim-misc'
  Plug 'ingydotnet/yaml-vim'
  Plug 'hail2u/vim-css3-syntax'

  call plug#end()
  



"``````````````````````````````````````````````````````````````````````````````
" Experiments 

  " ConqueTerm stuff
  nmap <leader>x vip<leader>x
  vmap <leader>x <f9><cr>

  " Shift Width
  noremap <leader>2 :set shiftwidth=2<cr>
  noremap <leader>4 :set shiftwidth=4<cr>

  " delimitMate nice behavior on newlines
  let delimitMate_expand_cr = 1

  " File browsing sidebar
  command! Files Vex | silent vertical resize 30 | set winfixwidth

  " Open a scratch buffer
  map <leader><leader>s :vsp<cr> <c-l>:e /tmp/scratch-<c-r>=strftime("%Y%m%d%H%M")<cr><cr>i

  " Airline
  " let g:airline_powerline_fonts = 1
  let g:airline_left_sep = ' '
  let g:airline_right_sep = ' '

  " Use system clipboard
  set clipboard=unnamed

  " Sane regex searching
  map <leader>/ /\v 

  " Ultisnips
  let g:UltiSnipsExpandTrigger="<c-l>"
  let g:UltiSnipsJumpForwardTrigger="<tab>"
  let g:UltiSnipsJumpBackwardTrigger="<c-tab>"

  " Shortcuts
  map <leader>w <c-w>
  map <leader>js :set syntax=javascript<cr>

  set noequalalways

  "" let g:airline#extensions#whitespace#enabled|show_message=0

  " Lua plugin is too zealous
  let g:lua_complete_dynamic=0

  " syntax checking is handy, but there's a but that
  " causes random lines to turn red, even after error
  " is fixed. could remove this if that gets resolved.
  let g:lua_check_syntax=0

  " CSS uses hyphens & stuff
  autocmd FileType css,scss set iskeyword=@,48-57,_,-,?,!,192-255

  " custom file extensions
  autocmd BufNewFile,BufRead *.cfg set filetype=yaml
  autocmd BufNewFile,BufRead *.hhtml set filetype=html.handlebars
  autocmd BufNewFile,BufRead *.cljx set filetype=clojure

  let g:mustache_operators = 1

  map <leader>cs mhva{:CSScomb<cr>'h


  "```````````````
  " Projects
  
  " source ~/.vim/rafflecopter.vim





"``````````````````````````````````````````````````````````````````````````````
" UI Options

  " mouse in the terminal
  set mouse=a
  if has('mouse_sgr')
    set ttymouse=sgr
  endif

  " No beeping
  set vb

  " paste in the terminal
  nnoremap <leader>p :set invpaste paste?<cr>i
  set pastetoggle=<leader>p
  set showmode

  " we don't need no steeenking scrollbars
  set guioptions-=r
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L

  " such font.     so letters. wow
  set guifont=Source\ Code\ Pro\ for\ Powerline:h12

  " Status Bar
  set statusline=%F%m%r%h%w\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
  set laststatus=2

  " Powerline status bar
  " let g:Powerline_stl_path_style="short"
  " let g:Powerline_symbols = 'fancy'

  " Buffer space top/bottom
  set scrolloff=3

  " Netrw tree view
  " let g:netrw_liststyle=3
  " let g:netrw_browse_split=4
  " let g:netrw_preview=1
  " let g:netrw_winsize=30

  " iTerm insert mode cursor
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

  function! Solarized()
    " set background=light
    set background=dark
    let g:solarized_termcolors=256
    let g:solarized_contrast="high"
    let g:solarized_visibility="high"
    colo solarized
  endfunction

  function! Pencil()
    set bg=light
    colo pencil
  endfunction

  function! Smyck()
    set bg=dark
    colo smyck
  endfunction

  " call Solarized()
  call Smyck()

  command! SOL let g:solarized_termcolors=16 | colo solarized | set bg=dark
  command! PEN set bg=light | colo pencil
  command! SMY set bg=dark | colo smyck

  command! LIGHT set bg=light
  command! DARK set bg=dark




"``````````````````````````````````````````````````````````````````````````````
" Vim Core

  " Fix for delay exiting insert mode in terminal (iTerm only?)
  set ttimeoutlen=100

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

  " Sometimes syntax highlighting breaks. Fix it.
  command! SSS syntax sync fromstart




"``````````````````````````````````````````````````````````````````````````````
" In-buffer Editing

  " make backspace delete everything
  set bs=2

  " Indenting
  set autoindent
  "set smartindent
  set expandtab
  set sts=2
  set ts=2
  set sw=2

  command! TABS set noexpandtab|set sts=0|set ts=4|set sw=0

  " Detect tabs or spaces
  set smarttab

  " Line numbers
  " set nu
  " set numberwidth=5
  set nonu
  set foldcolumn=1

  " Line wrapping is for wimps :)
  set nowrap

  " remap native vim autocomplete shortcut
  inoremap <c-\> <c-x><c-n><down>

  inoremap <C-Space> <C-x><C-o><down>
  inoremap <C-@> <C-Space><down>

  " For navigating context menus w/o using arrows
  inoremap <c-j> <down>
  inoremap <c-k> <up>

  " Searching is a fast way to navigate
  set ignorecase
  set smartcase
  set incsearch
  set nohls

  " Copy to system clipboard
  noremap <leader>ca mygg"*yG'y
  vnoremap <leader>cc "*y




"``````````````````````````````````````````````````````````````````````````````
" Folding

  " Indent folding is the KISS solution
  set foldmethod=indent

  " whatever
  set foldnestmax=12

  " quick fold/unfold
  noremap <leader><space> za

  " fold/unfold recursively
  noremap <leader>ff zA

  " set foldlevel to a particular level
  noremap <leader>fs :set foldlevel=

  " jump to the next/previous fold and toggle it
  noremap <leader>fj zjza
  noremap <leader>fk zkza

  set fillchars="fold: "




"``````````````````````````````````````````````````````````````````````````````
" File/Buffer/Window Navigation

  " Quickly list open buffers (obsoleted by ctrlp plugin)
  "noremap \ :ls<cr>:b<space>

  " Quickly switch between 2 buffers (cancels ZZ to close)
  noremap <leader>z :b#<cr>

  " ctrl-p is all that and a bag of chips
  noremap <c-\> :CtrlPMRU<cr>
  let g:ctrlp_by_filename = 0

  " The way I like command-mode autocomplete to work. #ymmv
  set completeopt=longest,menuone
  set wildmode=list:longest

  " Ignore certain files in autocomplete
  set wildignore+=*.png,*.gif,*.jpg,*.psd
  set wildignore+=*.pyc,.git
  set wildignore+=*.swp,.DS_Store
  set wildignore+=*.scssc
  set wildignore+=*/node_modules/*
  set wildignore+=*/bower_components/*

  " Switch windows easily.
  noremap <c-h> <c-w>h
  noremap <c-l> <c-w>l
  noremap <c-j> <c-w>j
  noremap <c-k> <c-w>k

  " I like Ack (http://www.vim.org/scripts/script.php?script_id=2572)
  " noremap <leader>s :Ack<space>

  " Using silver searcher instead
  noremap <leader>s :Ag!<space>

  " navigate quickfix list
  noremap <leader>ee :cc
  noremap <leader>er :cn<cr>

  " Make marks a little easier
  nnoremap ' `
  nnoremap ` '

  " Ignore built files
  let g:ctrlp_custom_ignore = {
  \ 'dir':  'dist$\|tmp$'
  \ }

  let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

  " quicker suspend
  nnoremap Z <c-z>




"``````````````````````````````````````````````````````````````````````````````
" Specific Languages/Filetypes

  " Wrap lines. Useful for editing prose (blog post, email, comments, etc...)
  command! WRAP set wrap|set formatoptions=l|set lbr|map j gj|map k gk

  " Close HTML Tags
  inoremap /<tab> </<c-x><c-o><esc>a

  " JS/CS 'norm' is 2-space indents
  au BufNewFile,BufReadPost *.js,*.coffee,*.jade setl shiftwidth=2

  " GLSL
  au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setf glsl 

  " Django Templates
  au BufNewFile,BufRead *.html command! HTML setfiletype htmldjango




"``````````````````````````````````````````````````````````````````````````````
" Stuff that's 1/2 effective - maybe find a plugin?

  " Util for aligning things w/ colons (like CSS properties or JS object fields)
  noremap <leader>m 0f:wi<tab><esc>
  vnoremap <leader>m :norm ,m<cr>



" vim: set fdm=indent ts=2 sw=2:

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






"" experiment

" Set a nicer foldtext function
set foldtext=MyFoldText()
function! MyFoldText()
  let line = getline(v:foldstart)
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
  let info = " " . n . " lines"
  let sub = sub . "                                                                                                                  "
  let num_w = getwinvar( 0, '&number' ) * getwinvar( 0, '&numberwidth' )
  let fold_w = getwinvar( 0, '&foldcolumn' )
  let sub = strpart( sub, 0, winwidth(0) - strlen( info ) - num_w - fold_w - 1 )
  " return sub . info
  return info
endfunction

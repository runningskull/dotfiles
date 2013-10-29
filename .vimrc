set nocompatible
let mapleader = "," 
filetype plugin on



"``````````````````````````````````````````````````````````````````````````````
" Vundle 

  " Required boilerplate
  filetype off
  set rtp+=~/.vim/bundle/vundle/
  call vundle#rc()
  Bundle 'gmarik/vundle'

  " Rougly in order of most life-changing
  Bundle 'kien/ctrlp.vim'
  "Bundle 'scrooloose/nerdcommenter'
  Bundle 'Lokaltog/vim-easymotion'
  Bundle 'tpope/vim-fugitive'
  Bundle 'tpope/vim-commentary'
  Bundle 'mileszs/ack.vim'
  Bundle 'tpope/vim-surround'
  Bundle 'rson/vim-conque'
  Bundle 'Lokaltog/vim-powerline'

  " Test-driving
  Bundle 'Raimondi/delimitMate'

  " Colors
  Bundle 'runningskull/vim-colors-solarized'
  Bundle 'chriskempson/base16-vim'

  " Language Runtimes
  Bundle 'tpope/vim-markdown'
  Bundle 'jnwhiteh/vim-golang'
  Bundle 'digitaltoad/vim-jade'
  Bundle 'groenewege/vim-less'
  Bundle 'kchmck/vim-coffee-script'
  Bundle 'vim-scripts/glsl.vim'
  Bundle 'juvenn/mustache.vim'
  Bundle 'jnwhiteh/vim-golang'

  filetype plugin on
  


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
  nnoremap <F2> :set invpaste paste?<cr>
  set pastetoggle=<F2>
  set showmode

  " we don't need no steeenking scrollbars
  set guioptions-=r
  set guioptions-=R
  set guioptions-=l
  set guioptions-=L

  " experiment w/ fonts
  set guifont=Source\ Code\ Pro\ for\ Powerline:h12
  "set guifont=Source\ Code\ Pro:h12
  "set guifont=Consolas:h13
  "set guifont=Menlo:h12
  "set guifont=Monaco:h13

  " Colors like whoa!
  syn on
  set t_Co=256
  set background=dark
  let g:solarized_termcolors=256
  command! TERM let g:solarized_termcolors=16 | set bg=dark | colo solarized
  let g:solarized_contrast="high"
  let g:solarized_visibility="high"
  colo solarized
  "colo peaksea

  "let base16colorspace=256  " Access colors present in 256 colorspace"
  "colo base16-railscasts

  " Status Bar
  set statusline=%F%m%r%h%w\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
  set laststatus=2

  " Powerline status bar
  let g:Powerline_stl_path_style="short"
  let g:Powerline_symbols = 'fancy'

  " Buffer space top/bottom
  set scrolloff=3

  " iTerm insert mode cursor
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"

  " Netrw tree view
  let g:netrw_liststyle=3
  let g:netrw_browse_split=4
  let g:netrw_preview=1
  let g:netrw_winsize=30




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
  set sts=4
  set ts=4
  set sw=4

  " Detect tabs or spaces
  set smarttab

  " Line numbers
  set nu
  set numberwidth=5

  " Line wrapping is for wimps :)
  set nowrap

  " remap native vim autocomplete shortcut
  inoremap <c-\> <c-x><c-n><down>

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




"``````````````````````````````````````````````````````````````````````````````
" File/Buffer/Window Navigation

  " Quickly list open buffers (obsoleted by ctrlp plugin)
  "noremap \ :ls<cr>:b<space>

  " Quickly switch between 2 buffers (cancels ZZ to close)
  noremap Z :b#<cr>

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
  noremap <leader>s :Ack<space>

  " navigate quickfix list
  noremap <leader>ee :cc
  noremap <leader>er :cn<cr>

  " Make marks a little easier
  nnoremap ' `
  nnoremap ` '

  " Ignore built files
  let g:ctrlp_custom_ignore = {
  \ 'dir':  'dist$',
  \ }

  " quicker suspend
  nnoremap <leader>z <c-z>




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




"``````````````````````````````````````````````````````````````````````````````
" Stuff that's 1/2 effective - maybe find a plugin?

  " Util for aligning things w/ colons (like CSS properties or JS object fields)
  noremap <leader>m 0f:wi<tab><esc>
  vnoremap <leader>m :norm ,m<cr>



" vim: set fdm=indent ts=2 sw=2:

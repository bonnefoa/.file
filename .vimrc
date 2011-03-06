colorscheme desert

filetype off 
call pathogen#runtime_append_all_bundles()
filetype plugin on
call pathogen#helptags()

let mapleader = ","
let g:mapleader = ","

nmap <silent> <leader>s <Esc>:tabnew<CR><bar>:AckFromSearch<CR>

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" empty search
nmap <silent> ,/ :nohlsearch<CR>

map <F10> <Esc>:tabnew<CR> 

" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible
" Attempt to determine the type of a file based on its name and possibly its
" contents.  Use this to allow intelligent auto-indenting for each filetype
" and for plugins that are filetype specific.
filetype indent on
" Enable syntax highlighting
syntax on
" One of the most important options to activate. Allows you to switch from an
" unsaved buffer without saving it first. Also allows you to keep an undo
" history for multiple files. Vim will complain if you try to quit without
" saving, and swap files will keep you safe if your computer crashes.
set hidden
"set autochdir
set showmatch
set autowrite 

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Better command-line completion
set wildmenu

" Show partial commands in the last line of the screen
set showcmd

" Highlight searches (use <C-L> to temporarily turn off highlighting; see the
" mapping of <C-L> below)
set hlsearch
" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase
" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start

" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
" set autoindent

" Stop certain movements from always going to the first character of a line.
" While this behaviour deviates from that of Vi, it does what most users
" coming from other editors would expect.
set nostartofline

" Display the cursor position on the last line of the screen or in the status
" line of a window
set ruler

" Always display the status line, even if only one window is displayed
set laststatus=2

" Instead of failing a command because of unsaved changes, instead raise a
" dialogue asking if you wish to save changed files.
set confirm

" Use visual bell instead of beeping when doing something wrong
set visualbell

" And reset the terminal code for the visual bell.  If visualbell is set, and
" this line is also included, vim will neither flash nor beep.  If visualbell
" is unset, this does nothing.
set t_vb=

" Enable use of the mouse for all modes
set mouse=a

" Set the command window height to 2 lines, to avoid many cases of having to
" "press <Enter> to continue"
set cmdheight=2

" Display line numbers on the left
set number

" Quickly time out on keycodes, but never time out on mappings
set notimeout ttimeout ttimeoutlen=200

" Use <F11> to toggle between 'paste' and 'nopaste'
set pastetoggle=<F11>

" Indentation settings for using 2 spaces instead of tabs.
" Do not change 'tabstop' from its default value of 8 with this setup.
set shiftwidth=2
set softtabstop=2
set expandtab

let g:haddock_browser = "/usr/bin/firefox"

"completeopt=menu,menuone,longest

set tags=TAGS;$HOME


" Automatically generate tags 
"au BufWritePost *.hs,*.lhs !hasktgs -R 

function! UPDATE_TAGS()
  let _f_ = expand("%:p")
  let _cmd_ = 'find . -name "*.hs" | xargs hasktags' 
  let _sed_ = "sed -i \'/^(/d\' TAGS"
  let _list = system(_cmd_)
  let _list2 = system(_sed_)
  unlet _f_
  unlet _cmd_
  unlet _sed_ 
  unlet _list
  unlet _list2
endfunction

autocmd BufWrite *.hs call UPDATE_TAGS()


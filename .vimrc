
filetype off 
call pathogen#runtime_append_all_bundles()
filetype plugin on
call pathogen#helptags()

" Enable syntax highlighting
"let g:solarized_termcolors=256
syntax enable
colorscheme desert
set background=dark

let mapleader = ","
let g:mapleader = ","

let NERDTreeIgnore=[ '\.cmi$' ,'\.cmx$' ,'\.o$' ,'\.annot$' ,'\~$']

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" empty search
nmap <silent> ,/ :nohlsearch<CR>

nmap <silent> <leader>s <Esc>:tabnew<CR><bar>:AckFromSearch<CR>
nnoremap <F6> viw"+p
vnoremap <F6> "+p

map <F5> "qdt,"sx"wdib"wP"sp"qp

map <F10> <Esc>:tabnew<CR> 

set wildmode=longest,list,full
set wildmenu

" Set 'nocompatible' to ward off unexpected things that your distro might
" have made, as well as sanely reset options when re-sourcing .vimrc
set nocompatible
" Attempt to determine the type of a file based on its name and possibly its
" contents.  Use this to allow intelligent auto-indenting for each filetype
" and for plugins that are filetype specific.
filetype indent on
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
"set mouse=a

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
set smartindent
set tabstop=2
set shiftwidth=2
set expandtab

let g:haddock_browser = "/usr/bin/firefox"
let g:ackprg="ack-grep -r -H --nocolor --nogroup --column"

filetype plugin indent on

let g:diffed_buffers=[]
function DiffText(a, b, diffed_buffers)
    enew
    setlocal buftype=nowrite
    call add(a:diffed_buffers, bufnr('%'))
    call setline(1, split(a:a, "\n"))
    diffthis
    vnew
    setlocal buftype=nowrite
    call add(a:diffed_buffers, bufnr('%'))
    call setline(1, split(a:b, "\n"))
    diffthis
endfunction
function WipeOutDiffs(diffed_buffers)
    for buffer in a:diffed_buffers
        execute 'bwipeout! '.buffer
    endfor
endfunction
" diff between buffers, use "ya and "yb
nnoremap <special> <F7> :call DiffText(@a, @b, g:diffed_buffers)<CR>
nnoremap <special> <F8> :call WipeOutDiffs(g:diffed_buffers)<CR>

set diffopt+=iwhite

" For folding 
inoremap <F9> <C-O>za
nnoremap <F9> za
onoremap <F9> <C-C>za
vnoremap <F9> zf

set foldmethod=manual

set tags=TAGS;$HOME

" Automatically generate tags
"au BufWritePost *.hs,*.lhs !hasktgs -R

function! UPDATE_TAGS_OCAML()
  let _f_ = expand("%:p")
  let _cmd_ = 'find . -name "*.ml" | xargs ctags58 -f tags_ml'
  let _cmd_js_ = 'find . -name "*.js" | xargs ctags58 -f tags_js'
  let _list = system(_cmd_)
  let _list2 = system(_cmd_js_)
  unlet _f_
  unlet _cmd_
  unlet _list
  unlet _list2
endfunction

func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc

"autocmd BufWrite *.ml call UPDATE_TAGS_OCAML()
autocmd BufWrite *.erl call UPDATE_TAGS('find . -name "*.erl" -o -name "*.hrl" | xargs etags')
autocmd BufWrite *.* :call DeleteTrailingWS()
set tabpagemax=200

set expandtab

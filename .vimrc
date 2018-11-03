set nocompatible
filetype off

set exrc
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

Bundle 'gmarik/vundle'
Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'
Bundle "lepture/vim-jinja"
Bundle "Glench/Vim-Jinja2-Syntax"
Bundle 'duff/vim-scratch.git'
Bundle 'godlygeek/tabular.git'
Bundle 'ctrlpvim/ctrlp.vim.git'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/syntastic.git'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git.git'
Bundle 'tpope/vim-repeat.git'
Bundle 'tpope/vim-surround.git'
Bundle 'Valloric/YouCompleteMe.git'
Bundle 'vim-scripts/matchit.zip'
"Bundle 'vim-scripts/swap-parameters.git'
"Bundle 'vim-scripts/The-NERD-tree.git'
Bundle 'kana/vim-altr'
Bundle 'tpope/vim-dispatch.git'
Bundle 'altercation/vim-colors-solarized'
Bundle 'sjl/gundo.vim'
Bundle 'rking/ag.vim'
Bundle 'rhysd/vim-clang-format'
Bundle 'hynek/vim-python-pep8-indent'
Bundle 'tpope/vim-abolish'
"Bundle 'tpope/vim-unimpaired.git'
Bundle 'ludovicchabant/vim-gutentags'
Bundle 'majutsushi/tagbar'
Bundle 'fatih/vim-go'
Bundle 'ngmy/vim-rubocop'
Bundle 'JamshedVesuna/vim-markdown-preview'
Bundle 'joeyespo/grip'
Bundle 'hashivim/vim-terraform'
Bundle "motus/pig.vim"
Bundle "jceb/vim-orgmode.git"
Bundle 'chase/vim-ansible-yaml'
Bundle 'vim-scripts/mako.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'junegunn/fzf'
Bundle 'junegunn/fzf.vim'

call vundle#end()

filetype plugin indent on

set spelllang=en
set diffopt+=iwhite
set makeprg=make\ -j9
map <F9> :GoBuild<CR>
set errorformat^=%-GMakefile:%l:\ recipe\ for\ target\ %.%#

syntax enable
syntax on
set background=dark
colorscheme solarized

let mapleader = ","
let g:mapleader = ","
let maplocalleader = ','
let g:maplocalleader = ','

let g:syntastic_go_checkers = ['govet', 'errcheck', 'go', 'golint']

let g:syntastic_python_checkers=['flake8']
let g:syntastic_ruby_checkers=['mri', 'rubocop']
let g:syntastic_python_flake8_args='--ignore=E501,E225'
let g:syntastic_always_populate_loc_list = 1
let g:clang_format#auto_formatexpr = 1
"let g:clang_format#auto_format = 1
let vim_markdown_preview_github=1
let vim_markdown_preview_hotkey='<C-m>'
let vim_markdown_preview_browser='Google Chrome'

let NERDTreeIgnore=[ '\.cmi$' ,'\.cmx$' ,'\.o$' ,'\.annot$' ,'\~$', '\.class', 'target', '\.d']
set wildignore+=*.o,*.obj,.git,dist,deps,logs,*.pyc,**/target/**,*.d,docs,*.o,autom4te.cache
set wildignore+=build-aux,doxydoc,*.la,*.sign,*.pub,*.trs,*.Po,*.pyo
set wildignore+=**/eggs/**,**/parts/**,*.log,*.vdproj,*.src,*.sln,*.ri
set wildignore+=*_check

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>


let g:UltiSnipsExpandTrigger="<leader>s"
" empty search
nmap <silent> <leader>/ :nohlsearch<CR>

nmap <leader>a  <Plug>(altr-forward)

"nmap <leader>b :CtrlPBuffer<cr>
nmap <leader>B :CtrlPMRUFiles<cr>
nmap <leader>t :CtrlPTag<cr>

nnoremap <silent> <expr> <Leader><Leader> (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <Leader>l  :Lines<CR>
nnoremap <silent> <Leader>S  :AgFromSearch <CR>
nnoremap <silent> <Leader>a  :Ag <CR>
nnoremap <silent> <Leader>b  :Buffers<CR>
nnoremap <silent> <Leader>`  :Marks<CR>

nmap <leader>sw :call SwapParams("forwards")<cr>
nmap <leader>sb :call SwapParams("backwards")<cr>

nmap <leader>] :cn<cr>
nmap <leader>[ :cp<cr>
nmap <leader>g :YcmCompleter GoToDefinition<cr>
nmap <F8> :TagbarToggle<CR>

map <F5> "qdt,"sx"wdib"wP"sp"qp

set lazyredraw          " redraw only when we need to.
set ttyfast

autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

set incsearch
inoremap jj <ESC>

set wildmode=longest,list,full
set wildmenu

set hidden
set showmatch
set autowrite

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Show partial commands in the last line of the screen
set showcmd
set cursorline

" Highlight searches (use <C-L> to temporarily turn off highlighting; see the
" mapping of <C-L> below)
set hlsearch
" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase
" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start
set nostartofline
set ruler
set laststatus=2
set confirm
set visualbell

" And reset the terminal code for the visual bell.  If visualbell is set, and
" this line is also included, vim will neither flash nor beep.  If visualbell
" is unset, this does nothing.
set t_vb=
set cmdheight=2

" Quickly time out on keycodes, but never time out on mappings
set notimeout ttimeout ttimeoutlen=200

" Use <F11> to toggle between 'paste' and 'nopaste'
set pastetoggle=<F10>

" Indentation settings for using 2 spaces instead of tabs.
" Do not change 'tabstop' from its default value of 8 with this setup.
set smartindent
set tabstop=4
set shiftwidth=4
"set expandtab

" fold
set foldenable
set foldmethod=indent
set foldlevelstart=10
set foldnestmax=10

" ctrlp
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

" ag
"let g:ag_working_path_mode="r"
let g:gutentags_dont_load=0

" gutentags
let g:gutentags_ctags_exclude = ['*.sql', 'parts', 'eggs', 'build', 'node_modules']

func! DeleteTrailingWS()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc

autocmd BufWrite *.* call DeleteTrailingWS()
set tabpagemax=200

set completeopt=menuone,longest,preview

" Unfold all
nnoremap <space> za
nnoremap <leader>u :GundoToggle<CR>

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

"Backups {{{

set backupdir=~/.vim/tmp/backup/
set directory=~/.vim/tmp/swap/
set backup

" }}}
"

highlight ExtraWhitespace ctermbg=red guibg=red
"match ExtraWhitespace /\t/

let $GROFF_NO_SGR=1
fun! ReadMan()
  " Assign current word under cursor to a script variable:
  let s:man_word = expand('<cword>')
  " Open a new window:
  :exe ":Sscratch"
  " delete everything
  :exe ":1,$d"
  " Read in the manpage for man_word (col -b is for formatting):
  :exe ":r!man 3 " . s:man_word . " | col -b"
  " Goto first line...
  :exe ":goto"
  " and delete it:
  :exe ":delete"
  " finally set file type to 'man':
  :exe ":set filetype=man"
endfun
" Map the K key to the ReadMan function:
map K :call ReadMan()<CR>

set cino=N-sg0
set relativenumber

let g:ycm_key_list_select_completion = ['<Down>']
let g:ycm_confirm_extra_conf=0
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 0
let g:ycm_always_populate_location_list = 1
let g:ycm_python_binary_path = 'python'


let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

command! -nargs=? -range Dec2hex call s:Dec2hex(<line1>, <line2>, '<args>')
function! s:Dec2hex(line1, line2, arg) range
  if empty(a:arg)
    if histget(':', -1) =~# "^'<,'>" && visualmode() !=# 'V'
      let cmd = 's/\%V\<\d\+\>/\=printf("0x%x",submatch(0)+0)/g'
    else
      let cmd = 's/\<\d\+\>/\=printf("0x%x",submatch(0)+0)/g'
    endif
    try
      execute a:line1 . ',' . a:line2 . cmd
    catch
      echo 'Error: No decimal number found'
    endtry
  else
    echo printf('%x', a:arg + 0)
  endif
endfunction

command! -nargs=? -range Hex2dec call s:Hex2dec(<line1>, <line2>, '<args>')
function! s:Hex2dec(line1, line2, arg) range
  if empty(a:arg)
    if histget(':', -1) =~# "^'<,'>" && visualmode() !=# 'V'
      let cmd = 's/\%V0x\x\+/\=submatch(0)+0/g'
    else
      let cmd = 's/0x\x\+/\=submatch(0)+0/g'
    endif
    try
      execute a:line1 . ',' . a:line2 . cmd
    catch
      echo 'Error: No hex number starting "0x" found'
    endtry
  else
    echo (a:arg =~? '^0x') ? a:arg + 0 : ('0x'.a:arg) + 0
  endif
endfunction

command! -nargs=? -range Hex2chr call s:Hex2chr(<line1>, <line2>, '<args>')
function! s:Hex2chr(line1, line2, arg) range
  if empty(a:arg)
    if histget(':', -1) =~# "^'<,'>" && visualmode() !=# 'V'
      let cmd = 's/\%V0x\x\+/\=printf("' . " '%c'\", submatch(0)+0)/g"
    else
      let cmd = 's/0x\x\+/\=printf("' . " '%c'\", submatch(0)+0)/g"
    endif
    try
      execute a:line1 . ',' . a:line2 . cmd
    catch
      echo 'Error: No hex number starting "0x" found'
    endtry
  else
    echo (a:arg =~? '^0x') ? a:arg + 0 : ('0x'.a:arg) + 0
  endif
endfunction

:autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
:autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
:autocmd FileType json setlocal ts=2 sts=2 sw=2 expandtab

function! Multiply(cnt)
    " save register v
    let old_reg    = getreg("v")

    " select the number under the cursor
    call search('\d\([^0-9\.]\|$\)', 'cW')
    normal v
    call search('\(^\|[^0-9\.]\d\)', 'becW')

    " yank it into register v then reselect
    normal "vygv

    " change the selection with the yanked number multiplied by the count
    execute "normal c" . @v * a:cnt

    " restore register v
    call setreg("v", old_reg)
endfunction

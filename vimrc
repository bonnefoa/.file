set nocompatible
filetype off

set exrc
set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

Bundle 'fatih/vim-go'
Bundle 'gmarik/vundle'
Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'
Bundle 'godlygeek/tabular.git'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/syntastic.git'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-git.git'
Bundle 'tpope/vim-repeat.git'
Bundle 'tpope/vim-surround.git'
Bundle 'tpope/vim-dispatch.git'
Bundle 'Valloric/YouCompleteMe.git'
Bundle 'vim-scripts/matchit.zip'
Bundle 'altercation/vim-colors-solarized'
"Bundle 'hynek/vim-python-pep8-indent'
Bundle 'ludovicchabant/vim-gutentags'
Bundle 'junegunn/fzf'
Bundle 'junegunn/fzf.vim'
Bundle 'kburdett/vim-nuuid.git'
"Bundle 'dag/vim-fish'
"Bundle 'majutsushi/tagbar'
"Bundle 'ngmy/vim-rubocop'
"Bundle 'JamshedVesuna/vim-markdown-preview'
"Bundle 'hashivim/vim-terraform'
"Bundle 'vim-scripts/mako.vim'
"Bundle 'previm/previm'
Bundle 'wannesm/wmgraphviz.vim'
Bundle 'tex/vimpreviewpandoc'
Bundle 'ambv/black', {'rtp': 'vim'}
Bundle 'mtth/scratch.vim'
"Bundle 'jremmen/vim-ripgrep.git'

Bundle 'kana/vim-altr'
Bundle 'rhysd/vim-clang-format'
Bundle 'skywind3000/asyncrun.vim'
Bundle 'vhdirk/vim-cmake'

call vundle#end()

filetype plugin indent on

set spelllang=en
set diffopt+=iwhite

syntax enable
syntax on
set background=dark
colorscheme solarized

let mapleader = ','
let g:mapleader = ','
let maplocalleader = ','
let g:maplocalleader = ','

let g:syntastic_check_on_open = 1
let g:syntastic_lua_checkers = ["luac", "luacheck"]
let g:syntastic_lua_luacheck_args = "--no-unused-args"
let g:syntastic_vim_checkers = ['vint']
let g:syntastic_go_checkers = ['govet', 'errcheck', 'go', 'golint']
let g:syntastic_python_checkers=['flake8']
let g:syntastic_quiet_messages = { "type": "style" }
let g:syntastic_json_checkers=['jsonlint']
let g:syntastic_ruby_checkers=['mri', 'rubocop']
let g:syntastic_python_flake8_args='--ignore=E501,E225'
let g:syntastic_always_populate_loc_list = 1
let g:clang_format#auto_formatexpr = 1
let vim_markdown_preview_github=1
let vim_markdown_preview_hotkey='<C-m>'
let vim_markdown_preview_browser='Google Chrome'
let g:WMGraphviz_output = 'png'

set wildignore+=*.o,*.obj,.git,dist,deps,logs,*.pyc,**/target/**,*.d,docs,*.o,autom4te.cache
set wildignore+=build-aux,doxydoc,*.la,*.sign,*.pub,*.trs,*.Po,*.pyo
set wildignore+=**/eggs/**,**/parts/**,*.log,*.vdproj,*.src,*.sln,*.ri
set wildignore+=*_check

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>so :so $MYVIMRC<CR>

nmap <silent> <leader>a <Plug>(altr-forward)

let g:UltiSnipsExpandTrigger='<leader>s'
" empty search
nmap <silent> <leader>/ :nohlsearch<CR>

" Fzf bindings
nnoremap <silent> <Leader>l  :Rg<CR>
nnoremap <silent> <Leader>p  :Files<CR>
nnoremap <silent> <Leader>b  :Buffers<CR>
nnoremap <silent> <Leader>t  :Tags<CR>
nnoremap <silent> <Leader>`  :Marks<CR>
nnoremap <silent> <Leader>B  :History<CR>
nnoremap <silent> <Leader>S  :call RgFromSearch()<CR>

" RipGrep
function! RgFromSearch()
  let search =  getreg('/')
  " translate vim regular expression to perl regular expression.
  let search = substitute(search,'\(\\<\|\\>\)','\\b','g')
  exec 'Rg ' . search
endfunction

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

nmap <leader>] :cn<cr>
nmap <leader>[ :cp<cr>
nmap <leader>g :YcmCompleter GoToDefinition<cr>
nnoremap <leader>cd :cd %:p:h<CR>

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

set lazyredraw
set ttyfast
set incsearch
set wildmode=longest,list,full
set wildmenu
set hidden
set showmatch
set autowrite
set showcmd

set cursorline
set hlsearch
set ignorecase
set smartcase
set backspace=indent,eol,start
set nostartofline
set ruler
set laststatus=2
set confirm
set visualbell
set pastetoggle=<F10>
set cino=N-sg0
set relativenumber

" And reset the terminal code for the visual bell.  If visualbell is set, and
" this line is also included, vim will neither flash nor beep.  If visualbell
" is unset, this does nothing.
set t_vb=
set cmdheight=2

" Quickly time out on keycodes, but never time out on mappings
set notimeout ttimeout ttimeoutlen=200

" Indentation settings for using 2 spaces instead of tabs.
" Do not change 'tabstop' from its default value of 8 with this setup.
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

let g:gutentags_dont_load=0
let g:gutentags_ctags_exclude = ['*.sql', 'parts', 'eggs', 'build', 'node_modules']
let g:gutentags_exclude_project_root = ['/usr/local', '/home/sora', '/home/git-repos/cloudops']
let g:ycm_disable_for_files_larger_than_kb = 10000

func! DeleteTrailingWS()
    exe 'normal mz'
    %s/\s\+$//ge
    exe 'normal `z'
endfunc

autocmd BufWrite *.* call DeleteTrailingWS()
set tabpagemax=200

set backupdir=~/.vim/tmp/backup/
set directory=~/.vim/tmp/swap/
set backup

highlight ExtraWhitespace ctermbg=red guibg=red

let $GROFF_NO_SGR=1

fun! ReadMan()
  " Assign current word under cursor to a script variable:
  let s:man_word = expand('<cword>')
  " Open a new window:
  :exe ':Scratch'
  " delete everything
  :exe ':1,$d'
  " Read in the manpage for man_word (col -b is for formatting):
  :exe ':r!man 3 ' . s:man_word . ' | col -b'
  " Goto first line...
  :exe ':goto'
  " and delete it:
  :exe ':delete'
  " finally set file type to 'man':
  :exe ':set filetype=man'
endfun
" Map the K key to the ReadMan function:
map K :call ReadMan()<CR>

set completeopt=menuone,longest,preview
let g:ycm_key_list_select_completion = ['<Down>']
let g:ycm_confirm_extra_conf=0
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 0
let g:ycm_always_populate_location_list = 1
"let g:go_auto_type_info = 1
"let g:ycm_python_binary_path = 'python'
let g:ycm_gocode_binary_path = "$GOPATH/bin/gocode-gomod"
let g:ycm_godef_binary_path = "$GOPATH/bin/godef"

:autocmd BufRead *.cfg setlocal noexpandtab
:autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
:autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
:autocmd FileType json setlocal ts=2 sts=2 sw=2 expandtab

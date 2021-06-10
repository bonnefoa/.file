set nocompatible
filetype off

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'davidhalter/jedi-vim'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'fatih/vim-go'
Plug 'vim-airline/vim-airline'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'godlygeek/tabular'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'scrooloose/nerdcommenter'
"Plug 'scrooloose/syntastic'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'vim-scripts/matchit.zip'
Plug 'altercation/vim-colors-solarized'
"Plug 'ludovicchabant/vim-gutentags'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'wannesm/wmgraphviz.vim'
Plug 'ambv/black', {'rtp': 'vim'}
Plug 'mtth/scratch.vim'
Plug 'jceb/vim-orgmode'

Plug 'kana/vim-altr'
"Plug 'Chiel92/vim-autoformat'
"Plug 'rhysd/vim-clang-format'
"Plug 'dense-analysis/ale'
Plug 'vhdirk/vim-cmake'

call plug#end()

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

set wildignore+=*.o,*.obj,.git,dist,deps,logs,*.pyc,**/target/**,*.d,docs,*.o,autom4te.cache
set wildignore+=build-aux,doxydoc,*.la,*.sign,*.pub,*.trs,*.Po,*.pyo
set wildignore+=**/eggs/**,**/parts/**,*.log,*.vdproj,*.src,*.sln,*.ri
set wildignore+=*_check

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>so :so $MYVIMRC<CR>

nmap <silent> <leader>a <Plug>(altr-forward)

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

func! DeleteTrailingWS()
    exe 'normal mz'
    %s/\s\+$//ge
    exe 'normal `z'
endfunc

autocmd BufWrite *.* call DeleteTrailingWS()
set tabpagemax=200

set directory=~/.vim/tmp/swap/

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

let g:utl_cfg_hdl_scm_http_system = "silent !open '%u#%f'"
set foldlevelstart=20
let g:go_gopls_enabled = 1

:autocmd BufRead *.cfg setlocal noexpandtab
:autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
:autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
:autocmd FileType json setlocal ts=2 sts=2 sw=2 expandtab


" Coc Config
" Set internal encoding of vim, not needed on neovim, since coc.nvim using some
" unicode characters in the file autoload/float.vim
set encoding=utf-8

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" " Mappings for CoCList
" " Show all diagnostics.
" nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" " Manage extensions.
" nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" " Show commands.
" nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" " Find symbol of current document.
" nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" " Search workspace symbols.
" nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
" nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" " Do default action for previous item.
" nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" " Resume latest coc list.
" nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>


" " Use <C-l> for trigger snippet expand.
imap <leader>s <Plug>(coc-snippets-expand)
" " Use <C-j> for select text for visual placeholder of snippet.
" vmap <C-j> <Plug>(coc-snippets-select)
" " Use <C-j> for jump to next placeholder, it's default of coc.nvim
" let g:coc_snippet_next = '<c-j>'
" " Use <C-k> for jump to previous placeholder, it's default of coc.nvim
" let g:coc_snippet_prev = '<c-k>'
" " Use <C-j> for both expand and jump (make expand higher priority.)
" imap <C-j> <Plug>(coc-snippets-expand-jump)
" " Use <leader>x for convert visual selected code to snippet
" xmap <leader>x  <Plug>(coc-convert-snippet)


if has("win16") || has("win32") || has("win64")
    let g:iswindows=1
else
    let g:iswindows=0
endif

if g:iswindows
    "set nocompatible
    source $VIMRUNTIME/vimrc_example.vim
    source $VIMRUNTIME/mswin.vim
endif

"解决菜单乱码
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" fix vim starting in replace mode
" disable requesting cursor position
"set t_u7=
" ambiguous characters mode to double
set ambw=double

" Syntax
syntax on
filetype on
filetype plugin on
filetype indent on

" Indent
set autoindent
set cindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smarttab
set smartindent

" Edit & view
set backspace=2
"set backspace=indent,eol,start
set ruler
set showcmd
set showmode
set linebreak
set wildmenu
set number
"set relativenumber
set scrolloff=2
set history=1000
"set cursorline "highlight line
"set cursorcolumn "highlight colum
set laststatus=2
set cmdheight=2
set showmatch
set matchtime=2
"set mouse=a
"set whichwrap+=<,>,h,l   " allow backspase cursor move cross line
set splitright
set splitbelow


" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if g:iswindows
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

" Don't redraw while executing macros (good performance config)
set lazyredraw 

" No annoying sound on errors
set noerrorbells
set novisualbell
set belloff=all
set tm=500

" Properly disable sound on errors on MacVim
if has("gui_macvim")
    autocmd GUIEnter * set vb t_vb=
endif

" Search
set hlsearch
set incsearch
set magic
set sm
set iskeyword+=_,$,@,%,#,-
set ignorecase
set smartcase
set nowrapscan
"set noincsearch

" Code fold
set foldenable
set foldmethod=manual
set fo+=mB
set foldcolumn=0
set foldmethod=indent
set foldlevel=9999

" Prompts message
set shortmess=atI
set confirm

" Encoding & font
"set guifont=DejaVu_Sans_Mono:h10.5:cANSI
"set guifont=Consolas:h10.5:cANSI
"set guifont=Bront:h10.5:cANSI
"set guifont=Code_New_Roman:h10.5:cANSI
"set guifont=Courier_New:h10.5:cANSI
"set guifont=Consolas_Italic:h10.5:cANSI
"set guifont=BPmono:h10.5:cANSI
"set guifont=Lucida_Console:h10.5:cANSI
"set guifont=Lao_UI:h10.5:cANSI
"set guifont=FuraCode_Nerd_Font:h10.5:cANSI
"set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
"set termencoding=utf-8
"set encoding=utf-8
set encoding=utf-8
set fencs=utf-8,ucs-bom,shift-jis,gb18030,gbk,gb2312,cp936
set fileencodings=utf-8,ucs-bom,chinese
set langmenu=zh_CN.UTF-8
set helplang=cn
autocmd BufRead,BufNewFile,BufEnter * call ReloadCmds()

"解决consle输出乱码
"language messages zh_CN.utf-8

let $LANG='en' 
set langmenu=en

" GUI
set guioptions-=T " Hide tool bar
set guioptions-=m " Hide menu bar
"set go=          " no GUI button
set noeb
set mousemodel=popup
set selection=inclusive

" File
"set clipboard+=unnamed
set autoread
set autowrite
set nobackup
set noswapfile
set noundofile
set nowb

" Theme
"set t_Co=256
"set bg=light
"autocmd InsertLeave * se nocul  " light highlight line
"autocmd InsertEnter * se cul    " light highlight col

" Enable 256 colors palette in Gnome Terminal
"if $COLORTERM == 'gnome-terminal'
"    set t_Co=256
"endif

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    "set guioptions-=e
    set t_Co=256
    set cursorline "highlight line
    set guitablabel=%M%t
    set lines=49 columns=147
    se ghr=0

    "autocmd BufRead,BufNewFile,BufEnter *.cpp,*.cc,*.[ch],*.hh,*.hxx,*.hpp,*.sh,*.java,*.py,*.html exec "let &colorcolumn=80"
    "autocmd BufRead,BufNewFile,BufEnter *.txt,*.map,*.log,*.vim exec "let &colorcolumn=0"
"else
    "autocmd BufWritePost *.py,*.pyw,*.sh !chmod +x %
endif


" Use Unix as the standard file type
set ffs=unix,dos,mac

" Key map
command H %!xxd
command D %!xxd -r
command CD cd %:p:h
" 常规模式下输入 cs 清除行尾空格
nmap cs :%s/\s\+$//g<CR>:noh<CR>
" 常规模式下输入 ds 清除空行
nmap ds :g/^\s*$/d<CR>:noh<CR>
" 常规模式下输入 cM 清除行尾 ^M 符号
nmap cm :%s/\r$//g<CR>:noh<CR>
nmap cr @q
nmap cp "+p
" Remove the Windows ^M - when the encodings gets messed up
"noremap cm mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm
" expand Tab
nmap ct :%s/\t/    /g<CR>:noh<CR>
nmap tm :%s/\r$//g<CR>:%s/\n/\r/g<CR>:nohl<CR>
" Ctrl + K 插入模式下光标向上移动
"imap <c-k> <Up>
" Ctrl + J 插入模式下光标向下移动
"imap <c-j> <Down>
" Ctrl + H 插入模式下光标向左移动
"imap <c-h> <Left>
" Ctrl + L 插入模式下光标向右移动
"imap <c-l> <Right>
" Ctrl + f 光标跳转到行头
"imap <c-f> <ESC>0i
" Ctrl + e 光标跳转到行尾
"imap <c-e> <ESC>$i
nmap <space>    <c-d>
nmap <s-space>  <c-u>
" 常规模式下用空格键来开关光标行所在折叠（注：zR 展开所有折叠，zM 关闭所有折叠
"nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>
imap <c-v> <ESC>"+pa
vmap <c-v> "+p
vmap <c-c> "+y

nmap <c-s> :w<CR>


if has("mac") || has("macunix")
  nmap <D-j> <M-j>
  nmap <D-k> <M-k>
  vmap <D-j> <M-j>
  vmap <D-k> <M-k>
endif

" tag mgr
"set autochdir
map  <C-S-tab>  :tabprevious<CR>
map  <C-tab>    :tabnext<CR>
"map  <C-w>      :tabclose<CR>
map  <C-1>      :tabfirst<CR>
nmap <C-S-tab>  :tabprevious<CR>
nmap <C-tab>    :tabnext<CR>
nmap <C-t>      :browse tabnew<CR>
nmap <C-n>      :tabnew<CR>
imap <C-S-tab> <Esc>:tabprevious<CR>i
imap <C-tab> <Esc>:tabnext<CR>i
imap <C-n> <Esc>:tabnew<CR>
imap <C-t> <Esc>:browse tabnew<CR>

set viminfo+=!

let mapleader=','
" Fast saving
nmap <leader>w :w!<cr>
command W w !sudo tee % > /dev/null
nmap <leader>q @

"inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"    " <CR> Select chosen item
inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>
" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>
" Close the current buffer
map <leader>bd :Bclose<cr>:tabclose<cr>gT
" Close all the buffers
map <leader>ba :bufdo bd<cr>
map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" color_coded
let g:color_coded_enabled=0

set diffexpr=MyDiff()

" Function define
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      let cmd = '""' . $VIMRUNTIME . '\diff"'
      let eq = '"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction

" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
    let l:currentBufNum = bufnr("%")
    let l:alternateBufNum = bufnr("#")

    if buflisted(l:alternateBufNum)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == l:currentBufNum
        new
    endif

    if buflisted(l:currentBufNum)
        execute("bdelete! ".l:currentBufNum)
    endif
endfunction

function! CmdLine(str)
    call feedkeys(":" . a:str)
endfunction 

function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", "\\/.*'$^~[]")
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'gv'
        call CmdLine("Ack '" . l:pattern . "' " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

function! ReloadCmds()
    let g:my_colorcolumn_num=0
    if &diff
        set guifont=Consolas:h10:cANSI
    else
        call ReloadMyGlvars()
    endif
    let &colorcolumn = g:my_colorcolumn_num
endfunction

function! ReloadMyGlvars()
    if has("gui_running")
        if  &ft=='cpp'||&ft=='cc'||&ft=='c'||&ft=='h'||&ft=='hh'||&ft=='hxx'||&ft=='hpp'||&ft=='sh'||&ft=='java'||&ft=='python'||&ft=='py'||&ft=='html'
            let g:my_colorcolumn_num=80
        endif
        set guifont=
        set lsp=0
    else
        "set guifont=JetBrainsMono_NF:h10.5:cANSI
        set guifont=Code_New_Roman:h10.5:cANSI
        set lsp=0
    endif
endfunction


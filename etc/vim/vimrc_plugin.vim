" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
"call plug#begin('~/.vim/plugged')
"call plug#begin('~/.vim/plugged')
call plug#begin('$MYVIMPATH/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
"Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
"Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
"Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':'NERDTreeToggle' }
"Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Using a non-master branch
"Plug 'Valloric/YouCompleteMe', { 'frozen': 1, 'for':['c', 'cpp', 'python', 'java', 'html', 'sh', 'vb', 'dosbatch', 'make', 'vim'] }
if has("win64")
   Plug 'snakeleon/YouCompleteMe-x64', { 'for':['python', 'sh', 'make', 'vim', 'c'] }
elseif has("win32")
   Plug 'snakeleon/YouCompleteMe-x86', { 'for':['python', 'sh'] }
else
   Plug 'Valloric/YouCompleteMe', { 'for':['python', 'sh', 'make', 'vim', 'c'] }
endif
Plug 'nvie/vim-flake8'
"Plug 'davidhalter/jedi-vim', {'for': ['python']}
"Plug 'rip-rip/clang_complete', {'for': ['c', 'cpp']}
"Plug 'hynek/vim-python-pep8-indent', {'for': 'python'}
"Plug 'prabirshrestha/asyncomplete.vim', { 'branch': 'fuzzy' }

"Plug 'rdnetto/YCM-Generator', { 'branch':'stable' }

"Plug 'scrooloose/syntastic', {'for': ['c', 'cpp', 'python']}

"Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
"Plug 'neoclide/coc-git', {'do': 'yarn install --frozen-lockfile'}
"Plug 'neoclide/coc-html', {'do': 'yarn install --frozen-lockfile'}
"Plug 'neoclide/coc-json', {'do': 'yarn install --frozen-lockfile'}
"Plug 'neoclide/coc-python', {'do': 'yarn install --frozen-lockfile'}
"Plug 'clangd/coc-clangd', {'do': 'yarn install --frozen-lockfile'}
"Plug 'Maxattax97/coc-ccls', {'do': 'yarn install --frozen-lockfile'}
"Plug 'voldikss/coc-cmake', {'do': 'yarn install --frozen-lockfile'}
"Plug 'fannheyward/coc-markdownlint', {'do': 'yarn install --frozen-lockfile'}
"Plug 'fannheyward/coc-sql', {'do': 'yarn install --frozen-lockfile'}
"Plug 'josa42/coc-sh', {'do': 'yarn install --frozen-lockfile'}

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
"Plug 'fatih/vim-go', { 'tag':'*' }

" Plugin options
"Plug 'nsf/gocode', { 'tag':'v.20150303', 'rtp':'vim' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/seoul256.vim'

" Unmanaged plugin (manually installed and updated)
"Plug '~/my-prototype-plugin'

" Highlight several words in different colors simultaneously
Plug 'inkarkat/vim-ingo-library'
Plug 'inkarkat/vim-mark'

" Asynchronous Lint Engine
"Plug 'w0rp/ale'

" A Flexible Input Method Framework
"Plug 'fcitx/fcitx'

" Fuzzy search of filenames and paths in a programming project.
Plug 'shemerey/vim-peepopen'

" Text objects
"Plug 'kana/vim-textobj-user'
"Plug 'kana/vim-textobj-indent'
"Plug 'kana/vim-textobj-syntax'
"Plug 'kana/vim-textobj-function', { 'for':['c', 'cpp', 'vim', 'java'] }
"Plug 'sgur/vim-textobj-parameter'

Plug 'vim-scripts/taglist.vim'
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
"Plug 'vim-scripts/pylint.vim'

" vim-markdown
"Plug 'godlygeek/tabular'
"Plug 'plasticboy/vim-markdown', { 'for':['md', 'markdown'] }
"Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for':['md', 'markdown'] }
"Plug 'JamshedVesuna/vim-markdown-preview'

"  spaceline status line
"Plug 'taigacute/spaceline.vim'
Plug 'airblade/vim-gitgutter'

"Plug 'xolox/vim-misc'
"Plug 'xolox/vim-notes'

" filetype syntax
"Plug 'vim-jp/vim-cpp'
"Plug 'sheerun/vim-polyglot'

" colorscheme
Plug 'cormacrelf/vim-colors-github'
Plug 'veigrent/vim-colors-e6e1'
Plug 'jonathanfilip/vim-lucius'
Plug 'vim-scripts/peaksea'
"Plug 'wesgibbs/vim-irblack'
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'
Plug 'nlknguyen/papercolor-theme'
Plug 'altercation/vim-colors-solarized'

" Initialize plugin system
call plug#end()


""""""""""""""""""""""""""""""
" => YouCompleteMe
""""""""""""""""""""""""""""""
set completeopt=longest,menu
"set completeopt=menu,menuone
set conceallevel=2
set concealcursor=vin
set pumheight=20
"let g:ycm_log_level='error'
"let g:ycm_global_ycm_extra_conf='$VIM\.ycm_extra_conf.py'
"let g:ycm_extra_conf_globlist=['$HOME\.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/.ycm_extra_conf.py']
"let g:ycm_extra_conf_globlist=['$VIM/.vim/plugged/YouCompleteMe/third_party/ycmd/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py']
if has("win64")
    let g:ycm_global_ycm_extra_conf='$VIM/.vim/plugged/YouCompleteMe-x64/python/.ycm_extra_conf.py'
    let g:ycm_extra_conf_globlist=['$VIM/.vim/plugged/YouCompleteMe-x64/third_party/ycmd/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py']
elseif has("win32")
    let g:ycm_global_ycm_extra_conf='$VIM/.vim/plugged/YouCompleteMe-x86/python/.ycm_extra_conf.py'
    let g:ycm_extra_conf_globlist=['$VIM/.vim/plugged/YouCompleteMe-x64/third_party/ycmd/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py']
else
    let g:ycm_global_ycm_extra_conf='$VIM/.vim/plugged/YouCompleteMe/python/.ycm_extra_conf.py'
    let g:ycm_extra_conf_globlist=['$VIM/.vim/plugged/YouCompleteMe-x64/third_party/ycmd/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/.ycm_extra_conf.py', '$VIM/.vim/plugged/YouCompleteMe/third_party/ycmd/examples/.ycm_extra_conf.py']
endif
"let g:ycm_python_interpreter_path = '/usr/bin/env python3'
"let g:ycm_python_sys_path = ['/usr/lib/python3.6', '/usr/lib/python2.7']
"let g:ycm_extra_conf_vim_data = ['g:ycm_python_interpreter_path', 'g:ycm_python_sys_path']
let g:ycm_complete_in_comments=1
let g:ycm_complete_in_strings=1
let g:ycm_collect_identifiers_from_comments_and_strings=0
let g:ycm_confirm_extra_conf=0
let g:ycm_cache_omnifunc=1
let g:ycm_seed_identifiers_with_syntax=1
let g:ycm_collect_identifiers_from_tags_files=1
let g:clang_snippets=1
let g:clang_conceal_snippets=1
let g:clang_snippets_engine='clang_complete'
let g:SuperTabDefaultCompletionType='<c-x><c-u><c-p>'
"let g:ycm_filetype_blacklist={'*':1}
"let g:ycm_filetype_whitelist={'c':1, 'cpp':1, 'python':1, 'java':1, 'html':1, 'sh':1, 'vb':1, 'dosbatch':1, 'make':1, 'vim':1}
let g:ycm_disable_for_files_larger_than_kb=3000
"let g:ycm_key_list_select_completion=['<c-n>']
"let g:ycm_key_list_select_completion=['<Down>']
"let g:ycm_key_list_previous_completion=['<c-p>']
"let g:ycm_key_list_previous_completion=['<Up>']
"let g:ycm_auto_trigger=0 "trun off YCM
"let g:ycm_auto_trigger=1 "trun on YCM
let g:ycm_goto_buffer_command='new-or-existing-tab'
if g:iswindows
    let g:clangd = glob('D:\Scoop\apps\llvm\10.0.0\bin\clangd*')
else
    let g:clangd = glob('/usr/bin/clangd*')
endif
if !empty(g:clangd)
    let g:ycm_clangd_binary_path = g:clangd
    "if version >= 802   " VIM 8.2 以上支持 popup 特性
        " 关闭 echodoc 的帮助说明，使用 clangd 的帮助说明
        "autocmd FileType c,cpp call echodoc#disable()
    "endif
endif
"nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
nnoremap <leader>jd :YcmCompleter GoToImprecise<CR>
nmap <leader>yfw <Plug>(YCMFindSymbolInWorkspace)
nmap <leader>yfd <Plug>(YCMFindSymbolInDocument)

""""""""""""""""""""""""""""""
" => StatusLine
""""""""""""""""""""""""""""""
" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE '
    endif
    return ''
endfunction

function! GitBranch()
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

" Format the status line
" set stl=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \  Line:\ %l\ \ Column:\ %c
set stl=
set stl+=%#PmenuSel#
set stl+=\ %{HasPaste()}%t%m%r%h\ %w
set stl+=%#LineNr#
set stl+=\ \ %<\ CWD:%{getcwd()}
"set stl+=%#CursorLine#
"set stl+=%#CursorColumn#
"set stl+=\ \ %{coc#status()}
"set stl+=%#StatusLine#
set stl+=%=
set stl+=\ \ %l,%c
set stl+=\ \ [%L,%o]
set stl+=%=\ \ %{&ff}:%{&fenc}:%Y\ %<


""""""""""""""""""""""""""""""
" => air-line
""""""""""""""""""""""""""""""
let g:airline_left_sep=''
let g:airline_right_sep=''
"let g:airline#extensions#whitespace#max_lines = 80
"let g:airline#extensions#whitespace#checks = [ 'indent', 'trailing', 'long' ]
"

""""""""""""""""""""""""""""""
" => Taglist
""""""""""""""""""""""""""""""
let Tlist_WinWidth=30
"let Tlist_Use_Right_Window=1
let Tlist_Use_Left_Window=1
let Tlist_Show_One_File=1
let Tlist_Exit_OnlyWindow=1
let Tlist_File_Fold_Auto_Close=1
let Tlist_GainFocus_On_ToggleOpen=1
"let Tlist_Sort_Type='name'
"let Tlist_Enable_Fold_Column=0
nmap tl :TlistToggle<CR>


""""""""""""""""""""""""""""""
" => NERDTree
""""""""""""""""""""""""""""""
let NERDTreeWinSize=30
let NERDTreeWinPos='right'
let NERDTreeIgnore = ['\.pyc$', '__pycache__']
nmap fl :NERDTreeToggle<CR>
"let NERDTreeShowHidden=0
"map <leader>nn :NERDTreeToggle<cr>
"map <leader>nb :NERDTreeFromBookmark<Space>
"map <leader>nf :NERDTreeFind<cr>
"nmap <silent> <c-n> :NERDTreeToggle<CR>


""""""""""""""""""""""""""""""
" => Mark
" Highlight several words in different colors simultaneously
" 1: Cyan  2:Green  3:Yellow  4:Red  5:Magenta  6:Blue
""""""""""""""""""""""""""""""
let g:mwAutoLoadMarks = 1
let g:mwDefaultHighlightingPalette = 'extended'
nnoremap <leader>c :MarkClear<CR>


""""""""""""""""""""""""""""""
" => ale
" Asynchronous Lint Engine
""""""""""""""""""""""""""""""
let g:ale_linters_explicit = 1
let g:ale_completion_delay = 500
let g:ale_echo_delay = 20
let g:ale_lint_delay = 500
let g:ale_echo_msg_format = '[%linter%] %code: %%s'
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
let g:ale_c_gcc_options = '-Wall -O2 -std=c99'
let g:ale_cpp_gcc_options = '-Wall -O2 -std=c++14'
let g:ale_c_cppcheck_options = ''
let g:ale_cpp_cppcheck_options = ''
let g:airline#extensions#ale#enabled = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => surround.vim config
" Annotate strings with gettext 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
vmap Si S(i_<esc>f)
au FileType mako vmap Si S"i${ _(<esc>2f"a) }<esc>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => lightline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ }

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [ ['mode', 'paste'],
      \             ['fugitive', 'readonly', 'filename', 'modified'] ],
      \   'right': [ [ 'lineinfo' ], ['percent'] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"🔒":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ },
      \ 'separator': { 'left': ' ', 'right': ' ' },
      \ 'subseparator': { 'left': ' ', 'right': ' ' }
      \ }


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vimroom
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:goyo_width=100
let g:goyo_margin_top = 2
let g:goyo_margin_bottom = 2
nnoremap <silent> <leader>z :Goyo<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim-go
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:go_fmt_command = "goimports"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Syntastic (syntax checker)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_linters = {
\   'javascript': ['jshint'],
\   'python': ['flake8'],
\   'go': ['go', 'golint', 'errcheck']
\}

nmap <silent> <leader>a <Plug>(ale_next_wrap)

" Disabling highlighting
let g:ale_set_highlights = 0

" Only run linting when saving the file
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git gutter (Git diff)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_enabled=1
nnoremap <silent> <leader>d :GitGutterToggle<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => coc.nvim (code completer)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colorscheme
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    set bg=light
    set t_Co=256
    "let g:gruvbox_contrast="hard"
    "let g:gruvbox_contrast_dark="hard"
    "let g:gruvbox_contrast_light="hard"
    "let g:gruvbox_italic=0
    "let g:gruvbox_inverse=0
    "color zellner
    "color gruvbox
    "color PaperColor
    "color lucius
    "color github
    "color solarized
    color e6e1
else
    "set bg=dark
    "set t_Co=256
    "color e6e1
    "color github
    "color default
    "color onedark
endif

if has("linux") || has("unix")
    "set background=dark
    "colorscheme peaksea
    "set t_Co=256
    "color lucius
    "LuciusBlackHighContrast
    "LuciusWhiteHighContrast
    "LuciusBlackLowContrast
    "LuciusWhiteLowContrast
endif


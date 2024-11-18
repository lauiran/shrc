if !exists('g:vimrc_auth')
    let g:vimrc_auth = ""
    let g:vimrc_mail = ""
    let g:vimrc_cprt = ""
    let g:vimrc_desc = ""
endif

" New file
autocmd BufNewFile *.cpp,*.cc,*.[ch],*.hh,*.hxx,*.hpp,*.sh,*.java,*.py,*.html exec ":call SetTitle()"

" cd to file direct
autocmd BufRead,BufNewFile,BufEnter * cd %:p:h
autocmd BufRead,BufNewFile * set expandtab
autocmd BufRead,BufNewFile Makefile*,makefile* set noexpandtab

" debug
"autocmd BufRead,BufNewFile,BufEnter *.py,*.pyw nmap <F5> :!start cmd /K python3 %<CR>
autocmd BufRead,BufNewFile,BufEnter *.py,*.pyw nmap <F5> :vert term python3 %<CR>
autocmd BufRead,BufNewFile,BufEnter *.py,*.pyw imap <F5> <Esc> :vert term python3 %<CR>
"autocmd BufRead,BufNewFile,BufEnter *.html nmap <F5> :!start "C:\Program Files\Google\Chrome\Application\chrome.exe" --incognito %:p<CR>
"autocmd BufRead,BufNewFile,BufEnter *.html imap <F5> <Esc>:!start "C:\Program Files\Google\Chrome\Application\chrome.exe" --incognito %:p<CR>
"autocmd BufRead,BufNewFile,BufEnter *.md nmap <F5> <Plug>MarkdownPreview
"autocmd BufRead,BufNewFile,BufEnter *.md imap <F5> <Plug>MarkdownPreview
"autocmd BufRead,BufNewFile,BufEnter *.md nmap <F5> <Plug>MarkdownPreviewToggle
"autocmd BufRead,BufNewFile,BufEnter *.md imap <F5> <Plug>MarkdownPreviewToggle
autocmd BufRead,BufNewFile,BufEnter *.md nmap <F5> :MarkdownPreview<CR>
autocmd BufRead,BufNewFile,BufEnter *.md imap <F5> <Esc>:MarkdownPreview<CR>
autocmd BufRead,BufNewFile,BufEnter *.sh nmap <F5> :vert term bash %<CR>
autocmd BufRead,BufNewFile,BufEnter *.sh imap <F5> <Esc> :vert term bash %<CR>
nmap <silent> <F8> <Plug>MarkdownPreview        " for normal mode
imap <silent> <F8> <Plug>MarkdownPreview        " for insert mode
nmap <silent> <F9> <Plug>StopMarkdownPreview    " for normal mode
imap <silent> <F9> <Plug>StopMarkdownPreview    " for insert mode


""""""""""""""""""""""""""""""
" => Python section
""""""""""""""""""""""""""""""
let python_highlight_all = 1
au FileType python syn keyword pythonDecorator True None False self

au BufNewFile,BufRead *.jinja set syntax=htmljinja
au BufNewFile,BufRead *.mako set ft=mako

au FileType python map <buffer> F :set foldmethod=indent<cr>

au FileType python inoremap <buffer> $r return 
au FileType python inoremap <buffer> $i import 
au FileType python inoremap <buffer> $p print 
au FileType python inoremap <buffer> $f # --- <esc>a
au FileType python map <buffer> <leader>1 /class 
au FileType python map <buffer> <leader>2 /def 
au FileType python map <buffer> <leader>C ?class 
au FileType python map <buffer> <leader>D ?def 
au FileType python set cindent
au FileType python set cinkeys-=0#
au FileType python set indentkeys-=0#


""""""""""""""""""""""""""""""
" => JavaScript section
"""""""""""""""""""""""""""""""
au FileType javascript call JavaScriptFold()
au FileType javascript setl fen
au FileType javascript setl nocindent

au FileType javascript imap <c-t> $log();<esc>hi
au FileType javascript imap <c-a> alert();<esc>hi

au FileType javascript inoremap <buffer> $r return 
au FileType javascript inoremap <buffer> $f // --- PH<esc>FP2xi

function! JavaScriptFold() 
    setl foldmethod=syntax
    setl foldlevelstart=1
    syn region foldBraces start=/{/ end=/}/ transparent fold keepend extend

    function! FoldText()
        return substitute(getline(v:foldstart), '{.*', '{...}', '')
    endfunction
    setl foldtext=FoldText()
endfunction


""""""""""""""""""""""""""""""
" => CoffeeScript section
"""""""""""""""""""""""""""""""
function! CoffeeScriptFold()
    setl foldmethod=indent
    setl foldlevelstart=1
endfunction
au FileType coffee call CoffeeScriptFold()

au FileType gitcommit call setpos('.', [0, 1, 1, 0])


""""""""""""""""""""""""""""""
" => Shell section
""""""""""""""""""""""""""""""
if exists('$TMUX') 
    if has('nvim')
        set termguicolors
    else
        set term=screen-256color 
    endif
endif


""""""""""""""""""""""""""""""
" => Twig section
""""""""""""""""""""""""""""""
autocmd BufRead *.twig set syntax=html filetype=html


""""""""""""""""""""""""""""""
" => SetTitle
""""""""""""""""""""""""""""""
func SetTitle()
    "if &filetype == 'sh'
    if (expand("%:e") == 'sh')
        call setline(1,"\#!/bin/bash")
        call append(line("."), "")
        call append(line(".")+1, "")
        call append(line(".")+2, "")
        normal G-
    elseif (expand("%:e") == 'py')
        call setline(1,"#!/usr/bin/env python")
        "call append(line("."),"# coding=utf-8")
        call append(line("."),"# -*- coding: utf-8 -*-")
        call append(line(".")+1, "")
        call append(line(".")+2, "")
        call append(line(".")+3, "")
        call append(line(".")+4, "'''")
        call append(line(".")+5, "if __name__ == '__main__':")
        call append(line(".")+6, "    test code")
        call append(line(".")+7, "'''")
        call append(line(".")+8, "")
        normal 4G
    elseif (expand("%:e") == 'h')
        let header_def = "_".expand("%:r")."_".expand("%:e")
        let header_def = toupper(header_def)
        call setline(1,"#ifndef ".header_def)
        call append(line("."),"#define ".header_def."  1")
        call append(line(".")+1, "")
        call append(line(".")+2, "#ifdef __cplusplus")
        call append(line(".")+3, "extern \"C\" {")
        call append(line(".")+4, "#endif")
        call append(line(".")+5, "")
        call append(line(".")+6, "")
        call append(line(".")+7, "")
        call append(line(".")+8, "")
        call append(line(".")+9, "#ifdef __cplusplus")
        call append(line(".")+10, "} /* extern \"C\" */")
        call append(line(".")+11, "#endif")
        call append(line(".")+12, "")
        call append(line(".")+13, "#endif /* ".header_def." */")
        call append(line(".")+14, "")
        unlet header_def
        normal 8G
    elseif (expand("%:e") == 'hxx') || (expand("%:e") == 'hh') || (expand("%:e") == 'hpp')
        let header_def = "_".expand("%:r")."_".expand("%:e")
        let header_def = toupper(header_def)
        call setline(1,"#ifndef ".header_def)
        call append(line("."),"#define ".header_def."  1")
        call append(line(".")+1, "")
        call append(line(".")+2, "")
        call append(line(".")+3, "")
        call append(line(".")+4, "")
        call append(line(".")+5, "#endif /* ".header_def." */")
        call append(line(".")+6, "")
        unlet header_def
        normal 4G
    elseif (expand("%:e") == 'html')
        call setline(1,"<!DOCTYPE html>")
        call append(line("."),"")
        call append(line(".")+1,"<html>")
        call append(line(".")+2,"<head>")
        call append(line(".")+3,"    <meta charset=\"utf-8\">")
        call append(line(".")+4,"    <title>TITLE</title>")
        call append(line(".")+5,"</head>")
        call append(line(".")+6,"")
        call append(line(".")+7,"")
        call append(line(".")+8,"")
        call append(line(".")+9,"</html>")
        normal 6G,12l,cw
    else
        call setline(1, "/*")
        call append(line("."),   "    File : ".expand("%"))
        call append(line(".")+1, "    Auth : ".expand(g:vimrc_auth))
        call append(line(".")+2, "    Mail : ".expand(g:vimrc_mail))
        call append(line(".")+3, "    Time : ".strftime("%c"))
        call append(line(".")+4, "*/")
        call append(line(".")+5, "")
    endif

    if (expand("%:e") == 'cpp') || (expand("%:e") == 'cc')
        call append(line(".")+6, "#include <iostream>")
        call append(line(".")+7, "")
        call append(line(".")+8, "using namespace std;")
        call append(line(".")+9, "")
        call append(line(".")+10, "")
        call append(line(".")+11, "")
        call append(line(".")+12, "")
        normal G-
    elseif expand("%:e") == 'c'
        call append(line(".")+6, "#include <stdio.h>")
        call append(line(".")+7, "")
        call append(line(".")+8, "")
        call append(line(".")+9, "")
        call append(line(".")+10, "")
        normal G-
    elseif expand("%:e") == 'java'
        call append(line(".")+6,"public class ".strpart(expand("%d"),0,strlen(expand("%"))-5))
        call append(line(".")+7,"")
        call append(line(".")+8,"")
        call append(line(".")+9,"")
        normal G-
    endif
endfunc


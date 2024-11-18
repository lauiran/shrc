@echo off

echo.> %USERPROFILE%\.vimrc
echo.>> %USERPROFILE%\.vimrc
echo set runtimepath+=$VIM/.vim>> %USERPROFILE%\.vimrc
echo.>> %USERPROFILE%\.vimrc
echo source $VIM/.vim/vimrc_basic.vim>> %USERPROFILE%\.vimrc
echo source $VIM/.vim/vimrc_filetype.vim>> %USERPROFILE%\.vimrc
echo source $VIM/.vim/vimrc_plugin.vim>> %USERPROFILE%\.vimrc
echo source $VIM/.vim/vimrc_extended.vim>> %USERPROFILE%\.vimrc
echo.>> %USERPROFILE%\.vimrc

echo All done :)
pause


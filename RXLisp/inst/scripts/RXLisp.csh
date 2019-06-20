if(${?LD_LIBRARY_PATH}) then
 setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/lib/R/bin:/home/jtan/Projects/xlisp:/home/jtan/Apps/RLibrary/RXLisp/libs
else
 setenv LD_LIBRARY_PATH /usr/lib/R/bin:/home/jtan/Projects/xlisp:/home/jtan/Apps/RLibrary/RXLisp/libs
endif

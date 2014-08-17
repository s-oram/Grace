@rem deletes old installer - must happen after generating docs
del ..\bin\newac_docs.rar
@rem updates filelist.txt with any new files
filelister.exe
@rem compresses files in filelist.txt
rar a -m5 ..\bin\newac_docs.rar @filelist.txt

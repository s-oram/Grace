SET PATH = %PATH%;C:\Perl\bin\
copy d:\repo\newac\doc\config\newac.css c:\NaturalDocs-1.4\Styles
@rem NaturalDocs.bat needs us to be in d:\utils\naturaldocs (where the script lives)
d:
cd d:\NaturalDocs-1.4
NaturalDocs.bat -s newac -img d:\repo\newac\doc\images -i d:\repo\newac\ -o html d:\repo\newac\doc\html -p d:\repo\newac\doc\config
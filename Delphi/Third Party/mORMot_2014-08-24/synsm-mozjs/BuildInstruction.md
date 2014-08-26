<!-- Build instruction for SpiderMonkey24 for Windows for using with Delphi -->

###Preparation
* Download and install **MozillaBuild**. See instruction here [MozillaBuild](https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Build_Instructions/Windows_Prerequisites#mozillabuild)

* Download and install **Mercurial**. See instruction here [Installing Mercurial](https://developer.mozilla.org/en-US/docs/Installing_Mercurial)

* Get Mozilla Source Code. See instruction here [Getting Mozilla Source Code Using Mercurial](https://developer.mozilla.org/en-US/docs/Mozilla/Developer_guide/Source_Code/Mercurial). You need get code from [this repository](https://hg.mozilla.org/releases/mozilla-esr24/)

* Apply patches from folder. You need apply this patches 
 * disable-intl-api.patch
 * shell-version.patch
 * strdup.patch
 * remove-vanilla-alloc.patch
 * JS_DEBUG.patch
 * IsInRequest-949195.patch
 * Delphi patch.patch

 `Delphi patch.patch` must be applied last 
 
### Build NSPR
* Build **NSPR** using **MozillaBuild**. See in struction here [NSPR build instructions](https://developer.mozilla.org/en-US/docs/NSPR_build_instructions)

* Copy **NSPR** files to **MozillaBuild** directory. You need copy this files
 * libnspr4.lib
 * libplc4.lib
 * libplds4.lib

into `mozilla-build/msys/local/lib/`

## Build SpiderMonkey 24
* Run **MozillaBuild**
* `$ cd js\src` 
* `$ autoconf-2.13`
* `$ ./configure --enable-threadsafe  --with-nspr-cflags="-IC:/mozilla-build/msys/local/include"  --with-nspr-libs="  C:/mozilla-build/msys/local/lib/libnspr4.lib  C:/mozilla-build/msys/local/lib/libplds4.lib  C:/mozilla-build/msys/local/lib/libplc4.lib"`
If you install **MozillaBuild** Not in `"C:\"` Change it to correct path.
* `$ make`
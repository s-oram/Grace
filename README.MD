# Grace Project Notes

Grace was originally a closed source, commercial project. In early 2016 I re-released Grace as freeware and started working towards open sourcing it. As such the code and project is organised to fit within my development workflow. I'm planning a few changes to make the project more approachable. 

# Project Organisation

* Grace was originally named *Lucidity* but someone used the name a few months before my first public release. Many of the files still reference *Lucidity*.
* The bulk of the project code is found in `Delphi\Lucidity` and `Delphi\Shared Packages`.
* The BAT files in the project directory root automate a couple basic development tasks. 
* When working on Grace, the project directory is mapped to `s:\`. This provides helps to create consistent working environment and allows absolute paths to be used in Delphi. ` 	01 Lucidity Project Setup.bat` and `04 Lucidity Project Teardown.bat` automate mapping the project directory to `s:\` and cleaning up afterwards. 
* `Delphi\Shared Packages` contains library  code that is shared between all my projects. Not all packages are used in Grace. 
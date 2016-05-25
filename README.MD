# Grace

![Grace Screenshot](/readme/grace_full_size.png?raw=true)

Grace is a software [sampler](https://en.wikipedia.org/wiki/Sampler_(musical_instrument) "wikipedia.org/sampler") instrument for Windows. Use it to create music! 

For more info see http://onesmallclue.com/plugin/grace/

# Features

* 2 filters per voice (LP, BP, HP, comb, lofi & ring modulation)
* 2 x AHDSR envelopes
* 2 x LFOs
* 2 x step sequencers
* 4 x XY control pads
* WAV, AIF and SND formats support
* SFZ patch import

# Press

Bedroom Producers Blog [reviewed Grace](http://bedroomproducersblog.com/2015/09/14/grace-sampler-free/). They said: 

> There’s little doubt that Grace is the best freeware sampler on today’s VST plugin market. [...] It is a stable and compact sampler with a great streamlined workflow and a strong feature set.  It’s the sampler that you’ll think of first when you need to import some of your samples into a project. Some users will be turned off by the lack of round robin support, however Grace will be more than capable of completing the majority of common sampling tasks. This virtual instrument is a wonderful gift for the freeware community and we can’t thank One Small Clue enough for it!

# Project Notes

Grace was originally a closed source, commercial project. In early 2016 Grace was re-released as freeware and I started working towards open sourcing it. Due to it's heritage as a closed-source project, the code and project is organised to fit within my development workflow. I'm planning a few more changes to make the project more approachable for others. 

* Grace was originally named *Lucidity* but someone used the name a few months before my first public release. Many of the files still reference *Lucidity*.
* The bulk of the project code is found in `Delphi\Lucidity` and `Delphi\Shared Packages`.
* The BAT files in the project directory root automate a couple basic development tasks. 
* When working on Grace, the project directory is mapped to `s:\`. This provides helps to create consistent working environment and allows absolute paths to be used in Delphi. ` 	01 Lucidity Project Setup.bat` and `04 Lucidity Project Teardown.bat` automate mapping the project directory to `s:\` and cleaning up afterwards. 
* `Delphi\Shared Packages` contains library  code that is shared between all my projects. Not all packages are used in Grace. 
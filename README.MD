# Grace

![Grace Screenshot](/readme/grace_full_size.png?raw=true)

Grace is a [sampler](https://en.wikipedia.org/wiki/Sampler_(musical_instrument) "wikipedia.org/sampler"). Use it to create music!

Grace is available as a VST 2.4 format plugin for Windows.

For more info see http://onesmallclue.com/plugin/grace/

# Features

* Load as many samples as your memory allows.
* WAV, AIF and SND  audio formats supported.
* SFZ patch import
* 2 x Filters (LP, BP, HP, comb, lofi & ring modulation)
* 2 x AHDSR envelopes
* 2 x LFOs
* 2 x step sequencers
* 4 x XY control pads

# Requirements

* Windows 7 or newer. Grace may run on Windows XP but hasn't been tested. Let me know how it goes!
* VST 2.4 plugin host such as Cubase, Ableton Live or Reaper.
* Grace is a VST 2.4 plugin (32 and 64bit available).

# Press

Bedroom Producers Blog reviewed Grace. [They said:](http://bedroomproducersblog.com/2015/09/14/grace-sampler-free/)

> There’s little doubt that Grace is the best freeware sampler on today’s VST plugin market. [...] It is a stable and compact sampler with a great streamlined workflow and a strong feature set.  It’s the sampler that you’ll think of first when you need to import some of your samples into a project. Some users will be turned off by the lack of round robin support, however Grace will be more than capable of completing the majority of common sampling tasks. This virtual instrument is a wonderful gift for the freeware community and we can’t thank One Small Clue enough for it!

# Download

Grace is can be downloaded from http://onesmallclue.com/plugin/grace/

# Build Notes

If you'd like to hack on Grace or build it from source:

1. Clone this repository.
2. Edit `01 Lucidity Project Setup.bat`. Update `ProjectPath` to match the Grace repository location on your system.
3. Run `01 Lucidity Project Setup.bat`. This creates a new virtual drive at `S:\` containing the contents of the Grace repository.
4. Edit `02 Launch Delphi with Lucidity Project.bat`. Update `Delphi` to match the full path of your Delphi executable.
5. Run `02 Launch Delphi with Lucidity Project.bat`.
6. `Delphi -> Tools -> Options -> Enviroment Variables` <br>
  * Add a new environment variable named `DelphiCompiledLib` set to to `S:\Bin\Delphi XE2`
  * Add a new environment variable named `VstPluginsX86` set to your 32bit VST 2.4 plugin directory. Building the Grace project will output
  the Grace.dll here.
  * Add a new environment variable named `VSTPluginsX64` set to your 64bit VST 2.4 plugin directory.
7. In the project manager you'll see a list of Delphi projects. Build all projects starting from the top.
8. Install:
  * dclDradDropXE2.bpl
  * VamLib_Design.bpl
  * AggPasDesXE2.bpl
  * RedFoxDesXE2.bpl
  * VamBasicDesXE2.bpl
  * LucidityGuiComponentsDesXE2.bpl
9. Finally build the `Grace.dll` project! Check the "build confiurations" for release and debug builds. The configurations named "ForLocal" will output the `Grace.dll` right into your VST plugin folder. Useful when developing and testing changes. The `ReleaseAsFull` configuration will output the `Grace.dll` to the repository `bin` directory. Do this when running the install maker script (Inno Setup).
10. Run `04 Lucidity Project Teardown.bat` when finished to unmount the virtual drive at `S:\`.

# Project Notes

* Grace was orginally named *Lucidity* however someone else used the name before it could be released. The project was renamed to Grace but references to the old name still remain in places.
* Grace was originally a closed source commercial project. In early 2016 Grace was released as freeware and made open source. The project was organised to fit within my personal development workflow. I've since reduced dependencies and made several changes to make the project more approachable. Ideally a few more changes are yet due. For now you'll just need to work around them.
* The bulk of the interesting code for Grace is found in `Delphi\Lucidity`.
* `Delphi\Shared Packages` contains library  code that is shared between all my projects. Not all packages are used in Grace. Removing the unused code here is on the TODO list.

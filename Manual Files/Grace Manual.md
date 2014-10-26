# Grace Manual #

* Last Updated 25th October 2014
* For Grace Version 1.0

# Welcome to Grace #

Thank you for purchasing Grace! 

Grace is a VST Plugin Sampler. It's ideal for playing melodic samples. Pianos, keys, strings etc. Creative producers and musicians will find a host of other uses as well. 
Grace's main goal is to make using original samples in your music as simple as possible. 

I hope you enjoy using Grace! Please do not hesitate to get in touch via the support forum or email if you have any questions.   



Yours Sincerely<br> 
Shannon Oram <br>
One Small Clue <br> 
shannon@onesmallclue.com <br>

### Thank You ###

A huge thanks to everyone who's helped with Grace's development in one way or another. <br>
- Andy Brader<br>
- Damien Sutton<br>
- Yann SNK<br>
- Juanjo Cotado<br>
- Chris Holmes<br>
- Hugo Vinagre<br>
- Lowkey<br>
- Biasior Andrea<br>
- Goran Podrugovic (G-Roove)<br>
- Joseph<br>
- Tooshka<br>
- Lilly<br>

### Trademark ###

VST is a trademark of Steinberg Media Technologies GmbH





# Installation #

1) Visit www.onesmallclue.com/grace.php<br>
2) Download and run the installer.<br>

## Installation Troubleshooting ##

If Grace doesn't show in your DAW or VST host after installing. Try:

* Check the Grace.dll plugin file was installed into the correct VST plugin directory. Many hosts use different default locations for VST plugins and you may have several plugin directories on your hard drive. 
* Check you installed the correct version. 32bit DAWs require 32bit plugins. 64bit DAWs require 64bit plugins.  
* Check you only have one Grace.dll file in your VST pluging directories.
* Use the "rescan plugin directory" command in your DAW.
* Try uninstalling and reinstalling. 
* Contact support if none of the above work. 

 

## Installation Effects ##

The installer will place files into two locations. Your VST plugin directory and a data directory. The data directory can be specified when running the installer. The default location is:<br> 
&nbsp;&nbsp;C:\ProgramData\One Small Clue\Grace<br>

The data directory contains files essential to Grace and Grace will not run properly without it. 

The installer will store the installation paths in the Windows registry. These are used by the Grace installer when running updates.  
 






 







# Interface Elements #

<img src="images/MainInterfaceDescription.png">

## File Browser ##

Add and Remove items from the file browser via context menu. (Right click anywhere in the browser.) 


## Sample Display ##

The sample display shows the currently selected sample. 

The sample markers (sample start, sample end, loop start, loop end) can all be adjusted by dragging. 

There are a number of sample playback controls under the sample display.

**Volume:** Adjust volume of sample.<br>
**Pan:** Adjust stereo balance of sample.<br> 
**Tune:** Adjust pitch of sample in semitone steps.<br>
**Fine:** Adjust pitch of sample by +/- 100 cents.<br>
  
## Voice Controls ##

The voice controls are split over two panels. The voice controls adjust sample playback. All voice control settings are applied to all samples in the *key group*.

### Main Voice Controls ###

<img src="images/MainVoiceControls.png" >

**Gain:** Adjust volume of key group.<br>
**Pan:** Adjust stereo balance of key group.<br> 
**Tune:** Adjust pitch of key group in semitone steps.<br>
**Fine:** Adjust pitch of key group by +/- 100 cents.<br>

**Track:** controls how and when a sample is re-pitched. 

* **NOTE:** Use for keyboard type sounds.
* **BPM:** Use for tempo synced loops.
* **OFF:** No pitch tracking. Sample always plays back at the same pitch.


**Trigger:** Turns looping on or off.

* **ONE SHOT:** No looping. Sample plays until end.
* **NO LOOP:** No looping. Sample plays until MIDI Note Off event is received.
* **LOOP CONTINUOUS:** Looping on.
* **LOOP UNTIL RELEASE:** Looping on until a MIDI Note Off event is received.



**Bounds:** Controls where the sample loops.

* **SAMPLE:** Loops between sample start and sample end markers.
* **LOOP:** Loops between loop start and loop end markers.

Looping using **SAMPLE** bounds is useful when modulating the sample start and end markers to produce "glitchy" sounds.


**Reset:** When set to **LFO 1** or **LFO 2**, the sample playback position will be reset to the beginning in sync with the LFO source. This is useful for doing tempo synced drum roll type sounds. 


**Mode:** The voice mode selector.

* **POLY:** Normal polyphonic mode.
* **MONO:** Monophonic, each note re-triggers envelopes and LFOs. 
* **LEGATO:** Monophonic, envelopes aren't re-triggered when notes overlap.  
* **LATCH:** The first note triggers a voice. The second note releases or stops the voice. LATCH is normally used with loops. 

**Glide:** Controls the pitch slew rate when using MONO or LEGATO voice modes. 




### The Filter Section ###

Grace features two flexible filter slots. Each filter slot can load one of several different filter types. The two filters can be routed in a serial or parallel configuration. 

<img src="images/FilterControls.png" >

**Filter Type Selector:** Choose the type of filter.

**Filter Key Follow:** Controls how much the filter frequency will track the MIDI Input.  

**Filter Blend:** When the knob is fully CCW only FILTER ONE will heard at the output. When the knob is fully CW only FILTER TWO will be heard at the output. In between values allow for the two filters to be mixed together.  

**Filter Routing:**

* **SERIAL:** The output of FILTER ONE will be fed to the input of FILTER TWO. FILTER TWO is sent to the output. 
* **PARALLEL:** The output of both filters is sent directly to the output. 

BLEND will control the balance of the two filters in both FILTER ROUTING modes. Ensure the BLEND knob is fully CW for the classic serial mode filter routing. Other values will allow some of filter one to be heard in the output.  
 

### The AHDSR Envelope Section ###
 
Grace has two clasic AHDSR envelopes. The **AMP ENVELOPE** is always routed to the voice amplitude (volume). The **MOD ENVELOPE** is freely assignable to any destination via the modulation matrix.  

<img src="images/EnvelopeControls.png" >

**A:** Attack envelope time.<br> 
**H:** Hold envelope time.<br>
**D:** Decay envelope time.<br>
**S:** Sustain envelope time.<br>
**R:** Release envelope time.<br>

**Snap On/Off:** SNAP ON envelopes have an instantaneous minimum attack time. It's often used with drum samples when it's important to preserve the initial attack transient. <br>
SNAP OFF envelopes will always have smooth attack, decay and release stages. This prevents clicks when envelopes open and close too quickly. This is the default behaviour and recommended for most situations.    


**Velocity Sensitivity:** Controls how much the MIDI velocity input level affects the amplitude of the envelope.  At 0% the envelope will always rise to the full envelope value. At 100% the envelope will be scaled by the velocity value. Lower velocity values will trigger correspondingly smaller envelope signals.   


### The LFO Section ###

Grace has two Low Frequency Oscillators (LFO) with the usual selection of triangle, square and ramp wave shapes. There are some more exotic selections as well. 

<img src="images/LfoControls.png" >
  
**LFO Shape:** Selects the LFO waveform shape. The function of the three LFO knobs will vary depending on the LFO shape. 

**LFO Range:** Select the LFO timing range. 

#### Periodic LFO Waveform Shapes ####

* Saw 
* Ramp
* Triangle
* Square 
* Sine

The three LFO knobs are **RATE**, **PH** (phase) and **SYM** (symmetry) when using the periodic waveform shapes.

 
#### Random LFO Waveform Shapes ####

* Random Smooth 
* Random Stepped

The three LFO knobs are **RATE**, **%** (chance of change) and **FLUX** (amount of change).

  
#### Attack-Decay LFO Waveform Shapes ####

* Attack-Decay 
* Attack-Release
* Attack-Decay-Cycle

The three LFO knobs are **RATE**, **BIAS** and **BEND**.

These selections are not regular LFO shapes in the traditional sense. They are based on Attack-Decay envelopes commonly found in modular synthesisers.

**Attack-Decay** and **Attack-Release** will trigger once at the beginning of a note then stop. They are not true LFOs because they do not oscillate. They are included here because it's sometime useful to have an extra envelope.

**Attack-Decay-Cycle** will trigger at the beginning of the note and continue cycling like a regular LFO. This shape is very similar to the Triange LFO shape but it reacts different when it's controls are modulated.

### The Sequencer Section ###

Grace also features two step sequencers for maximum modulation possibilities!

<img src="images/SequencerControls.png" >

**Clock:** Controls the speed of the sequencer. <br>
**Mode:** Controls the direction of the sequencer. <br> 
**Steps:** Sets the number of steps in a sequence. <br>

A sequencer context menu has commands to RESET or RANDOMISE the steps.

### The XY Pads ###

The XY Pads provide another modulation source similar to the Mod Envelope or the LFOs. The XY Pads can modulate multiple parameters at once using the Modulation Matrix.

Most commonly the XY Pads will be linked to your MIDI controller knobs. They provide an important interface point between your MIDI controller and Grace's parameters.

<img src="images/XyPadControls.png" >

Right click an XY Pad to show the context menu with commands for MIDI Learn etc.       

## Filters ##

(Details about the individual filter types goes here!)   

## Modulation ##

The Modulation Matrix routes signals from the modulation sources (LFOs, Envelopes, XY Pads, MIDI Input etc) to Grace's parameters. It is the heart of Grace's modulation system.   

<img src="images/ModMatrix.png" >

**Main:** Modulation Matrix editing is disabled when the MAIN button is selected (as shown in the image above).

**Mod Shots:** Grace features 8 MOD SLOTS. Each MOD SLOT provides a way to route one modulation source to multiple destinations. 

**Prev/Next Mod Slot:** These buttons select the previous or next MOD SLOT. Off course you don't have to use these buttons. You may simply click on the desired MOD SLOT to select it.


### How To Internally Modulate a Parameter ###

<img src="images/InternalModulationSetup_Step1.png" >

**Step 1** Select a MOD SLOT

<img src="images/InternalModulationSetup_Step2.png" >

**Step 2** Locate the parameter you would like to modulate. Filter One FREQ in this case.

**Step 3** Hold the ALT key while using the adjust the knob. You will see the modulation depth indicator advance around the knob.  

<img src="images/InternalModulationSetup_Step4.png" >

**Step 4** Select the MAIN modulation button. Notice the modulation depth indicator around the FREQ knob changes color. It now shows the maximum modulation depth of all modulation sources combined. (A parameter can be modulated by multiple modulation sources.)


# The Mapping Editor #

Samples, which are audio files, are made playable by creating Regions. Regions are mapped to a location on a keyboard. Each region contains 1 sample.  

<img src="images/SampleMapEditDisplay.png" >

One or more sample files may be dropped on the mapping display to add more sample regions. Audio samples can dragged from Grace's browser, Windows' file explore or from within some hosts. (Reaper, Cubase and many others support audio being dragged directly from the project timeline to VST Plugins.)


# Keyboard Shortcuts #

Hotkey support is fairly limited at this time and will slowly improve in future updates. 

**Up/Down/Left/Right:** Navigate the browser.<br>
**Enter:** Load selected item in browser.<br> 
 



    

 




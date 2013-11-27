== EasyEffectTemplate ===

This folder contains all the essential files for the Easy Effect VST template.


== EasyEffectExtra ===

Contains extra files that may be useful in building a VST plugin. There files 
aren't used in base Easy Effect template project. 



== eeGen ===

Contains oscillator type classes. 




=================================
* EasyEffectTemplate folder should contain files for implementing a VST Plugin. 

* EasyEffectTemplate Core should contain 'core' units 
  that will always be available for use in any EasyEffect
  unit. (They must be maintained for backwards compatibility.
  Development of core classes needs to be very carefully 
  considered as they will be used by multiple bits of code.


=================================
* Sub System folders should contain additional
  classes that can be used. Ideally sub systems 
  - shouldn't depend on other sub systems.
  - can be optionally excluded. 

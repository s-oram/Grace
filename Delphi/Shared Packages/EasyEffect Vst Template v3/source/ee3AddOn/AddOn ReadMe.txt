=================================
        AddOn Rules
=================================

AddOn's encapsulate chunks of functionality commonly useful in creating
VST plugins.

1) Do not use any AddOn in the VST template.
   They are only for use by the plugin developers. If the functionality
   is needed at the template level, it is not an AddOn.

2) AddOn's should be as self contained as possible.
   Dependencies should clearly be marked and limited.
   Minimising dependencies should make it easier for plugin developers
   to mix and match add ons as the project requires.


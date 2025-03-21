# Experiment folder
This folder contains the necessary scripts to run the NetLogo experiments and perform some automatic processing of the output.

The top level contains (bash) scripts which use the environment variables defined in the .env to generate the population data and the experiment configuration files for NetLogo and then run them.

NOTE: these scripts are intended to be run from the project root folder.

## netlogo_templates
The netlogo_templates/ folder contains the template experiment configuration for NetLogo. These are used by the experiment scripts as a basis for finished experiment configurations. In essence they perform variable substitution in order to set the right paths for the input and output of the experiment runs.
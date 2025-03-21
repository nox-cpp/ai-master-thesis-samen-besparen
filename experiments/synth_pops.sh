#! /bin/bash

START_TIME=$SECONDS

# import environment variables from the .env
source .env

# make it so the most important variables are available for R
export GP_ROOT_FOLDER=$GP_ROOT_FOLDER
export GP_SCRIPT_FOLDER=$GP_SCRIPT_FOLDER

# give hint
echo "Make sure to run this script from the project root"

# check if Rscript can be run
echo "Checking Rscript version for compatibility"
if !(Rscript --version); then
    echo "R(script) not available in the current environment, exiting."
    exit 1
fi

# general settings and setup
EXPERIMENT_BASE_NAME=synth_pops
EXPERIMENT_OUTPUT_DIR=$GP_OUTPUT_DATA_FOLDER/experiments/$EXPERIMENT_BASE_NAME
SETUP_LOG=$EXPERIMENT_OUTPUT_DIR/setup_log.txt
mkdir -p $EXPERIMENT_OUTPUT_DIR

# check for extra script arguments
PERFORM_SETUP=false
EXTRA_ARGS=
PERFORM_EXPERIMENT=true
PERFORM_ANALYSIS=true
for arg in $@ ; do
    if [[ $arg == "--skip-setup" ]]; then
        PERFORM_SETUP=false
    elif [[ $arg == "--perform-setup" ]]; then
        PERFORM_SETUP=true
    elif [[ $arg == "--skip-run" ]]; then
        PERFORM_EXPERIMENT=false
    elif [[ $arg == "--skip-analysis" ]]; then
        PERFORM_ANALYSIS=false
    elif [[ $arg == "--filter="* ]]; then
        POPULATION_FILTER=${arg:9:${#arg}}
        echo Using population filter: $POPULATION_FILTER
    elif [[ $arg == "--help" ]]; then
        echo "This script runs the population synthesis experiment."
        echo "The following options are available when running this script:"
        echo -e "\t--help: prints this help message and exits"
        echo -e "\t--perform-setup: (re)generates the experiment populations"
        echo -e "\t\tIf setup is enabled, some extra logs may be available at:\
                $SETUP_LOG"
        echo -e "\t--skip-setup: (default) skip generating experiment populations"
        echo -e "\t\t The last passed of the two former arguments will hold"
        echo -e "\t--skip-run: skips running the experiment (but not setup)"
        echo -e "\t--skip-analysis: skips running the experiment analysis"
        echo -e "\t--filter=<filter>: pass a filter to the population selection"
        echo -e "\t\tThis can be used to only run sub-experiments."
        echo -e "\t\tThe filter is passed directly as argument to grep"
        exit 0
    else
        # unaccounted arguments are passed on to the script by default
        EXTRA_ARGS="$EXTRA_ARGS $arg"
    fi
done

# timestamp for use in result files
TIMESTAMP=$(date -d "today" +"%Y-%m-%d")
POPULATION_DIR=$EXPERIMENT_OUTPUT_DIR/populations
ARCHIVE_DIR=$EXPERIMENT_OUTPUT_DIR/archive/$TIMESTAMP

# configure setup
# NOTE: this setup generates files in the output folder that are used as input
if [[ $PERFORM_SETUP == "true" ]]; then
    # clean up the population folder
    rm -f $POPULATION_DIR/*.csv
    # take the filter into account if present
    if [[ $POPULATION_FILTER != "" ]]; then
      EXTRA_ARGS=$EXTRA_ARGS"--filter="${POPULATION_FILTER}
    fi
    # run the setup
    EXPERIMENT_SETUP=$GP_SCRIPT_FOLDER/experiments/$EXPERIMENT_BASE_NAME/setup.r
    SCRIPT_CMD="Rscript $EXPERIMENT_SETUP $EXTRA_ARGS"
    echo Running $SCRIPT_CMD
    # run the setup script and write output logs to file
    if !($SCRIPT_CMD > $SETUP_LOG); then
        echo "Rscript setup failed to complete successfully, exiting."
        echo "Check $SETUP_LOG for additional information."
        exit 1
    fi
    # copy all the generated populations to the archive
    mkdir -p $ARCHIVE_DIR/populations
    # clean up the archive folder
    rm -f $ARCHIVE_DIR/*.csv
    cp -t $ARCHIVE_DIR/populations $POPULATION_DIR/*.csv
else
    echo "Skipping setup"
fi

# output folders and files
EXPERIMENT_OUTPUT_DIR=$GP_OUTPUT_DATA_FOLDER/experiments/$EXPERIMENT_BASE_NAME
BEHAVIORSPACE_OUTPUT_DIR=$EXPERIMENT_OUTPUT_DIR
BEHAVIORSPACE_OUTPUT_FILE=$BEHAVIORSPACE_OUTPUT_DIR/$EXPERIMENT_BASE_NAME.csv
EXPERIMENT_INPUT_FOLDER=$GP_ROOT_FOLDER/experiments/netlogo_templates
EXPERIMENT_FOLDER=$GP_ROOT_FOLDER/experiments/netlogo_templates
EXPERIMENT_TEMPLATE=$EXPERIMENT_INPUT_FOLDER/${EXPERIMENT_BASE_NAME}-experiment.xml_template
EXPERIMENT_CONFIG=$EXPERIMENT_INPUT_FOLDER/generated/${EXPERIMENT_BASE_NAME}-experiment.xml
ARCHIVE_PATH=$EXPERIMENT_OUTPUT_DIR/archive

# make sure the config target folder is available
mkdir -p $EXPERIMENT_INPUT_FOLDER/generated

# perform variable substition on the experiment template to get the experiment
# config that we will pass to NetLogo
REPLACEMENTS="s#\$GP_INPUT_DATA_FOLDER#$GP_INPUT_DATA_FOLDER#g;"\
"s#\$GP_OUTPUT_DATA_FOLDER#$GP_OUTPUT_DATA_FOLDER#g;"\
"s#\$EXPERIMENT_OUTPUT_DIR#$EXPERIMENT_OUTPUT_DIR#g;"

# replaces in/output folder locations
sed -e $REPLACEMENTS $EXPERIMENT_TEMPLATE > $EXPERIMENT_CONFIG

# this captures all the populations in the input folder and makes it so they can
# be inserted into the template folder
# the first cmd lists the population files full paths
# second adds the experiment XML markup
# the third replaces newline characters with escaped newlines
XML_MARKUP_REPLACEMENT="s#^\(.*\)\$#\t\t<value value=\"\\&quot;\1\\&quot;\"/>#g;"
POPULATIONS=$(ls -d $POPULATION_DIR/*.csv | grep "$POPULATION_FILTER" | sed -e "$XML_MARKUP_REPLACEMENT")

if [[ $POPULATIONS == ""  ]]; then
    echo "Population data missing, run this script with --perform-setup or manually add experiment populations to $POPULATION_DIR"
    exit 1
fi

# more output files and directories
POPULATION_LIST_FILE=$EXPERIMENT_FOLDER/generated/${EXPERIMENT_BASE_NAME}_populations.lst
BEHAVIORSPACE_OUTPUT_DIR=$EXPERIMENT_OUTPUT_DIR
BEHAVIORSPACE_OUTPUT_FILE=$EXPERIMENT_OUTPUT_DIR/$EXPERIMENT_BASE_NAME.csv

# write the population list to an intermediate file
tee $POPULATION_LIST_FILE <<< "$POPULATIONS" 1>/dev/null 

# add the population paths to the template
# this is a convoluted sed command which came about because in-place replacement
# variable substitution does not work for large population lists
LINE_NR=$(grep -n \$AGENT_FILES $EXPERIMENT_TEMPLATE | cut -d : -f 1)
if [[ $LINE_NR != "" ]]; then
    sed -i -e "$LINE_NR r $POPULATION_LIST_FILE" $EXPERIMENT_CONFIG
    sed -i -e ${LINE_NR}d $EXPERIMENT_CONFIG
else
    echo "Could not find target for population list in template."
    echo "Cannot generate experiment setup, check the template and try again."
    exit 1
fi

# at this point the configuration is done
echo "Experiment configuration saved in $EXPERIMENT_CONFIG"

# make sure that all our output locations are available
mkdir -p $EXPERIMENT_INPUT_FOLDER/generated
mkdir -p $EXPERIMENT_OUTPUT_DIR/output

# clean up the output folder if needed
[[ -f $WORLD_STATE_OUTPUT_DIR/*.csv ]] && rm $WORLD_STATE_OUTPUT_DIR/*.csv

# the archive is where we put (daily) timestamped results
mkdir -p $ARCHIVE_PATH
mkdir -p $ARCHIVE_PATH/$TIMESTAMP

# run NetLogo (headless)
if [[ $PERFORM_EXPERIMENT == "true" ]]; then
    echo Starting NetLogo with the following command:
    echo NetLogo --headless --model $GP_ROOT_FOLDER/model/Grunneger\ Power.nlogo \
    --setup-file $EXPERIMENT_CONFIG \
    --table $BEHAVIORSPACE_OUTPUT_FILE \
    --threads $GP_MAX_THREADS
    if (NetLogo --headless --model $GP_ROOT_FOLDER/model/Grunneger\ Power.nlogo \
    --setup-file $EXPERIMENT_CONFIG \
    --table $BEHAVIORSPACE_OUTPUT_FILE \
    --threads $GP_MAX_THREADS); then
        # timestamp results and add to archive
        ARCHIVE_FILE_PATH=$ARCHIVE_PATH/$TIMESTAMP/${EXPERIMENT_BASE_NAME}.csv
        echo Copying results to archive as $ARCHIVE_FILE_PATH\
                and $ARCHIVE_PATH/$TIMESTAMP
        # copy the behaviourspace output
        cp $BEHAVIORSPACE_OUTPUT_FILE $ARCHIVE_FILE_PATH
    fi
else
    echo "Skipping experiment run"
fi

if [[ $PERFORM_ANALYSIS == "true" ]]; then
    echo "Performing results analysis"
    # run a results analysis/generator
    SCRIPT_CMD="Rscript $GP_SCRIPT_FOLDER/experiments/$EXPERIMENT_BASE_NAME/analysis.r $EXTRA_ARGS"
    echo Running $SCRIPT_CMD
    if !($SCRIPT_CMD); then
        echo "Rscript for analysis failed to complete successfully, exiting."
        exit 1
    fi
else
    echo "Skipping results analysis"
fi

TIME_TAKEN=$(( SECONDS - START_TIME ))

echo "Done in $TIME_TAKEN seconds"

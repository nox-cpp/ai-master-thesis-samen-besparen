#! /bin/bash

# start a timer
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

# check for extra script arguments
PERFORM_SETUP=false
EXTRA_ARGS=
PERFORM_EXPERIMENT=true
POPULATION_FILTER=""
PERFORM_ANALYSIS=true
for arg in $@ ; do
    if [[ $arg == "--skip-setup" ]]; then
        PERFORM_SETUP=false
    elif [[ $arg == "--perform-setup" ]]; then
        PERFORM_SETUP=true
    elif [[ $arg == "--skip-analysis" ]]; then
        PERFORM_ANALYSIS=false
    elif [[ $arg == "--skip-run" ]]; then
        PERFORM_EXPERIMENT=false
    elif [[ $arg == "--filter="* ]]; then
        POPULATION_FILTER=${arg:9:${#arg}}
        echo Using population filter: $POPULATION_FILTER
    elif [[ $arg == "--help" ]]; then
        echo "This script runs the mapping space exploration experiment"
        echo "The following options are available when running this script:"
        echo -e "\t--help: prints this help message and exits"
        echo -e "\t--perform-setup: (re)generates the experiment populations"
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

# population folders
EXPERIMENT_BASE_NAME=mapping_space
EXPERIMENT_OUTPUT_DIR=$GP_OUTPUT_DATA_FOLDER/experiments/$EXPERIMENT_BASE_NAME
POPULATION_DIR=$EXPERIMENT_OUTPUT_DIR/populations
ARCHIVE_DIR=$EXPERIMENT_OUTPUT_DIR/archive/$TIMESTAMP

if [[ $PERFORM_SETUP == "true" ]]; then
    # clean up the population folder
    rm -f $POPULATION_DIR/*.csv
    # take the filter into account if present
    if [[ $POPULATION_FILTER != "" ]]; then
      EXTRA_ARGS=$EXTRA_ARGS"--filter="${POPULATION_FILTER}
    fi
    # run the r setup script to generate the populations
    SCRIPT_CMD="Rscript $GP_SCRIPT_FOLDER/experiments/$EXPERIMENT_BASE_NAME/setup.r $EXTRA_ARGS"
    echo Running $SCRIPT_CMD
    if !($SCRIPT_CMD); then
        echo "Rscript setup failed to complete successfully, exiting."
        exit 1
    fi
    # copy all the generated populations to the archive
    mkdir -p $ARCHIVE_DIR/populations
    # clean up the archive folder
    rm -f $ARCHIVE_DIR/*.csv
    cp -t $ARCHIVE_DIR/populations $POPULATION_DIR/*.csv
    MAPPING_CFG_FILE=$EXPERIMENT_OUTPUT_DIR/${EXPERIMENT_BASE_NAME}_cfg.csv
    cp -t $ARCHIVE_DIR $MAPPING_CFG_FILE
else
    echo "Skipping setup"
fi

# prepare variable substition on the template to get the experiment config that
# we will pass to NetLogo
EXPERIMENT_FOLDER=$GP_ROOT_FOLDER/experiments/netlogo_templates
EXPERIMENT_TEMPLATE=$EXPERIMENT_FOLDER/${EXPERIMENT_BASE_NAME}-experiment.xml_template
EXPERIMENT_CONFIG=$EXPERIMENT_FOLDER/generated/${EXPERIMENT_BASE_NAME}-experiment.xml

# make sure the config target folder is available
mkdir -p $EXPERIMENT_FOLDER/generated

# replacement targets for sed
REPLACEMENTS="s#\$GP_INPUT_DATA_FOLDER#$GP_INPUT_DATA_FOLDER#g;"\
"s#\$GP_OUTPUT_DATA_FOLDER#$GP_OUTPUT_DATA_FOLDER#g;"

# performs the replacement
sed -e $REPLACEMENTS $EXPERIMENT_TEMPLATE > $EXPERIMENT_CONFIG

# this captures all the populations in the input folder and makes it so they can
# be inserted into the template folder
# the first cmd lists the population files full paths
# second adds the experiment XML markup
# the third replaces newline characters with escaped newlines
XML_MARKUP_REPLACEMENT="s#^\(.*\)\$#\t\t\t<value value=\"\\&quot;\1\\&quot;\"/>#g;"
POPULATIONS=$(ls -d $POPULATION_DIR/*.csv | grep "$POPULATION_FILTER" | sed -e "$XML_MARKUP_REPLACEMENT")

if [[ $POPULATIONS == ""  ]]; then
    echo "Population data missing, run this script with --perform-setup or manually add experiment populations to $POPULATION_DIR"
    exit 1
fi

# output files and directories
POPULATION_LIST_FILE=$EXPERIMENT_FOLDER/generated/populations.lst
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

# make sure our output folders are available
mkdir -p $EXPERIMENT_OUTPUT_DIR
mkdir -p $EXPERIMENT_OUTPUT_DIR/output
mkdir -p $ARCHIVE_DIR

if [[ $PERFORM_EXPERIMENT == "true" ]]; then
    # run NetLogo (headless)
    # NOTE: we can't trivially/safely store the NetLogo call due to the space in
    # the model name, so this is duplicated
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
        echo Copying results to archive as $ARCHIVE_DIR/$EXPERIMENT_BASE_NAME.csv
        cp $BEHAVIORSPACE_OUTPUT_FILE $ARCHIVE_DIR/$EXPERIMENT_BASE_NAME.csv
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

echo Done in $TIME_TAKEN seconds
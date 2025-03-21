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

# check for extra script arguments
PERFORM_SETUP=false
EXTRA_ARGS=
PERFORM_EXPERIMENT=true
for arg in $@ ; do
    if [[ $arg == "--skip-setup" ]]; then
        PERFORM_SETUP=false
    elif [[ $arg == "--perform-setup" ]]; then
        PERFORM_SETUP=true
    elif [[ $arg == "--skip-run" ]]; then
        PERFORM_EXPERIMENT=false
    elif [[ $arg == "--help" ]]; then
        echo "This script runs the mapping space exploration experiment."
        echo "The following options are available when running this script:"
        echo -e "\t--help: prints this help message and exits"
        echo -e "\t--perform-setup: (re)generates the experiment populations"
        echo -e "\t--skip-setup: (default) skip generating experiment populations"
        echo -e "\t\t The last passed of the two former arguments will hold"
        echo -e "\t--skip-run: skips running the experiment (but not setup)"
        exit 0
    else
        # unaccounted arguments are passed on to the script by default
        EXTRA_ARGS="$EXTRA_ARGS $arg"
    fi
done

# NOTE: this setup generates files in the output folder that are used as input
EXPERIMENT_BASE_NAME=barebones
if [[ $PERFORM_SETUP == "true" ]]; then
    echo Running Rscript $GP_SCRIPT_FOLDER/experiments/$EXPERIMENT_BASE_NAME/setup.r $EXTRA_ARGS
    if !(Rscript $GP_SCRIPT_FOLDER/experiments/$EXPERIMENT_BASE_NAME/setup.r $EXTRA_ARGS); then
        echo "Rscript setup failed to complete successfully, exiting."
        exit 1
    fi
else
    echo "Skipping setup"
fi

# timestamp for use in result files
TIMESTAMP=$(date -d "today" +"%Y-%m-%d")

# output folders and files
EXPERIMENT_OUTPUT_DIR=$GP_OUTPUT_DATA_FOLDER/experiments/$EXPERIMENT_BASE_NAME
BEHAVIORSPACE_OUTPUT_DIR=$EXPERIMENT_OUTPUT_DIR
BEHAVIORSPACE_OUTPUT_FILE=$BEHAVIORSPACE_OUTPUT_DIR/$EXPERIMENT_BASE_NAME.csv
EXPERIMENT_INPUT_FOLDER=$GP_ROOT_FOLDER/experiments/netlogo_templates
EXPERIMENT_TEMPLATE=$EXPERIMENT_INPUT_FOLDER/${EXPERIMENT_BASE_NAME}-experiment.xml_template
EXPERIMENT_CONFIG=$EXPERIMENT_INPUT_FOLDER/generated/${EXPERIMENT_BASE_NAME}-experiment.xml
WORLD_STATE_OUTPUT_DIR=$EXPERIMENT_OUTPUT_DIR/world_states
ARCHIVE_PATH=$EXPERIMENT_OUTPUT_DIR/archive

# make sure the config target folder is available
mkdir -p $EXPERIMENT_INPUT_FOLDER/generated

# perform variable substition on the experiment template to get the experiment
# config that we will pass to NetLogo
REPLACEMENTS="s#\$GP_INPUT_DATA_FOLDER#$GP_INPUT_DATA_FOLDER#g;"\
"s#\$GP_OUTPUT_DATA_FOLDER#$GP_OUTPUT_DATA_FOLDER#g;"\
"s#\$EXPERIMENT_OUTPUT_DIR#$EXPERIMENT_OUTPUT_DIR#g;"

sed -e $REPLACEMENTS $EXPERIMENT_TEMPLATE > $EXPERIMENT_CONFIG

# make sure that all our output locations are available
mkdir -p $EXPERIMENT_INPUT_FOLDER/generated
mkdir -p $EXPERIMENT_OUTPUT_DIR
mkdir -p $EXPERIMENT_OUTPUT_DIR/output
mkdir -p $EXPERIMENT_OUTPUT_DIR/world_states

# clean up the output folder if needed
[[ -f $WORLD_STATE_OUTPUT_DIR/*.csv ]] && rm $WORLD_STATE_OUTPUT_DIR/*.csv

# the archive is where we put (daily) timestamped results
mkdir -p $ARCHIVE_PATH
mkdir -p $ARCHIVE_PATH/$TIMESTAMP
mkdir -p $ARCHIVE_PATH/$TIMESTAMP/world_states

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
        # copy the world state output
        cp -r $EXPERIMENT_OUTPUT_DIR/world_states $ARCHIVE_PATH/$TIMESTAMP
    fi
else
    echo "Skipping experiment run"
fi

# TODO: run a results analysis/generator

TIME_TAKEN=$(( SECONDS - START_TIME ))

echo Done in $TIME_TAKEN seconds
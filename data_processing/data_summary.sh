#! /bin/bash

# import environment variables from the .env
. .env

# make it so the most important variable are available for R
export GP_ROOT_FOLDER=$GP_ROOT_FOLDER
export GP_SCRIPT_FOLDER=$GP_SCRIPT_FOLDER

if [[ $GP_ROOT_FOLDER == "" ]]; then
    echo "Environment is not available, please add .env or add the required"\
        "environment variables manually"
fi

# check if Rscript can be run
if !(Rscript --version); then
    echo "R(script) not available in the current environment, exiting."
    exit 1
fi

# keep track of enabled options in the file-stamp
OPTION_SUFFIX=

# check the script arguments
REGENERATE=false
AUGMENT=false
for arg in $@ ; do
    if [[ $arg == "--augment" ]]; then 
        AUGMENT=true
        OPTION_SUFFIX=$OPTION_SUFFIX"_aug"
    fi
    if [[ $arg == "--regenerate" ]]; then 
        REGENERATE=true
        OPTION_SUFFIX=$OPTION_SUFFIX"_regen"
    fi
done

# set the arguments for the r script
SCRIPT_ARGS=""

if ($REGENERATE); then
    echo "Regenerating the data prior to running the script"
    SCRIPT_ARGS="--args --regenerate"
    if !($AUGMENT); then
        echo "Skipping data augmentation"
        SCRIPT_ARGS=$SCRIPT_ARGS" --no-augment"
    fi
fi

# prepare the date to timestamp the file
DATE_SUFFIX=$(date +%Y-%m-%d)
# run the script and write its output to file
OUTPUT_FILE=$GP_OUTPUT_DATA_FOLDER/data_summary$OPTION_SUFFIX"_"$DATE_SUFFIX.txt
echo "Writing output to $OUTPUT_FILE"
echo "Configured script options $SCRIPT_ARGS" 
Rscript $GP_SCRIPT_FOLDER/data_summary_helpers.r $SCRIPT_ARGS > $OUTPUT_FILE 2>&1

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

echo "GP root set:"
env | grep GP_

# check if Rscript can be run
echo "Checking Rscript version for compatibility"
if !(Rscript --version); then
    echo "R(script) not available in the current environment, exiting."
    exit 1
fi
echo 

# check the script arguments
AUGMENT=false
GEN_IPF=false
GEN_BASELINE=true
EXTRA_ARGS=
for arg in $@ ; do
    if [[ $arg == "--augment" ]]; then 
        AUGMENT=true
    elif [[ $arg == "--help" || $arg == "/?" ]]; then
        echo "This script loads the GP HUMAT input data and processes it, generating various datasets which are written to file."
        echo "The following options are available when running this script:"
        echo -e "\t--help: prints this help message and exits"
        echo -e "\t--augment: toggles on the augmentation of the dataset using the national energy label dataset (if available)"
        echo -e "\t--impute-na: runs a post-processing script that generates an additional dataset that imputes NA values of interest"
        echo -e "\t--generate-ipf: runs the script that generates an IPF population based on the basis dataset"
        echo -e "\t--skip-baseline: skips over the (re)generation of the baseline population"
        exit 0
    elif [[ $arg == "--generate-ipf" ]]; then
        GEN_IPF=true
    elif [[ $arg == "--skip-baseline" ]]; then
        GEN_BASELINE=false
    else
        # unaccounted arguments are passed on to the script by default
        EXTRA_ARGS="$EXTRA_ARGS $arg"
    fi
done

# set the arguments for the r script
SCRIPT_ARGS="--args --no-augment"


if [[ $GEN_BASELINE == "true" ]]; then
    if [[ $AUGMENT == "false" ]]; then
        echo "Running without augmentation by default, pass --augment to this script in order to perform augmentation step"
    else
        echo "Running with augmentation explicitly enabled"
        SCRIPT_ARGS=""
    fi
    echo Rscript $GP_SCRIPT_FOLDER/data_collection.r $SCRIPT_ARGS $EXTRA_ARGS
    if (Rscript $GP_SCRIPT_FOLDER/data_collection.r $SCRIPT_ARGS $EXTRA_ARGS); then
        echo Finished data collection
        echo 
    else
        echo "Failed to perform data collection"
        exit 1
    fi
else
    echo Skipping baseline population generation
fi

# optionally generate the IPF population
if [[ $GEN_IPF == "true" ]]; then
    echo Generating IPF population
    echo Rscript $GP_SCRIPT_FOLDER/ipf_generation.r $EXTRA_ARGS
    Rscript $GP_SCRIPT_FOLDER/ipf_generation.r $EXTRA_ARGS
fi

# run the data summary script
echo "Creating summary of data"
${GP_SCRIPT_FOLDER}/data_summary.sh

echo Done
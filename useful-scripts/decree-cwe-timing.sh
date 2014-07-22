#!/bin/bash

# Script for running fuzzball against a directory full of examples and
# collecting timing data on the decree cwe examples.  Number of paths
# to explore by default is 1000.  Can be overridden by a command line
# argument (the first command line argument)

fuzzballDir=/home/fuzzbomb/cgc/trunk/code/fuzzball
fuzzball=$fuzzballDir/exec_utils/fuzzball
logdir=$fuzzballDir/logs
targetDir=/home/fuzzbomb/cgc/trunk/code/vulns/decree-cwe/
currentDir=`pwd`
num_paths=${1:-1000}

args="-decree \
   -symbolic-receive -skip-timeouts \
   -x87-emulator $fuzzballDir/extras/x87-emu/math-emu.so \
   -check-for-null -finish-on-null-deref \
   -check-for-jump-to 42 -finish-on-controlled-jump \
   -zero-memory -max-receives 5 \
   -trace-assigns -solve-final-pc \
   -trace-iterations -trace-stopping -trace-syscalls \
   -num-paths $num_paths \
   -iteration-limit 1000"

echo $args

if [ ! -e $logdir ]
then
    echo "Making logging directory $logdir"
    mkdir -p $logdir
fi

if [ ! -d $logdir ]
then
    echo "$logdir exists but is not a directory. Aborting."
    exit 1
fi

function getLogString() {
    logstring="--trace BDT EXP $logdir/BDT_EXP_$1"
    logstring=$logstring" --trace Fuzzball Timing $logdir/Timing_$1"
    echo $logstring
}

for dir in `ls $targetDir`;
do
    echo "Checking $dir"
    if [ ! -d "$targetDir/$dir" ]
    then
	echo "$dir is not a directory, skipping..."
	continue
    else 
	if [ ! -d "$targetDir/$dir/bin" ]
	then
	    echo "$dir doesn't appear to have been built. Skipping ..."
	    continue
	fi
    fi
    echo "Running on $dir$"

    execFile=`echo $dir | sed 's/\(.*\)-/\1_/'`
    exec="$targetDir/$dir/bin/$execFile"
    echo -e "examining binary $execFile"
    
    logsuffix=$dir
    logstring=$(getLogString $logsuffix)
		
    $fuzzball $args $logstring $exec -- $exec
    fuzzballPID=$!
    echo -e "\t\tfuzzball started ($fuzzballPID)\n"
    
    echo -e "\t\twaiting for fuzzball to terminate ($fuzzballPID)"
    wait $fuzzballPID
    
    echo -e "\tdone"
done

## Clean up the temp directories

rm -r fuzball-tmp-*

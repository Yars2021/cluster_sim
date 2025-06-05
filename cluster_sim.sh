#!/bin/bash

# Variables
DATA_DIR=./cluster/data
OUTPUT_DIR=./output

if [ $# = 0 ]; then
  echo "No arguments supplied. Use help argument to get more information"
elif [[ $1 = "help" ]]; then
  echo "======================================================"
  echo "|       Welcome to Cluster ping simulator v1.0       |"
  echo "======================================================"
  echo "Required packages: erlang-devel, python3, graphviz"
  echo "cluster_sim help"
  echo "cluster_sim clean"
  echo "cluster_sim
    <list of strategies>
    <num of nodes>
    <max out edges of each node>
    <max num of broken nodes>
    <node timeout in ms>
    <packet TTL in hops>"
  echo "List of strategies example: \"[strat1,strat2,...]\""
  echo "Supported strategies:
    {single_cast}
    {multicast, N}
    {broadcast}
    {gossip}"
elif [[ $1 = "clean" ]]; then
  rm -rf ${OUTPUT_DIR} 2> /dev/null
  rm -rf ${DATA_DIR} 2> /dev/null
  rm -rf ./cluster/*.beam
  rm -rf ./cluster/*.dump
elif [ $# -ge 6 ]; then
  # Re-Create directories
  rmdir ${OUTPUT_DIR} 2> /dev/null
  rmdir ${DATA_DIR} 2> /dev/null

  mkdir ${OUTPUT_DIR} 2> /dev/null
  mkdir ${DATA_DIR} 2> /dev/null

  # Compile and run Erlang files
  cd ./cluster/
  erlc ./worker.erl ./monitor.erl
  erl -noshell -run monitor shell_autorun $1 $2 $3 $4 $5 $6 -s init stop > /dev/null 2>&1
  cd ../

  # Generate png from .dot file
  circo -T svg ${DATA_DIR}/cluster.dot -o ${OUTPUT_DIR}/cluster.svg

  # Create statistics html page
  python3 ./data_processor/simulation_analyzer.py ${DATA_DIR} ${OUTPUT_DIR} $2 $6

  echo "Simulation done, results can be found here: " `pwd`/${OUTPUT_DIR}
fi

#!/bin/bash

echo "start"

echo "Formatting apps..."
rebar3 format --files 'apps/**/*.erl'

echo "Formatting tests..."
rebar3 format --files 'test/**/*.erl'

echo "done"
#!/bin/bash
set -e

psql -U postgres -d postgres -c "CREATE DATABASE stc OWNER kkirsch"

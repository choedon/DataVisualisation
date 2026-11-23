#!/usr/bin/env bash
# Quick launcher for the Food Product Validation Shiny Dashboard
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR"

echo "=========================================================="
echo "🍏 Launching Food Product Validation Dashboard..."
echo "Open your browser at the URL shown below (e.g. http://127.0.0.1:8080)"
echo "Press Ctrl+C to stop the dashboard server."
echo "=========================================================="

R -e "shiny::runApp('.', port=8080, launch.browser=TRUE)"

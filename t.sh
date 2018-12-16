#!/bin/bash
echo ' CM.make "sources.cm";
Tester.testNumEdges();
Tester.testNumVertices();
Tester.testOutNeighbors();
Tester.testReport();
' | sml

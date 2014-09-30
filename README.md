dependency-graph-similarity-measure
===================================

This package provides a framework for calculating similarity between a pair of dependency parses according to *path overlap*. A very simple example can be run using SolverExample.scala.

Similarity as Path Overlap
--------------------------

Paths may be **Node Paths** which are bookended by a pair of vertices, or **Edge Paths** which are bookended by a pair of edges. For a pair of dependency graphs *d1* and *d2*, the similarity between the two graphs is calculated by generating the highest scoring **composite graph** in which:

    * Each vertex contains a Node Path from *d1* and a Node Path from *d2*.
    * Each edge contains an Edge Path from *d1* and an Edge Path from *d2*.

Edge paths may contain >=1 edge. Node paths may contain >=1 nodes. The composite graph is scored according to *path match* and *path deletion* features. Node paths from either *d1* or *d2* can be deleted at a cost.

Scoring Composite Graphs
--------------------------

Composite graphs are scored using:
    
    * Lexical similarity of the two Node Paths in a composite vertex; 
    * Deletion penalties for Words represented by nodes contained in the edge paths in a composite edge;
    * Deletion penalties for Node paths that have been deleted from *d1* or *d2*.
    
Feature templates and features are in:

    src / main / scala / org / allenai / ari / solvers / graphmatch / models / 



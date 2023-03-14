# Developer Guide

The developer guide is intended to be for people who come in more close contact with HOPR, i.e., code developers and performance
analysts as well as people who are tasked with working or extending the documentation of HOPR.

```{toctree}
---
maxdepth: 1
caption: Table of Contents
numbered:
---
git_workflow.md
styleguide.md
building_guide.md
compiler.md
examples.md
```

This guide is organized to guide the first implementation steps as well as provide a complete overview of 
the simulation code's features from a developer's point of view.

* The first Chapter {ref}`developerguide/git_workflow:GitHub Workflow` shall give an overview over the development workflow within
  the GitHub environment, and the necessary steps to create a release and deploy updates to GitHub.
* The second Chapter {ref}`developerguide/styleguide:Style Guide` describes the rules and guidelines regarding code development 
  such as how the header of functions and subroutines look like.
* Chapter {ref}`developerguide/building_guide:Building the Documentation` describes how to build the html and pdf files
  locally before committing changes to the repository.
* Chapter {ref}`developerguide/compiler:Compiler Options` gives an overview of compiler options that are used in PICLas and their
  purpose.
* Chapter {ref}`developerguide/examples:Markdown Examples` gives a short overview of how to include code, equations, figures, tables
  etc. in the user and developer guides in Markdown.

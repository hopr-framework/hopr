# Markdown Examples
## hyperlinks
[one with a title](http://fsf.org "click here for a good time!"). Unclear how to enforce new window.

## Code environment
Either use fenced style (tildes) 

~~~~~~~
if (a > 3) {
  moveShip(5 * gravity, DOWN);
}
~~~~~~~

or indented style (4 whitespaces)

    if (a > 3) {
      moveShip(5 * gravity, DOWN);
    }

Both works with pandoc and wordpress. Also see [pandoc verbatim code](http://pandoc.org/README.html#verbatim-code-blocks "pandoc verbatim code").

## Equations
(@gleichung1) $$a=b*c$$
As (@gleichung1) shows, blabla.

## Bibtex, cite
Hindenlang [@Hindenlang2015]. Only works with pandoc!

[bibshow file=references.bib]

Hindenlang [bibcite key=Hindenlang2015], Gassner [bibcite key=gassner2011disp]


## section references
## Figures, caption
![This is the caption\label{mylabel}](https://github.com/hopr-framework/hopr/blob/master/docs/doxygen/HOPR.png?raw=true)

See figure \ref{mylabel}.


```{figure} figures/HOPR.png
---
name: fig:example
width: 200px
align: center
---

This is an example caption.
```
See {numref}`fig:example`.

## tables
## unnumbered section headings
  just add 

    {-}

 after the heading

## Code blocks for various languages

```{code-block} C

int a = 32;
int a = 32;

```

# voronoi

Generate a voronoi diagram from a set of random points.

To run:

1. 'cabal configure'
2. 'cabal build'
3. './dist/build/voronoi/voronoi [i|N]'

The command-line option specifies which norm to use; the l<sub>2</sub> (Euclidian) norm is the default. N can be any (whole or fractional) number, or use "i" for l<sub>∞</sub>.

# Screenshots

Using Euclidian distance (l<sub>2</sub> norm):

![voronoi-l2](screenshots/voronoi2.png)  


Using Manhattan distance (l<sub>1</sub> norm):

![voronoi-l1](screenshots/voronoi1.png)  


Using l<sub>∞</sub> norm:

![voronoi-li](screenshots/voronoii.png)  


Using l<sub>1.2</sub> norm (uhh, pseudo-Manhattan?):

![voronoi-l1.2](screenshots/voronoi1.2.png)  

Positive edges- Edges which are part of the cycle.
Positive cell - Cell which is not a blank/space.

Represtations used while storing list:
	+	=> grid-node
	- |	=> Positive-edges
	#	=> empty cell
	*	=> empty edge (non-filled edge)



Cell Locations for (3*3) :
	+ + + +			0123456
	      		       	0123456 
	+ + + +			0123456
	      			0123456
	+ + + +			0123456
	       			0123456
	+ + + +			0123456

	Numbering starts at 0
	triple : 11U =>  2; 11L =>  8; 11R => 10; 11D => 16
		 21U => 16; 21L => 23; 21R => 25; 21D => 32
	
	nr : cell-row; nc: cell-col; NC: Total number of columns	
	Cell number formulae: (2nr -1) * (2NC + 1) + 2nc
		

- Game-Play / Algo :
----------------
	1.First check if the entered move is valid or not.
		Valid: 	Each grid-node can have 0/2 edges.
			Not redrawing the same input (Need to flip the edge selection-phase-2)
		
	2.If valid: 
	

- Game over check: 
------------------
	1-All cells have given number of adjacent edges
	2-No grid-node have open edge (only 1 edge, it should have either 0/2 edges)
	3-Form only 1 cylce

	1.Each positive cell is stored in a hashmap with location as key and its number as value. Same cell location is inserted into a list(Incomplete cell list (ICL)).
	  Whenever user enters a new triple we decrease that cell value by 1. When value becomes 0 we remove that cell from the ICL. When value becomes <0 or >0 we check for that cell in list and add if not present.  When ICL is empty it means cells-edges-constraint is satisfied. Proceed to check (2).

	2.Maintain a hashmap for all the grid-nodes and no.of edges as value. Maintain all the grid-nodes with only 1 edge in a list. When this list is empty then all grid-nodes have either 0/2 edges. Proceed to check (3).

	3.To check if only 1 cycle after game-over:	
		- Store all positive edges while user entering. 
		- Select any one edge among them and start navigating till you reach same edge. Then count the edges in this navigated cycle and total positive edges. If both are same then game-over.

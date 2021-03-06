URL analyzer wirtten in R for link data generated by web crawler and converter program.

Requires rglPlotCustom package.

Features:

Can accept either a converted crawled .csv file or an occurrence .csv file.

If using an occurrence file (for duplicates or tocrawl) an interactive scatterplot is generated showing each links' occurrences in comparison to each other link. Mousing over each point will display that points' information.

If using a crawled file, a 3D tree is generated showing each link in relation to each other link. Multiple options are available for this file type.

	*Display Vertex Labels - This option to set whether to display the link name on each vertex. This is for analysis purposes and is very slow on computers with no dedicated GPU.
	*Vertex Font Scaling - When showing vertex labels, this will scale the text based on its original size. (1 = standard, 0.5 = half size, 2 = double size). **This is the feature that requires the rglPlotCustom package.
	*Export as PNG - Export the current 3D model as a 2D PNG file (saved to the R folder within the project.
	*Export as STL - Export the current 3D model as a STL file for use in a 3D modeling program such as Blender. This allows the models to be edited or scaled for later export to be sent to a 3D printer. This feature is slow to export so do not close the render window or the application for a few minutes after clicking the button.
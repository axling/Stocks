-record(google_chart, 
	{type=line, title="", color="909090", font_size=10, 
	 width=420, height=200, axes=[], data, date_alignment,
	 grid_x=undefined, grid_y=undefined, grid_line_length=1, 
	 grid_blank_length=5, background_color=ffffff, chart_color=ffffff, 
	 legend_location=bottom, bar_space=3, bar_group_space=7}).

-record(chart_axis, {position, labels, color=909090, font_size=10}).
-record(chart_data, {color=909090, legend, values, min_value=0, max_value=100, 
		     line_width=1, line_length=1, blank_length=0}).

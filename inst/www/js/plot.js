$(document).ready(function(){
	var dropDownSelect ='';

	$("#varProfileOptions").on('click','a',function(){
		dropDownSelect = $(this).text();
		$('#varDropdownMenuButton').html(dropDownSelect);

		//Call the R script to update the plot
		plotProfilingGraph(dropDownSelect);
		dropDownSelect = '';
	});
});

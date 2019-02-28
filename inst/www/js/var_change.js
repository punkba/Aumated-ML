$(document).ready(function(){
$('#varChangeLnk').hide();
//Get the variable list after uploading the data



$('#varChangeBtn').on('click',function(){
	//Disable the button for the user
	$('#varChangeBtn').prop("disabled",true);

	var input;

	if (checkedVars.length > 0)
	{
		input = checkedVars;
	}
	else{
		input = 0;
	}
	console.log("Input is: "+input);

	var preProcessReq = ocpu.call('preprocessing',
								 {'conv_var_names':input,
							 	  'dv' : dvname
							  	 },
								 function(session){
								  	session.getObject(function(returnCode){
										if(returnCode == 0)
										{
											initiatePreProcess();
											$('#varChangeLnk').attr('href',session.getFileURL('LogFile.txt'));
											$('#varChangeLnk').show();
										}

									});
								}).fail(function(){
									alert('Server error: '+preProcessReq.responseText);
								}).always(function(){
								});
	//Get the list of variable names after updation by user for passing it to R
	$("#profiling-tab").removeClass('disabled');
	$("#modelling-tab").removeClass('disabled');
});
});

function initiatePreProcess(){

	var reqVarImp = ocpu.call('top_var_graph',{'target.var.name':dvname,'ds': ds},function(session){
		loc = session.getLoc();
		varImpPreImgLoc = loc+'graphics/last'
		$('#plotdiv').attr('src',varImpPreImgLoc);
	}).fail(function()
	{
		alert("Server error: " + reqVarImp.responseText);
	}).always(function(){

	});

	var reqVarList = ocpu.call('imp_var_list',{'target.var.name':dvname},function(session){
		session.getObject(function(output){
		tempOut1 = output;
		/* Plot the variable profile by default for the first variable  in
		the dropdown*/
		plotProfilingGraph(output[0]);
		$('#varDropdownMenuButton').html(output[0]);
		populateDropList(output);
		}).fail(function()
		{
			alert("Server error: " + reqVarImp.responseText);
		}).always(function(){
			console.log('populateDropList');
		})
	});

}

var tempOut1='';

function populateDropList(dataInput){
	for (var i=0; i < dataInput.length;++i)
	{
		$("#varProfileOptions").append("<a class='dropdown-item' href='#'>"+dataInput[i]+"</a>");
	}
}

function plotProfilingGraph(variableName){

	$('#plotdiv1').attr('src','#');

	var reqProfileInitGraph = ocpu.call('variable_profiling_function',{'dv':dvname,'vars':variableName},function(session){
		loc = session.getLoc();
		varImpPreImgLoc = loc+'graphics/last'
		$('#plotdiv1').attr('src',varImpPreImgLoc);
	}).fail(function()
	{
		alert("Server error: " + reqVarImp.responseText);
	}).always(function(){

	});
}

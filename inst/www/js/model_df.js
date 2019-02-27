output='';
sessionS='';
$(document).ready(function(){

	$("#modelDownloadLink").hide()
	$("#building_inter").hide();

	var model_persist = "";
	var varImpData = "";
    var modelLink = "";
	var modelSummaryPath="";

	isChecked=$('input[name=customRadio]:checked', '#modelsList').val();

	$('#modelsList input').on('change', function() {
		isChecked = $('input[name=customRadio]:checked', '#modelsList').val();
	});

	$("#modelSubmit").on("click", function(){

		$("#modelSubmit").attr("disabled", "disabled");

		//Check which model is selected
		var dvname=$("#dvname").val();
		var preddv=$("#preddv").val();

		$('#building_inter').show();
    	$("#building_inter").text("Training the Model... Will be ready in a jiffy!");

		var modelReq = ocpu.call("modelling_module",
								{
									"model_selection" : isChecked,
									"predictorClass" : preddv,
									"dv" : dvname
    							},
								function(session)
								{
									sessionS = session;
									getResultChartsAndDisplay(session);
									session.getObject(function(dataOutput){
										$("#building_inter").text('Model Trained !! Check next page for results');
										console.log(dataOutput);
										populateResults(dataOutput);
										$("#modelDownloadLink").attr('href',session.getFileURL(dataOutput[1]['modelSaveLocation'].toString()));
										$("#modelDownloadLink").show();
									}).fail(function(){

									});
								}).fail(function(){
									alert("Server error: " + modelReq.responseText);
								}).always(function(){
									$("#show_perf").removeAttr("disabled")
								});

		function populateResults(sessionData){
			populateConfusionMatrix(sessionData[2]['metricOutput'].flat());
		}

		function getResultChartsAndDisplay(session){
			var base_url = session.getLoc();
			var liftUrl = base_url +'graphics/2';
			$("#liftChart").attr('src',liftUrl);
			var varImpUrl = base_url +'graphics/3';
			$("#varImpChart").attr('src',varImpUrl);
		}

		function populateConfusionMatrix(ConfuseData){
			$('#TP').html((ConfuseData[0]*100).toPrecision(4));
			$('#FP').html((ConfuseData[1]*100).toPrecision(4));
			$('#TN').html((ConfuseData[2]*100).toPrecision(4));
			$('#FN').html((ConfuseData[3]*100).toPrecision(4));
		}
  	});
});

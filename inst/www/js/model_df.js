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
									"dv" : dvname,
									"prevSessionid" : prevSessionId
    							},
								function(session)
								{
									prevSessionId = session.getKey();
									getResultChartsAndDisplay(session);
									session.getObject(function(dataOutput){
										$("#building_inter").text('Model Trained !! Check next page for results');
										console.log(dataOutput);
										populateResults(dataOutput);
										$("#modelDownloadLink").attr('href',session.getFileURL(dataOutput[0][1]['modelSaveLocation'].toString()));
										$("#modelDownloadLink").show();
										$("#modelLogFileLinkActual").attr('href',session.getFileURL('ModelLogFile.csv'));
										$("#modelLogFileLinkBench").attr('href',session.getFileURL('ModelLogFile_Bench.csv'));

										$("#results-tab").removeClass('disabled');
										$("#scoring-tab").removeClass('disabled');
									}).fail(function(){

									});
								}).fail(function(){
									alert("Server error: " + modelReq.responseText);
								}).always(function(){
									$("#modelSubmit").removeAttr("disabled")
								});

		function populateResults(sessionData){
			populateConfusionMatrix(sessionData[0][2]['metricOutput'],'test');
			populateConfusionMatrix(sessionData[1][2]['metricOutput'],'train');
			$('#testResultConf').addClass('show');
			if(sessionData[0][4][0] == 'N'){
				$("#list-importantVariables-list").remove();
			}
		}

		function getResultChartsAndDisplay(session){
			var base_url = session.getLoc();
			var gainUrl = base_url +'graphics/2/png?width=700&height=400';
			$("#gainChart").attr('src',gainUrl);
			var varImpUrl = base_url +'graphics/3/png?width=700&height=400';
			$("#varImpChart").attr('src',varImpUrl);
		}

		function populateConfusionMatrix(ConfuseData,place){
			$('#'+place+'TP').html(((ConfuseData['tpr'].flat())*100).toPrecision(4));
			$('#'+place+'FP').html(((ConfuseData['fpr'].flat())*100).toPrecision(4));
			$('#'+place+'TN').html(((ConfuseData['tnr'].flat())*100).toPrecision(4));
			$('#'+place+'FN').html(((ConfuseData['fnr'].flat())*100).toPrecision(4));
			$('#'+place+'-f1-Act').html(ConfuseData['f1score'].flat());
			$('#'+place+'-pre-Act').html(ConfuseData['precision'].flat());
			$('#'+place+'-rec-Act').html(ConfuseData['recall'].flat());
			$('#'+place+'-acc-Act').html(ConfuseData['accuracy'].flat());
		}
  	});
});

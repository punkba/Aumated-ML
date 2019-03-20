$(document).ready(function(){

	var categorical = '';
	var continuous = '';
	var discrete = '';
	prevSessionId = "";

	var testOutput='';
	$("#progress1").hide();

	$("#fileInputArea").change(function(){

		filename = $("#fileInputArea")[0].files[0];
		$("#submitbutton").attr("disabled", "disabled");

		var reqInit = ocpu.call("identifyDVColumns", {
			inputFile : filename,
	    },function(session){
			session.getObject(function(varnameList){
				console.log(varnameList);
				addSelectValues(varnameList);
			}).fail(
			function(){
				alert("Server error: " + reqInit.responseText);
			});
	    });

		reqInit.fail(function(){
	      alert("Server error: " + reqInit.responseText);
	    });

		reqInit.always(function(){
	      $("#submitbutton").removeAttr("disabled")
	    });
	});

	$("#submitbutton").on("click", function(){
		$("#fileInputArea,#dvname,#preddv,#data-split").removeClass('is-invalid');

		dvname=$("#dvname").val();
		preddv=$("#preddv").val();
		ds="";

		if($("#data-split").val() == ""){
			ds = $('#data-split').attr('placeholder');
		}
		else{
			ds=$("#data-split").val();
		}

		if(!filename){
			alert('Enter FileName');
			return;
		}

		if(!dvname){
			alert('Enter DV');
		  	return;
		}

		if(!preddv){
			alert('Enter Class');
		  return;
		}

		if(!ds){
			alert('Enter Split Ratio');
		  	return;
		}

		uploadcsv(filename);
	  });
});

function uploadcsv(filename){
    $("#submitbutton").attr("disabled", "disabled");

	$("#progress1").text("Reading the File");
	$("#progress1").show();

    var req = ocpu.call("read_csv", {
		file : filename,
    },function(session){
		/*$("#status1").text("Got the file !!");*/
		initiatePrelimAnalysis(session);
    	$("#progress1").text("Read Successful");
    });

    //if R returns an error, alert the error message
    req.fail(function(){
      alert("Server error: " + req.responseText);
    });

    //after request complete, re-enable the button
    req.always(function(){
      $("#submitbutton").removeAttr("disabled")
    });
}

function initiatePrelimAnalysis(sessionData){

	var req = ocpu.call('exploreDf',
						{
							df_full: sessionData,
							dv		: dvname
						},
						function(session){
							/*$("#status1").text("Analysing the file !!");*/
							prevSessionId = session.getKey();
							session.getObject(function(full_output){
								getAndDisplayVariables(full_output);
							}).fail(
							function(){
								alert("Server error: " + req.responseText);
							});
						/*$("#status1").text("Go to next page for variable information ..");
						$("#status1").removeClass("lds-dual-ring");
						$("#chooseCSV").removeClass("section--is-active");
						$("#csvNav").removeClass("is-active");
						$("#varManip").addClass("section--is-active");
						$("#varManNav").addClass("is-active");*/
						});
}

function getAndDisplayVariables(listInput){
	testOutput = listInput;
	discrete = listInput[0]['discrete'];
	categorical = listInput[1]['categorical'];
	continuous = listInput[2]['continuous'];

	//create the lsit elements to display continuous variables
	for(elem = 0;elem < categorical.length;elem++){
		createListElem(categorical[elem],'#cate-list');
	}

	//create the lsit elements to display continuous variables
	for(elem = 0;elem < continuous.length;elem++){
		createListElem(continuous[elem],'#conte-list');
	}

	//create checkbox to convert discrete to categorical
	for(elem = 0;elem < discrete.length;elem++){
		createCheckBox(discrete[elem],elem);
	}

	$("#manipulation-tab").removeClass('disabled');
	$("#uploadcsv-tab").removeClass('active');
	$("#uploadcsv").removeClass('active show');
	$("#manipulation-tab").addClass('active');
	$("#manipulation").addClass('active show');
}

function createListElem(value,locToCreate){
	var node = document.createElement("LI");
	var textnode = document.createTextNode(value);
	node.appendChild(textnode);
	$(locToCreate).append(node);
}

function createCheckBox(value,elem){
	var chk = document.createElement('input');  // CREATE CHECK BOX.
    chk.setAttribute('type', 'checkbox');       // SPECIFY THE TYPE OF ELEMENT.
    chk.setAttribute('id', 'Variable'+elem);
	chk.setAttribute('class','form-check-input');
    chk.setAttribute('value', value);
    chk.setAttribute('name', 'variables');
	chk.setAttribute('onclick','updateCheckList('+'"Variable'+elem+'")');

	var lbl = document.createElement('label');  // CREATE LABEL.
    lbl.setAttribute('for', 'Variable'+elem);
	lbl.setAttribute('class','form-check-label');

    // CREATE A TEXT NODE AND APPEND IT TO THE LABEL.
    lbl.appendChild(document.createTextNode(value));

	var divElement = document.createElement('div');
	divElement.setAttribute('class','form-group');
	divElement.appendChild(chk);
	divElement.appendChild(lbl);

	discreteBox.appendChild(divElement);

	value = '';
}

var checkedVars = new Array();

function updateCheckList(variableName){

	var variable = document.getElementById(variableName);

	if(variable.checked)
	{
		checkedVars.push(variable.value);
	}
	else{
		var index = checkedVars.indexOf(variable.value);
		if(index !== -1)
		{
			checkedVars.splice(index,1);
		}
	}
}

function addSelectValues(inputList){
	for(elem = 0;elem < inputList.length;elem++){
		var option = new Option(inputList[elem],inputList[elem]);
		$("#dvname").append($(option))
	}
	$('#dvname option[value=""]').remove();
}

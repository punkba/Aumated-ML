$(document).ready(function(){

	var categorical = '';
	var continuous = '';
	var discrete = '';

	var testOutput='';


	$("#submitbutton").on("click", function(){
		$("#fileInputArea,#dvname,#preddv,#data-split").removeClass('is-invalid');

		filename = $("#fileInputArea")[0].files[0];
		dvname=$("#dvname").val();
		preddv=$("#preddv").val();
		ds=$("#data-split").val();

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

		/*$("#status1").text("Reading the CSV...")
		$("#status1").addClass("lds-dual-ring");*/

		uploadcsv(filename);
	  });
});

function uploadcsv(filename){
    $("#submitbutton").attr("disabled", "disabled");
    var req = ocpu.call("read_csv", {
		file : filename,
    },function(session){
		/*$("#status1").text("Got the file !!");*/
		initiatePrelimAnalysis(session);
        alert('Read Successful');
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
							globalSession = session;
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
	$("#uploadcsv").removeClass('active');
	$("#manipulation-tab").addClass('active');
	$("#manipulation").addClass('active');
}

function createListElem(value,locToCreate){
	var node = document.createElement("LI");
	var textnode = document.createTextNode(value);
	node.appendChild(textnode);
	console.log(locToCreate);
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

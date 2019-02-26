$(document).ready(function(){

	var categorical = '';
	var continuous = '';
	var discrete = '';

	var testOutput='';

	$("#submitbutton").on("click", function(){
		filename = $("#fileInputArea")[0].files[0];
		dvname=$("#dvname").val();
		preddv=$("#preddv").val();
		ds=$("#data-split").val();

		if(!filename){
			alert("No file selected.");
			return;
		}

		if(!dvname){
		  alert("Please provide DV name.");
		  return;
		}

		if(!preddv){
		  alert("Please provide DV class name.");
		  return;
		}

		if(!ds){
		  alert("Please provide split ratio name.");
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
		/*$("#status1").text("Got the file !!");
		initiatePrelimAnalysis(session);*/
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

	discreteBox.appendChild(chk);
	discreteBox.appendChild(lbl);

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
  function printsummary(mydata){
	var req = ocpu.call("printsummary",
						{"df_full" : mydata,
						"dvname" : dvname,
						"preddv" : preddv
						},
						function(session){
							session.getObject(function(output){
							$("#status1").text("Cleaning Successful! Setting up the results");

		//FUNCTION CALLS TO SIGNIFICANT VARIABLE LIST & GRAPHS PLOTS
	  plot_graph();
      	  add_var_list();
    }).fail(function(){
      alert("Server error: " + req.responseText);
    });
  });
  }
  /*

		//Adding code for var_list call

		function add_var_list()
			{
				//alert("inside Add Options");
				var vars;
				var req = ocpu.call("imp_var_list", {	"target.var.name" : dvname},
			function(session){
				session.getObject(function(data){
				//$("#output code").text(data);
				//alert("imp_var_list ends: trying to append" );

				for (var i=0; i < data.length;++i)
				{
					//alert("inside_for " + i);
					var x = document.getElementById("DropList");
					var option = document.createElement("option");
					option.text = data[i];

					x.add(option);

					//alert("option " + i + " added")

				}
					alert("All set!")
					$("#status1").text("Hurray! Go on and look at the results now...");
				document.getElementById('DropList').onchange = function () {
					var x = document.getElementById("DropList").selectedIndex;
					var y = document.getElementById("DropList").options;

						plot_graph_variable(y[x].text);

				};
				//plot_graph_variable()

				}).fail(function(){
					alert("R returned an error in var_list: " + req.responseText);
					});
				});

				}


		//VAR LIST CALL ENDS

		//Adding Code for Significant variables graph
				function plot_graph()
			{
				//alert("inside Plot graph");

				//var req = $("#plotdiv1").rplot("randomplot", {	nfield : 100, distfield : "normal"})

				var req = $("#plotdiv1").rplot("top_var_graph", {"target.var.name" : dvname, "ds" : ds });

				//if R returns an error, alert the error message
				req.fail(function(){
				alert("Server error: " + req.responseText);
				});

				//after request complete, re-enable the button
				req.always(function(){
				$("#submitbutton").removeAttr("disabled")
				});
				//alert("plotted");
			}


			//SIGNIFICANT GRAPH CALL ENDS

		function plot_graph_variable(tar)
			{
				//alert("inside variable Plot graph");

				//var req = $("#plotdiv").rplot("randomplot", {	nfield : 100, distfield : "normal" , title : data})

				var req = $("#plotdiv").rplot("variable_profiling_function", {dv : dvname, var: tar});

				//if R returns an error, alert the error message
				req.fail(function(){
				alert("Server error: " + req.responseText);
				});

				//after request complete, re-enable the button
				req.always(function(){
				$("#submitbutton").removeAttr("disabled")
				});
				//alert("plotted");

			}

  */

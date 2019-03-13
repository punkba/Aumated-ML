$(document).ready(function(){

    $('#scoreDownloadLink').hide();
    $('#progress3').hide();

    $('#submitbutton2').on('click',function(){
        scoreFileName =  $("#fileInputAreaScore")[0].files[0];;

        if(!scoreFileName){
            alert("No file selected.");
            return;
        }
        uploadcsv2(scoreFileName);
    });

    function uploadcsv2(inputFileName){

        $("#submitbutton2").attr("disabled", "disabled");
        $('#progress3').show();
        $('#progress3').text('Scoring in Progress!!');
        var req = ocpu.call("scoringmodule", {
            filename : inputFileName,
            modelSel:isChecked,
            "prevSessionid" : prevSessionId
        }, function(session){
            $('#scoreDownloadLink').attr('href',session.getFileURL('scoredData.csv'));
            $('#progress3').text('Scoring Completed').delay(3000).hide();
            $('#scoreDownloadLink').show();
        }).fail(function(){
          alert("Server error: " + req.responseText);
        }).always(function(){
          $("#submitbutton").removeAttr("disabled");
        });
    }
});

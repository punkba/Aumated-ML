$(document).ready(function(){

    console.log('In scoring');
    $('#scoreDownloadLink').hide();

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

        var req = ocpu.call("scoringmodule", {
            filename : inputFileName
        }, function(session){
            $('#scoreDownloadLink').attr('href',session.getFileURL('scoredData.csv'));
            $('#scoreDownloadLink').show();
        }).fail(function(){
          alert("Server error: " + req.responseText);
        }).always(function(){
          $("#submitbutton").removeAttr("disabled");
        });
    }
});

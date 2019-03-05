$(document).ready(function(){

    $('.custom-file-input').on('change',function(){
        var fileName = $(this).val().split('\\').pop();
        $(this).next('.custom-file-label').addClass("selected").html(fileName);
    });

    feather.replace();

    $(function () {
        $('[data-toggle="popover"]').popover()
    });

    jQuery('#trainRB,#testRB').click( function(e) {
        jQuery('.collapse').collapse('hide');
    });
})

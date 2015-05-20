
setInterval(function(){

  if ($('html').attr('class')=='shiny-busy' ) {
     
    $('div.busy').show()
  } 
 else if($('html').attr('class')==null )
{
$('div.busy2').show()
}
else {
    $('div.busy').hide()
$('div.busy2').hide()
  
  }
},100)
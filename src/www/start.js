$(document).ready(function(){
  // Ocultar campo de quantidades
  $('.shiny-input-container').eq(3).hide();

  // Exibir filtro por quantidade de itens do campo informação
  $("#informacoes").change(function() {
    if ($("#informacoes option:selected").text() != "Tipos de acidentes"){
      $('.shiny-input-container').eq(3).show('fast');
    }else{
      $('.shiny-input-container').eq(3).hide('fast');
    }
  });
});

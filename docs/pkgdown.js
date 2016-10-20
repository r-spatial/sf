$(function() {

  $('#sidebar').affix({
    offset: {
      top: $('#sidebar').offset().top - 40
    }
  });
  $('body').scrollspy({
    target: '#sidebar',
    offset: 40
  });

});

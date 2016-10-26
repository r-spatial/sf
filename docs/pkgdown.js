$(function() {

  $('#sidebar').affix({
    offset: {
      top: $('#sidebar').offset().top - 60
    }
  });
  $('body').scrollspy({
    target: '#sidebar',
    offset: 60
  });

});

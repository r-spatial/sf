$(function() {

  $('#tocnav').affix({
    offset: {
      top: $('#tocnav').offset().top - 80
    }
  });
  $('body').scrollspy({
    target: '#tocnav',
    offset: 80
  });

});

// taken from
// https://ourcodeworld.com/articles/read/143/how-to-copy-text-to-clipboard-with-javascript-easily
function setClipboardText(text) {
  var id = 'mycustom-clipboard-textarea-hidden-id';
  var existsTextarea = document.getElementById(id);

  if (!existsTextarea) {
    console.log('Creating textarea');
    var textarea = document.createElement('textarea');
    textarea.id = id;
    // Place in top-left corner of screen regardless of scroll position.
    textarea.style.position = 'fixed';
    textarea.style.top = 0;
    textarea.style.left = 0;

    // Ensure it has a small width and height. Setting to 1px / 1em
    // doesn't work as this gives a negative w/h on some browsers.
    textarea.style.width = '1px';
    textarea.style.height = '1px';

    // We don't need padding, reducing the size if it does flash render.
    textarea.style.padding = 0;

    // Clean up any borders.
    textarea.style.border = 'none';
    textarea.style.outline = 'none';
    textarea.style.boxShadow = 'none';

    // Avoid flash of white box if rendered for any reason.
    textarea.style.background = 'transparent';
    document.querySelector('body').appendChild(textarea);
    console.log('The textarea now exists :)');
    existsTextarea = document.getElementById(id);
  } else {
    console.log('The textarea already exists :3');
  }

  existsTextarea.value = text;
  existsTextarea.select();

  try {
    var status = document.execCommand('copy');
    if (!status) {
      console.error('Cannot copy text');
    } else {
      console.log('The text is now on the clipboard');
    }
  } catch (err) {
    console.log('Unable to copy.');
  }
}

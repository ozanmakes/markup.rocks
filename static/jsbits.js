(function () {
  window.fromTextArea = function (el, cb) {
    var cm = CodeMirror.fromTextArea(el, {
      mode: "gfm",
      lineWrapping: true,
      theme: "neo"
    });

    cm.on("change", function (e) {
      cb(cm.getValue());
    });

    setTimeout(function () {
      cm.refresh();
    }, 0);

    setTimeout(function () {
      cm.refresh();
    }, 1000);

    return cm;
  };

  window.insertSpaces = function (cm, enabled) {
    if (enabled) {
      cm.setOption("extraKeys", {
        Tab: function(cm) {
          var spaces = Array(cm.getOption("indentUnit") + 1).join(" ");
          cm.replaceSelection(spaces);
        }
      });
    } else {
      cm.setOption("extraKeys", {});
    }
  };

  window.dropdownOnChange = function (el, cb) {
    $(el).dropdown({ onChange: cb });
  };

  window.highlightCode = function () {
    $('.preview pre code').each(function (i, block) {
      window.hljs.highlightBlock(block);
    });
  };

  window.enableMenu = function (el, action) {
    $(el).dropdown({on: 'hover', action: action});
  };

  window.dropboxOpen =  function (el, cb) {
    $(el).click(function () {
      Dropbox.choose({
        success: function (files) { cb(files[0].link); },
        linkType: "direct",
        multiselect: false,
        extensions: [
          ".dbk",
          ".html",
          ".markdown",
          ".md",
          ".mw",
          ".opml",
          ".org",
          ".rst",
          ".t2t",
          ".tex",
          ".textile",
          ".twiki",
          ".txt",
          ".xml"
        ]
      });
    });
  };

  window.fileOpen = function (el, cb) {
    el.addEventListener('change', function (ev) {
      var reader = new FileReader();
      var file = ev.target.files[0]
      reader.onload = function () { cb(file.name, this.result); };
      reader.readAsText(file);
    }, false);
  };

  window.fileSave = function (filename, contents) {
    var blob = new Blob([contents], {type: "text/plain;charset=utf-8"});
    saveAs(blob, filename);
  };

  window.dropboxSave = function (filename, contents) {
    var b64 = window.btoa(unescape(encodeURIComponent(contents)));
    var dataUri = "data:text/plain;charset=UTF-8;base64," + b64;
    Dropbox.save(dataUri, filename);
  };
})();

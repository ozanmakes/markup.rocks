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

    return cm;
  };

  window.checkboxOnChange = function (el, cb) {
    $(el).dropdown({ onChange: cb });
  };

  window.highlightCode = function () {
    $('.preview pre code').each(function (i, block) {
      window.hljs.highlightBlock(block);
    });
  };
})();
